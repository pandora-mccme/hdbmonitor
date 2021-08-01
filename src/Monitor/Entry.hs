{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Monitor.Entry where

import Control.Concurrent.Async

import System.Directory
import System.Exit
import System.FilePath
import System.INotify

import qualified Data.ByteString.Char8 as BSC
import Data.List

import Monitor.Options (Options(..))
import Monitor.Config
import Monitor.Queue
import Monitor.DataModel

updateEventVariety :: [EventVariety]
updateEventVariety = [Modify, Move, MoveIn, MoveOut, Create, Delete, DeleteSelf, MoveSelf]

changeConfigAction :: INotify -> String -> FilePath -> FilePath -> IO ()
changeConfigAction watcher dir tgvar path = if path == configName
  then tryToEnter ConfigWatched watcher dir tgvar
  else pure ()

-- watches only config changes.
configWatch :: INotify -> FilePath -> String -> Event -> IO ()
configWatch watcher dir tgvar (Modified False (Just path)) =
  changeConfigAction watcher dir tgvar $ BSC.unpack path
configWatch watcher dir tgvar (MovedIn False path _) =
  changeConfigAction watcher dir tgvar $ BSC.unpack path
configWatch watcher dir tgvar (Created False path) =
  changeConfigAction watcher dir tgvar $ BSC.unpack path
configWatch _ _ _ _ = pure ()

-- FIXME: triad of parameters may be better to also be wrapped in Reader.
jobAction :: INotify -> FilePath -> String -> JobAction -> Settings -> FilePath -> IO ()
jobAction watcher dir tgvar action cfg path = if notHidden path
  then if path == configName
    then (flip runReaderT cfg . getMonitor $ destroyQueue)
      >> tryToEnter ConfigNonWatched watcher dir tgvar
    else do
      liftIO (print action)
      flip runReaderT cfg . getMonitor $ case action of
        Start -> startJob (dir </> path)
        Restart -> restartJob (dir </> path)
        Remove -> removeJob (dir </> path)
  else pure ()

-- watches check changes.
{-
  Expected behavior:
  On config changes -- drop all jobs, execute tryToEnter. On success inotify process is kept alive.
  On file changes -- actions for each type of event.
  Also note behavior on file renames -- two successive alerts comes, one deletes the job, one starts the same with another id.
  DeleteSelf event must trigger suicide alert and immediate exit.
-}
watchTower :: INotify -> FilePath -> String -> Settings -> Event -> IO ()
watchTower watcher _ _ cfg DeletedSelf = runReaderT (getMonitor $ destroyMonitor watcher) cfg
watchTower watcher _ _ cfg (MovedSelf _) = runReaderT (getMonitor $ destroyMonitor watcher) cfg
watchTower watcher dir tgvar cfg (Modified False (Just path)) =
  jobAction watcher dir tgvar Restart cfg (BSC.unpack path)
watchTower watcher dir tgvar cfg (Deleted False path) =
  jobAction watcher dir tgvar Remove cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (MovedOut False path _) =
  jobAction watcher dir tgvar Remove cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (MovedIn False path _) =
  jobAction watcher dir tgvar Start cfg (BSC.unpack path)
watchTower watcher dir tgvar cfg (Created False path) =
  jobAction watcher dir tgvar Start cfg $ BSC.unpack path
watchTower _ _ _ _ _ = pure ()

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _ = True

enter :: INotify -> FilePath -> [FilePath] -> String -> Settings -> IO ()
enter watcher dir checks tgvar cfg = do
  {-
    After successful start behavior changes: config now must be watched by process taking care about job queue,
    we don't want old settings to be applied so far.
    Hence on successful start we have to close watch descriptor. But we cannot pass it to it's own event handler.
    So we must restart whole inotify.
    First inotify process watches only config, second -- only queue.
    When config breaks, it turns into loop of starting process and dies as soon as queue is successfully restarted,
  -}
  killINotify watcher
  newWatcher@(INotify _ _ _ _ eventsThread) <- initINotify
  void $ addWatch newWatcher updateEventVariety (BSC.pack dir) (void . async . watchTower newWatcher dir tgvar cfg)
  -- In directories there may be any content.
  checkFiles <- filterM doesFileExist (map (dir </>) . filter notHidden $ checks)
  mapConcurrently_ (\f -> runReaderT (getMonitor $ startJob f) cfg) checkFiles
  wait eventsThread

missingConfigCase :: INotify -> FilePath -> String -> IO ()
missingConfigCase watcher dir tgvar = do
  putStrLn $ "Configuration file is missing or invalid in " <> dir <> ", ignoring. You do not need to restart after config fix."
  void $ addWatch watcher [MoveIn, Create, Modify] (BSC.pack dir) (void . async . configWatch watcher dir tgvar)

maybeAddConfigWatch :: INotify -> ConfigWatchFlag -> FilePath -> String -> IO ()
maybeAddConfigWatch watcher isWatched dir tgvar = case isWatched of
  ConfigWatched -> return ()
  ConfigNonWatched -> missingConfigCase watcher dir tgvar

tryToEnter :: ConfigWatchFlag -> INotify -> FilePath -> String -> IO ()
tryToEnter isWatched watcher dir tgvar = do
  (mConfigPath, checks) <- partition (== configName) <$> listDirectory dir
  case mConfigPath of
    [] -> maybeAddConfigWatch watcher isWatched dir tgvar
    (configPath:_) -> do
      !mSettings <- readSettings dir tgvar configPath
      case mSettings of
        Nothing -> maybeAddConfigWatch watcher isWatched dir tgvar
        Just cfg -> enter watcher dir checks tgvar cfg

trackDatabase :: String -> FilePath -> IO ()
trackDatabase tgvar dbDir = do
  watcher <- initINotify
  tryToEnter ConfigNonWatched watcher dbDir tgvar

watchNewTrack :: FilePath -> String -> Event -> IO ()
watchNewTrack _ _ DeletedSelf = die "Configuration directory deleted, exiting."
watchNewTrack dir tgvar (MovedIn True path _) =
  withAsync (trackDatabase tgvar $ dir </> BSC.unpack path) wait
watchNewTrack dir tgvar (Created True path) =
  withAsync (trackDatabase tgvar $ dir </> BSC.unpack path) wait
watchNewTrack _ _ _ = pure ()

runApp :: Options -> IO ()
runApp Options{..} = do
  mDatabaseDirs <- listDirectory optionsDir
  -- There can be any plain files on top level.
  databaseDirs <- filterM doesDirectoryExist (map (optionsDir </>) . filter notHidden $ mDatabaseDirs)
  mainWatcher@(INotify _ _ _ _ eventsThread) <- initINotify
  void $ addWatch mainWatcher [MoveIn, Create, DeleteSelf] (BSC.pack optionsDir)
          (void . async . watchNewTrack optionsDir optionsToken)
  mapConcurrently_ (trackDatabase optionsToken) databaseDirs
  -- Main thread cannot terminate if watch can create new threads, so parent process must wait for spawned inotify to terminate.
  -- It doesn't know it's pid and must take care about processes spawned in threads which can be killed. So there is a loop.
  -- The only way to terminate a program here -- catch exception if there is no child processes.
  wait eventsThread
