{-# LANGUAGE RecordWildCards #-}
module Monitor.Entry where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader

import System.Directory
import System.FilePath
import System.INotify

import qualified Data.ByteString.Char8 as BSC
import Data.List

import Monitor.Options (Options(..))
import Monitor.Config
import Monitor.Queue
import Monitor.DataModel

configName :: FilePath
configName = "conf.dhall"

updateEventVariety :: [EventVariety]
updateEventVariety = [Modify, MoveIn, MoveOut, Create, Delete, DeleteSelf]

changeConfigAction :: INotify -> String -> FilePath -> FilePath -> IO ()
changeConfigAction watcher dir tgvar path = if path == configName
  then tryToEnter ConfigWatched watcher dir tgvar
  else pure ()

-- watches only config changes.
configWatch :: INotify -> String -> FilePath -> Event -> IO ()
configWatch watcher dir tgvar (Modified False (Just path)) =
  changeConfigAction watcher dir tgvar $ BSC.unpack path
configWatch watcher dir tgvar (MovedIn False path _) =
  changeConfigAction watcher dir tgvar $ BSC.unpack path
configWatch watcher dir tgvar (Created False path) =
  changeConfigAction watcher dir tgvar $ BSC.unpack path
configWatch _ _ _ _ = pure ()

jobAction :: INotify -> FilePath -> String -> JobAction -> Settings -> FilePath -> IO ()
jobAction watcher dir tgvar action cfg path = if notHidden path
  then if path == configName
    then runReaderT destroyQueue cfg
      >> tryToEnter ConfigNonWatched watcher dir tgvar
    else flip runReaderT cfg $ case action of
      Start -> startJob path
      Restart -> restartJob path
      Remove -> removeJob path
  else pure ()

-- watches check changes.
{-
  Expected behavior:
  On config changes -- drop all jobs, execute tryToEnter. On success inotify process is kept alive.
  On file changes -- actions for each type of event.
  Problem -- connection between job and filename, seems easy to handle.
  Also note behavior on file renames -- two successive alerts comes, one deletes the job, one starts the same with another id.
  DeleteSelf event must trigger suicide alert and immediate exit.
-}
watchTower :: INotify -> FilePath -> String -> Settings -> Event -> IO ()
watchTower _ _ _ cfg DeletedSelf = runReaderT destroyProcess cfg
watchTower watcher dir tgvar cfg (Modified False (Just path)) =
  jobAction watcher dir tgvar Restart cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (Deleted False path) =
  jobAction watcher dir tgvar Remove cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (MovedOut False path _) =
  jobAction watcher dir tgvar Remove cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (MovedIn False path _) =
  jobAction watcher dir tgvar Start cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (Created False path) =
  jobAction watcher dir tgvar Start cfg $ BSC.unpack path
watchTower _ _ _ _ _ = pure ()

missingConfigCase :: INotify -> FilePath -> String -> IO ()
missingConfigCase watcher dir tgvar = do
  putStrLn $ "Configuration file is missing or invalid in " <> dir <> ", ignoring. You do not need to restart after config fix."
  void $ addWatch watcher [MoveIn, Create, Modify] (BSC.pack dir) (configWatch watcher tgvar dir)

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
  newWatcher <- initINotify
  _ <- addWatch newWatcher updateEventVariety (BSC.pack dir) (watchTower newWatcher dir tgvar cfg)
  -- In directories there may be any content.
  checkFiles <- filter notHidden <$> filterM doesFileExist checks
  runReaderT (mapM_ startJob checkFiles) cfg

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
      mSettings <- readSettings dir tgvar configPath
      case mSettings of
        Nothing -> maybeAddConfigWatch watcher isWatched dir tgvar
        Just cfg -> enter watcher dir checks tgvar cfg

finalizer :: FilePath -> Either SomeException () -> IO ()
finalizer dir (Right ()) =
  putStrLn $ "monitor for " <> dir <> " suddenly decided to be mortal with no exception or command received"
finalizer dir (Left e) =
  putStrLn $ "monitor for " <> dir <> " is dead by following reason: " <> show e

trackDatabase :: FilePath -> String -> FilePath -> IO ()
trackDatabase baseDir tgvar dbDir = void . flip forkFinally (finalizer dbDir) $
  do
    let dir = baseDir </> dbDir
    watcher <- initINotify
    tryToEnter ConfigNonWatched watcher dir tgvar

runApp :: Options -> IO ()
runApp Options{..} = do
  databaseDirs <- listDirectory optionsDir
  mapM_ (trackDatabase optionsDir optionsToken) databaseDirs
