{-# LANGUAGE RecordWildCards #-}
module Monitor.Entry where

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
updateEventVariety = [Modify, Move, Create, Delete, DeleteSelf]

-- watches only config changes.
configWatch :: INotify -> String -> FilePath -> Event -> IO ()
-- Looks like we do not need any data about event.
configWatch watcher dir tgvar _ = tryToEnter ConfigWatched watcher dir tgvar

-- watches check changes.
{--
  Expected behavior:
  On config changes -- drop all jobs, execute tryToEnter. On success inotify process is kept alive.
  On file changes -- actions for each type of event.
  Problem -- connection between job and filename, seems easy to handle.
  Also note behavior on file renames -- two successive alerts comes, one deletes the job, one starts the same with another id.
  DeleteSelf event must trigger suicide alert and immediate exit.
--}
watchTower :: Settings -> Event -> IO ()
watchTower = undefined

missingConfigCase :: INotify -> FilePath -> String -> IO ()
missingConfigCase watcher dir tgvar = do
  putStrLn $ "Configuration file is missing or invalid in " <> dir <> ", ignoring. You do not need to restart after config fix."
  void $ addWatch watcher [MoveIn, Create, Modify] (BSC.pack $ dir </> configName) (configWatch watcher tgvar dir)

enter :: INotify -> FilePath -> [FilePath] -> Settings -> IO ()
enter watcher dir checks cfg = do
  {--
    After successful start behavior changes: config now must be watched by process taking care about job queue,
    we don't want old settings to be applied so far.
    Hence on successful start we have to close watch descriptor. But we cannot pass it to it's own event handler.
    So we must restart whole inotify.
    First inotify process watches only config, second -- only queue.
    When config breaks, it turns into loop of starting process and dies as soon as queue is successfully restarted,
  --}
  killINotify watcher
  newWatcher <- initINotify
  _ <- addWatch newWatcher updateEventVariety (BSC.pack dir) (watchTower cfg)
  runReaderT (buildQueue checks) cfg

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
        Just cfg -> enter watcher dir checks cfg

trackDatabase :: FilePath -> String -> FilePath -> IO ()
trackDatabase baseDir tgvar dbDir = do
  let dir = baseDir </> dbDir
  watcher <- initINotify
  tryToEnter ConfigNonWatched watcher dir tgvar

runApp :: Options -> IO ()
runApp Options{..} = do
  databaseDirs <- listDirectory optionsDir
  mapM_ (trackDatabase optionsDir optionsToken) databaseDirs
