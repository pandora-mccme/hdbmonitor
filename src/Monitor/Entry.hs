{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
module Monitor.Entry where

import GHC.Conc (labelThread)

import Control.Concurrent
import Control.Concurrent.Async

import System.Exit
import System.FilePath
import System.FSNotify

import Monitor.Configuration.Options (Options(..))
import Monitor.Configuration.Config
import Monitor.Configuration.Read
import Monitor.Queue
import Monitor.DataModel

jobAction :: (?mutex :: Mutexes) => WatchManager -> FilePath -> String -> JobAction -> Settings -> FilePath -> IO ()
jobAction watcher dir tgvar action cfg path = if notHidden path
  then if path == configName
    then do
      stopManager watcher
      flip runReaderT cfg . getMonitor $ destroyQueue
      logMessage ("Monitor at " <> dir <> " is stopped due to configuration change. All jobs are removed, monitor will be restarted.")
      trackDatabase tgvar dir
    else
      label path . async . flip runReaderT cfg . getMonitor $ case action of
        Start -> startJob (dir </> path)
        Restart -> restartJob (dir </> path)
        Remove -> removeJob (dir </> path)
  else pure ()

watchTower :: (?mutex :: Mutexes)
           => WatchManager -> FilePath -> String -> Settings -> Event -> IO ()
watchTower watcher dir _ cfg (Removed path _ True)
    = if path == dir
      then logMessage ("Monitor at " <> dir <> " is stopped due to directory deletion." )
        >> runReaderT (getMonitor $ destroyMonitor watcher) cfg
      else pure ()
watchTower watcher dir tgvar cfg (Modified path _ False) =
  jobAction watcher dir tgvar Restart cfg path
watchTower watcher dir tgvar cfg (Removed path _ False) =
  jobAction watcher dir tgvar Remove cfg path
watchTower watcher dir tgvar cfg (Added path _ False) =
  jobAction watcher dir tgvar Start cfg path
watchTower _ _ _ _ _ = pure ()

trackDatabase :: (?mutex :: Mutexes) => String -> FilePath -> IO ()
trackDatabase tgvar dbDir = withManager $
  \monitorManager -> do
    (cfg, checks) <- readMonitor dbDir tgvar
    logMessage $ "Monitor at " <> dbDir <> " is started."
    void $ watchTree monitorManager dbDir (const True) (watchTower monitorManager dbDir tgvar cfg)
    mapM_ (\f -> void . async $ runReaderT (getMonitor $ startJob f) cfg) checks

-- FIXME: excess thread spawn?
watchNewTrack :: (?mutex :: Mutexes) => FilePath -> String -> Event -> IO ()
watchNewTrack dir _ (Removed path _ True)
    = if path == dir
      then die "Configuration directory deleted, exiting."
      else pure ()
watchNewTrack dir tgvar (Added path _ True) =
  spawnMonitorThread tgvar $ dir </> path
watchNewTrack _ _ _ = pure ()

label :: String -> IO (Async ()) -> IO ()
label lab action = do
  asyn <- action
  labelThread (asyncThreadId asyn) lab

spawnMonitorThread :: (?mutex :: Mutexes) => String -> FilePath -> IO ()
spawnMonitorThread tgvar dir =
  label dir . async $ trackDatabase tgvar dir

trackNewMonitors :: (?mutex :: Mutexes) => Options -> IO ()
trackNewMonitors Options{..} = do
  withManager $ \mainWatcher -> do
    void $ watchTree mainWatcher optionsDir (const True)
            ( watchNewTrack optionsDir optionsToken
            )

runApp :: Options -> IO ()
runApp opts@Options{..} = do
  stdoutMutex <- newMVar ()
  let ?mutex = Mutexes{..} in do
    logMessage "dbmonitor process started."
    databaseDirs <- collectMonitors optionsDir
    trackNewMonitors opts
    forM_ databaseDirs $ spawnMonitorThread optionsToken
    void . forever $ threadDelay maxBound
