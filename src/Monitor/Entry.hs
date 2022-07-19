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

watchTower :: (?mutex :: Mutexes)
           => MVar () -> FilePath -> String -> Settings -> Event -> IO ()
watchTower monitorHolder dir _ cfg (Removed path _ True) = if path == dir
  then logMessage ("Monitor at " <> dir <> " is stopped due to directory deletion." )
    >> runReaderT (getMonitor $ destroyMonitor monitorHolder) cfg
  else pure ()
watchTower monitorHolder dir tgvar cfg event = do
  let path = eventPath event
      filename = takeFileName path
  if isCheck filename
    then do
      label path . async
                 . flip runReaderT cfg
                 . getMonitor $
        case event of
          Modified _ _ False -> restartJob path
          Removed _ _ False -> removeJob path
          Added _ _ False -> startJob path
          _ -> pure ()
    else when (representsConfigName filename) $ do
      putMVar monitorHolder ()
      flip runReaderT cfg . getMonitor $ destroyQueue
      logMessage ("Monitor at " <> dir <> " is stopped due to configuration change. All jobs are removed, monitor will be restarted.")
      trackDatabase tgvar dir

trackDatabase :: (?mutex :: Mutexes) => String -> FilePath -> IO ()
trackDatabase tgvar dbDir = do
  (cfg, checks) <- readMonitor dbDir tgvar
  logMessage $ "Monitor at " <> dbDir <> " is started."
  withManager $
    \monitorManager -> do
      hold <- newEmptyMVar
      void $ watchTree monitorManager dbDir (const True)
             (watchTower hold dbDir tgvar cfg)
      mapM_ (\f -> void . async $ runReaderT (getMonitor $ startJob f) cfg) checks
      takeMVar hold

watchNewTrack :: (?mutex :: Mutexes) => FilePath -> String -> Event -> IO ()
watchNewTrack dir _ (Removed path _ True) = when (path == dir) $
  die "Configuration directory deleted, exiting."
watchNewTrack dir tgvar event@(Added path _ True) = do
  print event
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
