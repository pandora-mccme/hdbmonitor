{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
module Monitor.Entry where

import GHC.Conc (labelThread, atomically)

import System.FilePath
import System.FSNotify

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Monitor.Configuration.Options (Options(..))
import Monitor.Configuration.Config
import Monitor.Configuration.Read
import Monitor.Queue
import Monitor.DataModel

watchTower :: (?mutex :: Mutexes)
           => MVar () -> FilePath -> String -> Settings
           -> TVar (HashMap FilePath (MVar ())) -> Event -> IO ()
watchTower monitorHolder dir tgvar cfg locksTVar event = do
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
      trackDatabase tgvar dir locksTVar

trackDatabase :: (?mutex :: Mutexes) => String -> FilePath
              -> TVar (HashMap FilePath (MVar ())) -> IO ()
trackDatabase tgvar dbDir locksTVar = do
  (cfg, checks) <- readMonitor dbDir tgvar
  logMessage $ "Monitor at " <> dbDir <> " is started."
  withManager $
    \monitorManager -> do
      lock <- newEmptyMVar
      atomically $ modifyTVar locksTVar (HM.insert dbDir lock)
      void $ watchTree monitorManager dbDir (const True)
             (watchTower lock dbDir tgvar cfg locksTVar)
      mapM_ (\f -> void . async $ runReaderT (getMonitor $ startJob f) cfg) checks
      takeMVar lock

watchNewTrack :: (?mutex :: Mutexes) => String
              -> TVar (HashMap FilePath (MVar ())) -> Event -> IO ()
watchNewTrack _ locksTVar (Removed path _ True) = do
  locks <- liftIO $ readTVarIO locksTVar
  putMVar (locks HM.! path) ()
  atomically $ modifyTVar locksTVar (HM.delete path)
  logMessage $ "Monitor at " <> path <> " is stopped due to directory deletion."
watchNewTrack tgvar locksTVar (Added path _ True) =
  spawnMonitorThread tgvar locksTVar path
watchNewTrack _ _ _ = pure ()

label :: String -> IO (Async ()) -> IO ()
label lab action = do
  asyn <- action
  labelThread (asyncThreadId asyn) lab

spawnMonitorThread :: (?mutex :: Mutexes) => String
                   -> TVar (HashMap FilePath (MVar ())) -> FilePath -> IO ()
spawnMonitorThread tgvar locksTVar dir =
  label dir . async $ trackDatabase tgvar dir locksTVar

runApp :: Options -> IO ()
runApp Options{..} = do
  stdoutMutex <- newMVar ()
  let ?mutex = Mutexes{..} in do
    logMessage "dbmonitor process started."
    databaseDirs <- collectMonitors optionsDir
    eventChannel <- newChan
    locksTVar <- newTVarIO HM.empty
    withManager $ \mainWatcher -> do
      void $ watchTreeChan mainWatcher optionsDir (const True) eventChannel
      forM_ databaseDirs $ spawnMonitorThread optionsToken locksTVar
      forever $ do
        event <- readChan eventChannel
        watchNewTrack optionsToken locksTVar event
