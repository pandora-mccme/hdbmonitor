{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Monitor.Queue where

import Control.Concurrent
import qualified Control.Concurrent.Lifted as Lifted
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import System.Directory
import System.FilePath
import System.INotify

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text.IO as T
import Data.Time

import Monitor.DataModel
import Monitor.Loader
import Monitor.DB
import Monitor.Telegram

-- This is a hack. On connection error all thread must try to restart.
touchConfig :: Monitor ()
touchConfig = do
  dir <- asks databaseDirectory
  time <- liftIO getCurrentTime
  liftIO $ setModificationTime (dir </> configName) time

processQueryResult :: FilePath -> PureJob -> JobFeedback -> Monitor ()
processQueryResult _path _ (ConnectionError err) =
  alertConnectionError err >> touchConfig
processQueryResult path PureJob{..} (QueryError err) =
  alertQueryError path err pureJobSQL
processQueryResult path job (AssertionResult value) =
  if value
    then pure ()
    else alertFailedAssertion path job

purify :: Job -> Assertion -> FilePath -> PureJob
purify Job{..} assertion path = PureJob {
    pureJobDescription = fromMaybe ("Job at " <> path) jobDescription
  , pureJobAssertion = fromMaybe assertion jobAssertion
  , pureJobSQL = jobSQL
  }

periodicEvent :: Job -> FilePath -> Monitor ()
periodicEvent job@Job{..} path = forever $ do
  defFreq <- asks defaultFrequency
  defAssert <- asks defaultAssertion
  let pureJob = purify job defAssert path
      delay = 60 * 10^((6)::Int) * (fromMaybe defFreq jobFrequency)
  queryResult <- runSQL pureJob
  processQueryResult path pureJob queryResult
  liftIO $ threadDelay delay

forkWaitable :: Monitor () -> Monitor (ThreadId, MVar ())
forkWaitable action = do
  handle <- liftIO newEmptyMVar
  thread <- Lifted.forkFinally action (\_ -> liftIO $ putMVar handle ())
  return (thread, handle)

startJob :: FilePath -> Monitor ()
startJob path = do
  queue <- asks jobQueue
  job <- liftIO $ parseJob <$> T.readFile path
  queueMap <- liftIO $ readTVarIO queue
  case HM.lookup path queueMap of
    Nothing -> pure ()
    Just accidental_thread -> do
      logMessage ("Job " <> path <> " was probably initiated by different monitor threads. Please report a bug.")
      liftIO $ killThread accidental_thread
  (thread, waitHandle) <- forkWaitable (periodicEvent job path)
  liftIO . atomically $ modifyTVar queue (HM.insert path thread)
  logMessage ("Job " <> path <> " is started.")
  void $ liftIO $ takeMVar waitHandle

removeJob :: FilePath -> Monitor ()
removeJob path = do
  queueTVar <- asks jobQueue
  queue <- liftIO $ readTVarIO queueTVar
  liftIO . killThread $ queue HM.! path
  liftIO . atomically $ modifyTVar queueTVar (HM.delete path)
  logMessage ("Job " <> path <> " is removed")

restartJob :: FilePath -> Monitor ()
restartJob path = do
  queueTVar <- asks jobQueue
  queue <- liftIO $ readTVarIO queueTVar
  liftIO . killThread $ queue HM.! path
  job <- liftIO $ parseJob <$> T.readFile path
  (thread, waitHandle) <- forkWaitable (periodicEvent job path)
  liftIO . atomically $ modifyTVar queueTVar (HM.adjust (\_ -> thread) path)
  logMessage ("Job " <> path <> " is restarted due to file modification.")
  void $ liftIO $ takeMVar waitHandle

destroyQueue :: Monitor ()
destroyQueue = do
  queueTVar <- asks jobQueue
  queue <- liftIO $ readTVarIO queueTVar
  mapM_ (liftIO . killThread) $ HM.elems queue
  liftIO . atomically $ modifyTVar queueTVar (\_ -> HM.empty)

destroyMonitor :: INotify -> Monitor ()
destroyMonitor watcher = do
  destroyQueue
  alertThreadDeath
  liftIO $ killINotify watcher
