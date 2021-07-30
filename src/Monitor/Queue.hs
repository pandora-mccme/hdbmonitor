{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Monitor.Queue where

import Control.Concurrent
import qualified Control.Concurrent.Lifted as Lifted
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as T

import Monitor.DataModel
import Monitor.Config
import Monitor.Loader
--import Monitor.DB
import Monitor.Telegram

periodicEvent :: Job -> Settings -> Monitor ()
periodicEvent = undefined

startJob :: FilePath -> Monitor ()
startJob path = do
  s@Settings{..} <- ask
  job <- liftIO $ parseJob <$> T.readFile path
  thread <- Lifted.fork $ periodicEvent job s
  liftIO . atomically $ modifyTVar jobQueue (HM.insert path thread)

removeJob :: FilePath -> Monitor ()
removeJob path = do
  queueTVar <- asks jobQueue
  queue <- liftIO $ readTVarIO queueTVar
  liftIO . killThread $ queue HM.! path
  liftIO . atomically $ modifyTVar queueTVar (HM.delete path)

removeAllJobs :: Monitor ()
removeAllJobs = do
  queueTVar <- asks jobQueue
  queue <- liftIO $ readTVarIO queueTVar
  mapM_ (liftIO . killThread) $ HM.elems queue
  liftIO . atomically $ modifyTVar queueTVar (\_ -> HM.empty)

restartJob :: FilePath -> Monitor ()
restartJob path = removeJob path >> startJob path

destroyMonitor :: Monitor ()
destroyMonitor = do
  removeAllJobs
  alertThreadDeath
  thread <- Lifted.myThreadId
  Lifted.killThread thread
