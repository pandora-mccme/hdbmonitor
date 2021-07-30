{-# LANGUAGE RecordWildCards #-}
module Monitor.Queue where

import Control.Concurrent
import qualified Control.Concurrent.Lifted as Lifted
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM

import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.IO as T

import Monitor.DataModel
import Monitor.Config
--import Monitor.DB
import Monitor.Telegram

parseJob :: Text -> Job
parseJob = undefined

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
  liftIO $ killThread $ queue HM.! path
  liftIO . atomically $ modifyTVar queueTVar (HM.delete path)

restartJob :: FilePath -> Monitor ()
restartJob path = removeJob path >> startJob path

destroyMonitor :: Monitor ()
destroyMonitor = do
  alertThreadDeath
  thread <- Lifted.myThreadId
  Lifted.killThread thread
