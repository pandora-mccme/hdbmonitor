{-# LANGUAGE RecordWildCards #-}
module Monitor.Queue where

import Control.Concurrent
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

periodicEvent :: Job -> Settings -> IO ()
periodicEvent = undefined

startJob :: FilePath -> ReaderT Settings IO ()
startJob path = do
  s@Settings{..} <- ask
  job <- liftIO $ parseJob <$> T.readFile path
  thread <- liftIO . forkIO $ periodicEvent job s
  liftIO . atomically $ modifyTVar jobQueue (HM.insert path thread)

removeJob :: FilePath -> ReaderT Settings IO ()
removeJob path = do
  queueTVar <- asks jobQueue
  queue <- liftIO $ readTVarIO queueTVar
  liftIO $ killThread $ queue HM.! path
  liftIO . atomically $ modifyTVar queueTVar (HM.delete path)

restartJob :: FilePath -> ReaderT Settings IO ()
restartJob path = removeJob path >> startJob path

destroyMonitor :: ReaderT Settings IO ()
destroyMonitor = do
  alertThreadDeath
  thread <- liftIO myThreadId
  liftIO $ killThread thread
