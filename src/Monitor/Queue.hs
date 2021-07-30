module Monitor.Queue where

import Control.Monad.Reader

import Monitor.Config

destroyQueue :: ReaderT Settings IO ()
destroyQueue = undefined

destroyProcess :: ReaderT Settings IO ()
destroyProcess = undefined

restartJob :: FilePath -> ReaderT Settings IO ()
restartJob = undefined

removeJob :: FilePath -> ReaderT Settings IO ()
removeJob = undefined

startJob :: FilePath -> ReaderT Settings IO ()
startJob = undefined
