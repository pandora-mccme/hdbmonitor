module Monitor.Queue where

import Control.Monad.Reader

import Monitor.Config

buildQueue :: [FilePath] -> ReaderT Settings IO ()
buildQueue = undefined

stopAll :: ReaderT Settings IO ()
stopAll = undefined

destroyEvent :: ReaderT Settings IO ()
destroyEvent = undefined

restartJob :: FilePath -> ReaderT Settings IO ()
restartJob = undefined

removeJob :: FilePath -> ReaderT Settings IO ()
removeJob = undefined

startJob :: FilePath -> ReaderT Settings IO ()
startJob = undefined
