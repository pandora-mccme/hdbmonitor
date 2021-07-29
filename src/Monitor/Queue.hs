module Monitor.Queue where

import Control.Monad.Reader

import Monitor.Config

buildQueue :: [FilePath] -> ReaderT Settings IO ()
buildQueue = undefined
