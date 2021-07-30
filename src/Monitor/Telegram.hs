module Monitor.Telegram where

import Control.Monad.Reader

import Monitor.Config

alertThreadDeath :: ReaderT Settings IO ()
alertThreadDeath = undefined
