{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Monitor.DataModel (
    module Monitor.DataModel
  , module Control.Monad.Reader
  , Assertion(..)
  , Settings(..)
  , readAssertion
  , logMessage
  ) where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)

import Monitor.Config

newtype Monitor a = Monitor {getMonitor :: ReaderT Settings IO a} deriving
  (Functor, Applicative, Monad, MonadIO, MonadReader Settings, MonadBase IO)

instance MonadBaseControl IO Monitor where
  type StM Monitor a = a
  liftBaseWith f = Monitor $ liftBaseWith $ \q -> f (q . getMonitor)
  restoreM = Monitor . restoreM

configName :: FilePath
configName = "conf.dhall"

data ConfigWatchFlag = ConfigWatched | ConfigNonWatched
  deriving (Eq, Show)

data JobAction = Start | Restart | Remove
  deriving (Eq, Show)

data JobFeedback = ConnectionError String
                 | QueryError String
                 | AssertionResult Bool
  deriving (Eq, Show)

data Job = Job
  { jobDescription :: Maybe String
  , jobFrequency :: Maybe Int
  , jobAssertion :: Maybe Assertion
  , jobSQL :: ByteString
  } deriving (Eq, Show)

data PureJob = PureJob
  { pureJobDescription :: String
  , pureJobAssertion :: Assertion
  , pureJobSQL :: ByteString
  } deriving (Eq, Show)
