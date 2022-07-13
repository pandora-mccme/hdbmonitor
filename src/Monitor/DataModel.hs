{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Monitor.DataModel (
    module Monitor.DataModel
  , module Control.Monad.Reader
  , Assertion(..)
  , Settings(..)
  , Mutexes(..)
  , readAssertion
  , logMessage
  ) where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)

import Monitor.Configuration.Config

newtype Monitor a = Monitor {getMonitor :: ReaderT Settings IO a} deriving
  (Functor, Applicative, Monad, MonadIO, MonadReader Settings, MonadBase IO, MonadBaseControl IO)
  via ReaderT Settings IO

configName :: FilePath
configName = "conf.dhall"

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
