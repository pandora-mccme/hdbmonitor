module Monitor.DataModel where

import Control.Concurrent (ThreadId)

import Data.ByteString (ByteString)

data ConfigWatchFlag = ConfigWatched | ConfigNonWatched
  deriving (Eq, Show)

data Assertion = AssertNull | AssertNotNull | AssertTrue | AssertFalse | AssertZero
  deriving (Eq, Show)

data JobAction = Start | Restart | Remove
  deriving (Eq, Show)

data Job = Job
  { jobId :: ThreadId
  , jobDescription :: Maybe String
  , jobFrequency :: Maybe Int
  , jobAssertion :: Maybe Assertion
  , jobSQL :: ByteString
  } deriving (Eq, Show)
