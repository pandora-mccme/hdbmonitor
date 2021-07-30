module Monitor.DataModel where

import Data.ByteString (ByteString)

data ConfigWatchFlag = ConfigWatched | ConfigNonWatched
  deriving (Eq, Show)

data Assertion = AssertNull | AssertNotNull | AssertTrue | AssertFalse | AssertZero
  deriving (Eq, Show)

data JobAction = Start | Restart | Remove
  deriving (Eq, Show)

data Job = Job
  { jobDescription :: Maybe String
  , jobFrequency :: Maybe Int
  , jobAssertion :: Maybe Assertion
  , jobSQL :: ByteString
  } deriving (Eq, Show)
