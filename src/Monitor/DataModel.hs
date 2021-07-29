module Monitor.DataModel where

data ConfigWatchFlag = ConfigWatched | ConfigNonWatched
  deriving (Eq, Show)

data Assertion = AssertNull | AssertNotNull | AssertTrue | AssertFalse | AssertZero
  deriving (Eq, Show)
