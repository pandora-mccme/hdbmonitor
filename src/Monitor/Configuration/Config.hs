{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
module Monitor.Configuration.Config where

import System.Console.ANSI
import System.IO (hFlush, stdout)

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.IO.Class

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Time

import Dhall ( Generic, auto, inputFile, FromDhall, Natural )
import Dhall.Deriving

logMessage :: (?mutex :: Mutexes) => MonadIO m => String -> m ()
logMessage event = liftIO $ do
  takeMVar (stdoutMutex ?mutex)
  !time <- getCurrentTime
  setSGR [SetColor Foreground Dull Green]
  putStr $ (show time) <> ": "
  setSGR [SetDefaultColor Foreground]
  putStrLn $ event
  hFlush stdout
  putMVar (stdoutMutex ?mutex) ()

data Mutexes = Mutexes {
    stdoutMutex :: MVar ()
  } deriving (Eq)

data Config = Config
  { configConnection :: String
  , configChannels :: [Int]
  , configFrequency :: Natural
  , configAssertion :: String
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromDhall)
    via Codec (Dhall.Deriving.Field (SnakeCase <<< DropPrefix "config")) Config

-- NOTE: resultless is for periodic actions.
data Assertion = AssertNull | AssertNotNull | AssertTrue | AssertFalse | AssertZero | AssertResultless
  deriving (Eq, Show)

data Settings = Settings
  { dbConnection :: String
  , channels :: [Integer]
  , defaultFrequency :: Int
  , defaultAssertion :: Assertion
  , telegramTokenVar :: String
  , databaseDirectory :: FilePath
  , jobQueue :: TVar (HashMap FilePath ThreadId)
  }

readAssertion :: String -> Assertion
readAssertion "null" = AssertNull
readAssertion "true" = AssertTrue
readAssertion "false" = AssertFalse
readAssertion "zero" = AssertZero
readAssertion "resultless" = AssertResultless
-- NOTE: mention in README.
readAssertion _ = AssertNotNull

readSettings :: (?mutex :: Mutexes) => FilePath -> String -> FilePath -> IO (Maybe Settings)
readSettings dbDir tokenVar configPath = do
  cfg <- try $ inputFile auto configPath
  case cfg of
    Left ex -> do
      logMessage ("Config for " <> dbDir <> " cannot be read. See exception below.")
      putStrLn ("Exception: " <> show @SomeException ex)
      return Nothing
    Right Config{..} -> do
      queue <- newTVarIO HM.empty
      return . Just $ Settings
        { dbConnection = configConnection
        , channels = map fromIntegral configChannels
        , defaultFrequency = fromIntegral configFrequency
        , defaultAssertion = readAssertion configAssertion
        , telegramTokenVar = tokenVar
        , databaseDirectory = dbDir
        , jobQueue = queue
        }
