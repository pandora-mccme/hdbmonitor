{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Monitor.Config where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.IO.Class

import System.FilePath

import qualified Data.ByteString.Char8 as BSC
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Time

import qualified Hasql.Connection as HaSQL

import Dhall ( Generic, auto, inputFile, FromDhall, Natural )
import Dhall.Deriving

logMessage :: MonadIO m => String -> m ()
logMessage event = do
  time <- liftIO getCurrentTime
  liftIO . putStrLn $ (show time) <> ": " <> event

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

data Assertion = AssertNull | AssertNotNull | AssertTrue | AssertFalse | AssertZero
  deriving (Eq, Show)

data Settings = Settings
  { dbConnection :: HaSQL.Connection
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
-- NOTE: mention in README.
readAssertion _ = AssertNotNull

readSettings :: FilePath -> String -> FilePath -> IO (Maybe Settings)
readSettings dbDir tokenVar configName = do
  cfg <- try $ inputFile auto (dbDir </> configName)
  case cfg of
    Left ex -> do
      logMessage ("Config for " <> dbDir <> " cannot be read. See exception below.")
      putStrLn ("Exception: " <> show @SomeException ex)
      return Nothing
    Right Config{..} -> do
      dbConnection <- HaSQL.acquire (BSC.pack configConnection)
      case dbConnection of
        Left _ -> do
          logMessage ("Config error: connection string for " <> dbDir <> " directory does not provide connection to any database")
          return Nothing
        Right conn -> do
          queue <- newTVarIO HM.empty
          return . Just $ Settings
            { dbConnection = conn
            , channels = map fromIntegral configChannels
            , defaultFrequency = fromIntegral configFrequency
            , defaultAssertion = readAssertion configAssertion
            , telegramTokenVar = tokenVar
            , databaseDirectory = dbDir
            , jobQueue = queue
            }
