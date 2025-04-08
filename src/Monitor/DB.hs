{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
module Monitor.DB where

import Control.Exception.Lifted (bracket)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V

import qualified Hasql.Connection as HaSQL
import qualified Hasql.Session as HaSQL
import qualified Hasql.Statement as HaSQL
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import Monitor.DataModel

decodeAssertResultless :: D.Result Bool
decodeAssertResultless = (\() -> True) <$> D.noResult

decodeAssertNull :: D.Result Bool
decodeAssertNull = test . V.toList <$> D.rowVector (D.column (D.nullable (D.custom (\_ _ -> Right ()))))
  where
    test [] = True
    test [Nothing] = True
    test _ = False

decodeAssertNotNull :: D.Result Bool
decodeAssertNotNull = not <$> decodeAssertNull

decodeAssertTrue :: D.Result Bool
decodeAssertTrue = test <$> D.rowMaybe (D.column (D.nullable D.bool))
  where
    test Nothing = False
    test (Just Nothing) = False
    test (Just (Just a)) = a

decodeAssertFalse :: D.Result Bool
decodeAssertFalse = test <$> D.rowMaybe (D.column (D.nullable D.bool))
  where
    test Nothing = False
    test (Just Nothing) = False
    test (Just (Just a)) = not a

decodeAssertZero :: D.Result Bool
decodeAssertZero = test <$> D.rowMaybe (D.column (D.nullable D.int4))
  where
    test Nothing = False
    test (Just Nothing) = False
    test (Just (Just a)) = a == 0

session :: Assertion -> ByteString -> HaSQL.Session Bool
session assertion sql = HaSQL.statement () $ case assertion of
  AssertNull -> HaSQL.Statement sql E.noParams decodeAssertNull False
  AssertNotNull -> HaSQL.Statement sql E.noParams decodeAssertNotNull False
  AssertZero -> HaSQL.Statement sql E.noParams decodeAssertZero False
  AssertTrue -> HaSQL.Statement sql E.noParams decodeAssertTrue False
  AssertFalse -> HaSQL.Statement sql E.noParams decodeAssertFalse False
  AssertResultless -> HaSQL.Statement sql E.noParams decodeAssertResultless False

runSQL :: PureJob -> Monitor JobFeedback
runSQL PureJob{..} = do
  connectionString <- asks dbConnection
  bracket (liftIO $ HaSQL.acquire (BSC.pack connectionString))
          (liftIO . mapM HaSQL.release) $ \econn -> do
    case econn of
      Left err -> return $ ConnectionError (show err)
      Right conn -> do
        !result <- liftIO $ HaSQL.run (session pureJobAssertion pureJobSQL) conn
        return $ case result of
          Left (HaSQL.QueryError _ _ (HaSQL.ClientError err)) ->
            ConnectionError (show err)
          Left (HaSQL.QueryError _ _ (HaSQL.ResultError err)) ->
            QueryError (show err)
          Right assertionResult -> AssertionResult assertionResult
