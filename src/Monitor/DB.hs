{-# LANGUAGE RecordWildCards #-}
module Monitor.DB where

import Data.ByteString (ByteString)

import qualified Hasql.Session as HaSQL
import qualified Hasql.Statement as HaSQL
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import Monitor.DataModel
import Monitor.Config

decodeAssertNull :: D.Result Bool
decodeAssertNull = test <$> D.rowMaybe (D.column (D.nullable (D.custom (\_ _ -> Right ()))))
  where
    test Nothing = True
    test (Just Nothing) = True
    test (Just (Just ())) = False

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
  AssertNull -> HaSQL.Statement sql E.noParams decodeAssertNull True
  AssertNotNull -> HaSQL.Statement sql E.noParams decodeAssertNotNull True
  AssertZero -> HaSQL.Statement sql E.noParams decodeAssertZero True
  AssertTrue -> HaSQL.Statement sql E.noParams decodeAssertTrue True
  AssertFalse -> HaSQL.Statement sql E.noParams decodeAssertFalse True

runSQL :: PureJob -> Monitor JobFeedback
runSQL PureJob{..} = do
  conn <- asks dbConnection
  result <- liftIO $ HaSQL.run (session pureJobAssertion pureJobSQL) conn
  return $ case result of
    Left (HaSQL.QueryError _ _ (HaSQL.ClientError err)) ->
      ConnectionError (show err)
    Left (HaSQL.QueryError _ _ (HaSQL.ResultError err)) ->
      QueryError (show err)
    Right assertionResult -> AssertionResult assertionResult
