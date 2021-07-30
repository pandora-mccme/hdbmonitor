{-# LANGUAGE OverloadedStrings #-}
module Monitor.Loader where

import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Monitor.DataModel

parseLine :: Text -> Maybe Text
parseLine space = case T.splitOn "=" space of
  [_key, value] -> Just . T.strip $ value
  _ -> Nothing

parseDescription :: [Text] -> Maybe String
parseDescription arg = case catMaybes (map parseLine arg) of
  [] -> Nothing
  a -> Just . intercalate "\n" . map T.unpack $ a

parseAssertion :: [Text] -> Maybe Assertion
parseAssertion arg = case catMaybes (map parseLine arg) of
  [] -> Nothing
  (a:_) -> Just . readAssertion . T.unpack $ a

parseFrequency :: [Text] -> Maybe Int
parseFrequency arg = case catMaybes (map parseLine arg) of
  [] -> Nothing
  (a:_) -> Just . read . T.unpack $ a

parseJob :: Text -> Job
parseJob txt = Job {
    jobSQL = sql
  , jobDescription = parseDescription mDescription
  , jobAssertion = parseAssertion mAssertion
  , jobFrequency = parseFrequency mFrequency
  }
  where
    comments = filter (T.isPrefixOf "--") . map T.strip . T.lines $ txt
    sql = T.encodeUtf8 . T.unlines . filter (not . T.isPrefixOf "--") . map T.strip . T.lines $ txt
    mDescription = filter (T.isInfixOf "description") comments
    mFrequency = filter (T.isInfixOf "frequency") comments
    mAssertion = filter (T.isInfixOf "assertion") comments
