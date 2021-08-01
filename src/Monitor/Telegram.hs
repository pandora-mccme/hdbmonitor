{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Monitor.Telegram where

import Data.ByteString (ByteString)
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Methods
import Telegram.Bot.API.Types
import Telegram.Bot.Simple.BotApp (getEnvToken)

import Monitor.DataModel

standardRequest :: SomeChatId -> Text -> SendMessageRequest
standardRequest chan txt = SendMessageRequest
  { sendMessageChatId                = chan
  , sendMessageText                  = txt
  , sendMessageParseMode             = Just Markdown
  , sendMessageDisableWebPagePreview = Just True
  , sendMessageDisableNotification   = Just False
  , sendMessageReplyToMessageId      = Nothing
  , sendMessageReplyMarkup           = Nothing
  }

postAlert :: SendMessageRequest -> Monitor ()
postAlert msg = asks telegramTokenVar >>= \tgvar -> liftIO $ do
  token <- getEnvToken tgvar
  void $ defaultRunBot token (sendMessage msg)

broadcast :: (SomeChatId -> SendMessageRequest) -> Monitor ()
broadcast f = do
  chans <- asks channels
  mapM_ (postAlert . f. SomeChatId . ChatId) chans

deathNote :: FilePath -> SomeChatId -> SendMessageRequest
deathNote dir chan = standardRequest chan msg
  where
    msg = "*Monitor at " <> (pack dir) <> " has stopped by deleting or moving it's working directory*"

alertThreadDeath :: Monitor ()
alertThreadDeath = asks databaseDirectory
               >>= broadcast . deathNote

connectionErrorMessage :: String -> FilePath -> SomeChatId -> SendMessageRequest
connectionErrorMessage err dir chan = standardRequest chan msg
  where
    msg = "*Cannot connect to database at " <> (pack dir) <> ".* \n\
          \It may indicate cluster restart, check all applications.\n\
          \_Error message_: " <> (pack err)

alertConnectionError :: String -> Monitor ()
alertConnectionError err = asks databaseDirectory
                       >>= broadcast . connectionErrorMessage err

queryErrorMessage :: FilePath -> String -> ByteString -> FilePath -> SomeChatId -> SendMessageRequest
queryErrorMessage path err sql dir chan = standardRequest chan msg
  where
    msg = "*Query error while executing check for " <> (pack dir) <> " at " <> (pack path) <> ".* \n\
          \*Error message: *\n" <> (pack err) <> "\n\
          \*SQL text*: ```\n" <> (decodeUtf8 sql) <> "```\n\
          \It means incorrect assertion (parse errors are treated as 'not null') or error in query."

alertQueryError :: FilePath -> String -> ByteString -> Monitor ()
alertQueryError path err sql = asks databaseDirectory
                           >>= broadcast . queryErrorMessage path err sql

assertionMessage :: FilePath -> Assertion -> ByteString -> FilePath -> String -> SomeChatId -> SendMessageRequest
assertionMessage path assertion sql dir desc chan = standardRequest chan msg
  where
    msg = "*Assertion failed*:\nCheck for " <> (pack dir) <> " at " <> (pack path) <> ". \n\n\
          \*Assertion: *" <> (pack (show assertion)) <> "\n\n\
          \*SQL text*: ```\n" <> (decodeUtf8 sql) <> "```\n\
          \_Check description_:\n" <> (pack desc)

alertFailedAssertion :: FilePath -> PureJob -> Monitor ()
alertFailedAssertion path PureJob{..} = do
  dir <- asks databaseDirectory
  broadcast (assertionMessage path pureJobAssertion pureJobSQL dir pureJobDescription)
