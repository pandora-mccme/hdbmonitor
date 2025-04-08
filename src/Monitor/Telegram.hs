{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
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
  { sendMessageChatId              = chan
  , sendMessageText                = txt
  , sendMessageParseMode           = Just Markdown
  , sendMessageDisableNotification = Just False
  , sendMessageReplyToMessageId    = Nothing
  , sendMessageReplyMarkup         = Nothing
  , sendMessageEntities            = Nothing
  , sendMessageProtectContent      = Nothing
  , sendMessageMessageThreadId     = Nothing
  , sendMessageLinkPreviewOptions  = Nothing
  , sendMessageReplyParameters     = Nothing
  }

postAlert :: SendMessageRequest -> Monitor ()
postAlert msg = asks telegramTokenVar >>= \tgvar -> liftIO $ do
  token <- getEnvToken tgvar
  resp <- defaultRunBot token (sendMessage msg)
  print resp

broadcast :: (SomeChatId -> SendMessageRequest) -> Monitor ()
broadcast f = do
  chans <- asks channels
  mapM_ (postAlert . f. SomeChatId . ChatId) chans

deathNote :: FilePath -> SomeChatId -> SendMessageRequest
deathNote dir chan = standardRequest chan msg
  where
    msg = "*Monitor at " <> (pack dir) <> " has stopped by deleting or moving it's working directory*"

alertThreadDeath :: (?mutex :: Mutexes) => Monitor ()
alertThreadDeath = do
  dir <- asks databaseDirectory
  broadcast $ deathNote dir
  logMessage ("Death alert sent for monitor at " <> dir)

connectionErrorMessage :: String -> FilePath -> SomeChatId -> SendMessageRequest
connectionErrorMessage err dir chan = standardRequest chan msg
  where
    msg = "*Cannot connect to database at " <> (pack dir) <> ".* \n\
          \It may indicate cluster restart, check all applications.\n\
          \_Error message_: " <> (pack err)

alertConnectionError :: (?mutex :: Mutexes) => String -> Monitor ()
alertConnectionError err = do
  dir <- asks databaseDirectory
  broadcast $ connectionErrorMessage err dir
  logMessage ("Database connection problem alert sent for monitor at " <> dir)

queryErrorMessage :: FilePath -> String -> ByteString -> SomeChatId -> SendMessageRequest
queryErrorMessage path err sql chan = standardRequest chan msg
  where
    msg = "*Query error while executing check `"  <> (pack path) <> "`.* \n\
          \*Error message: *\n```" <> (pack err) <> "```\n\
          \*SQL text*: ```\n" <> (decodeUtf8 sql) <> "```\n\
          \It means incorrect assertion (parse errors are treated as 'not null') or error in query."

alertQueryError :: (?mutex :: Mutexes) => FilePath -> String -> ByteString -> Monitor ()
alertQueryError path err sql = do
  broadcast $ queryErrorMessage path err sql
  logMessage ("Query error alert sent for " <> path)

assertionMessage :: FilePath -> Assertion -> ByteString -> String -> SomeChatId -> SendMessageRequest
assertionMessage path assertion sql desc chan = standardRequest chan msg
  where
    msg = "*Assertion failed*:\nCheck `" <> (pack path) <> "`. \n\n\
          \*Assertion: *" <> (pack (show assertion)) <> "\n\n\
          \*SQL text*: ```\n" <> (decodeUtf8 sql) <> "```\n\
          \_Check description_:\n" <> (pack desc)

alertFailedAssertion :: (?mutex :: Mutexes) => FilePath -> PureJob -> Monitor ()
alertFailedAssertion path PureJob{..} = do
  broadcast (assertionMessage path pureJobAssertion pureJobSQL pureJobDescription)
  logMessage ("Failed assertion alert sent for " <> path)
