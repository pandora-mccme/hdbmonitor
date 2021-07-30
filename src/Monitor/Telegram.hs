module Monitor.Telegram where

import Data.ByteString (ByteString)

import Monitor.DataModel

alertThreadDeath :: Monitor ()
alertThreadDeath = undefined

alertConnectionError :: String -> Monitor ()
alertConnectionError = undefined

alertQueryError :: FilePath -> String -> ByteString -> Monitor ()
alertQueryError = undefined

alertFailedAssertion :: FilePath -> PureJob -> Monitor ()
alertFailedAssertion = undefined
