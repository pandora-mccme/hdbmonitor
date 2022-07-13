module Main where

import Options.Applicative (execParser)

import Monitor.Configuration.Options (options)
import Monitor.Entry (runApp)

main :: IO ()
main = execParser options >>= runApp
