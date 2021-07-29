module Main where

import Options.Applicative (execParser)

import Monitor.Options (options)
import Monitor.Entry (runApp)

main :: IO ()
main = execParser options >>= runApp
