module Monitor.Options where

import Options.Applicative

data Options = Options {
    optionsDir   :: FilePath
  , optionsToken :: String
  } deriving (Show, Eq)

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "dir"
     <> short 'D'
     <> metavar "CONFIG_DIR"
     <> value ".monitor"
     <> help "Path to configuration directory" )
  <*> strOption
      ( long "token"
     <> short 'T'
     <> metavar "TOKEN_VAR"
     <> value "TG_TOKEN"
     <> help "Variable with Telegram access token" )

options :: ParserInfo Options
options = info (optionsParser <**> helper)
   ( fullDesc
  <> progDesc "PostgreSQL data consistency monitoring tool"
  <> header "dbmonitor" )
