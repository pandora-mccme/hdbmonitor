{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
module Monitor.Configuration.Read
  ( readMonitor
  , notHidden
  , isCheck
  , collectMonitors
  )
  where

import Control.Concurrent

import System.Directory
import System.FilePath
import System.FSNotify

import Monitor.Configuration.Config
import Monitor.DataModel

collectMonitors :: FilePath -> IO [FilePath]
collectMonitors configDir = do
  mDatabaseDirs <- listDirectory configDir
  relativePaths <- filterM doesDirectoryExist
    (map (configDir </>) . filter notHidden $ mDatabaseDirs)
  currentDir <- getCurrentDirectory
  return $ map (currentDir </>) relativePaths

tryReadConfig :: (?mutex :: Mutexes) => FilePath -> MVar () -> String -> IO Settings
tryReadConfig dir configChange tgvar = do
  takeMVar configChange
  readConfig dir configChange tgvar

readConfig :: (?mutex :: Mutexes) => FilePath -> MVar () -> String -> IO Settings
readConfig dir configChange tgvar = do
  let configPath = dir </> configName
  configExists <- doesFileExist configPath
  if configExists
    then do
      mSettings <- readSettings dir tgvar configPath
      case mSettings of
        Nothing -> tryReadConfig dir configChange tgvar
        Just cfg -> return cfg
    else tryReadConfig dir configChange tgvar

modifiedCfg :: MVar () -> Event -> IO ()
modifiedCfg mvar _ = putMVar mvar ()

readConfigTillSuccess :: (?mutex :: Mutexes)
                      => WatchManager -> FilePath -> String -> MVar () -> IO Settings
readConfigTillSuccess cfgManager dir tgvar configChange = do
  removeWatch <- watchDir cfgManager dir (const True)
    (modifiedCfg configChange)
  cfg <- readConfig dir configChange tgvar
  logMessage $ "Successfully read configuration at " <> dir
  removeWatch
  return cfg

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _ = True

isCheck :: FilePath -> Bool
isCheck f = notHidden f && not (representsConfigName f) -- && takeExtension f == ".sql"

readInitialData :: FilePath -> IO [FilePath]
readInitialData dir = do
  contents <- listDirectory dir
  files <- filterM doesFileExist . map (dir </>) . filter isCheck $ contents
  subdirs <- filterM doesDirectoryExist . map (dir </>) . filter notHidden $ contents
  relativePaths <- case subdirs of
    [] -> return files
    lst -> mapM (readInitialData) lst
       >>= \nested -> return (files ++ concat nested)
  currentDir <- getCurrentDirectory
  return $ map (currentDir </>) relativePaths

readMonitor :: (?mutex :: Mutexes) => FilePath -> String -> IO (Settings, [FilePath])
readMonitor dir tgvar = do
  configReadMVar <- newEmptyMVar
  cfg <- withManager (\cfgManager -> readConfigTillSuccess cfgManager dir tgvar configReadMVar)
  checks <- readInitialData dir
  return (cfg, checks)
