{-# LANGUAGE RecordWildCards #-}
module Monitor.Entry where

import Control.Monad.Reader

import System.Directory
import System.FilePath
import System.INotify

import qualified Data.ByteString.Char8 as BSC
import Data.List

import Monitor.Options (Options(..))
import Monitor.Config
import Monitor.Queue
import Monitor.DataModel

configName :: FilePath
configName = "conf.dhall"

updateEventVariety :: [EventVariety]
updateEventVariety = [Modify, Move, Create, Delete, DeleteSelf]

-- watches only config changes.
configWatch :: FilePath -> Event -> IO ()
configWatch = undefined

-- watches check changes.
watchTower :: Settings -> Event -> IO ()
watchTower = undefined

missingConfigCase :: INotify -> FilePath -> IO ()
missingConfigCase watcher dir = do
  putStrLn $ "Configuration file is missing or invalid in " <> dir <> ", ignoring. You do not need to restart after config fix."
  void $ addWatch watcher [MoveIn, Create, Modify, DeleteSelf] (BSC.pack dir) (configWatch dir)

enter :: INotify -> FilePath -> [FilePath] -> Settings -> IO ()
enter watcher dir checks cfg = do
  _ <- addWatch watcher updateEventVariety (BSC.pack dir) (watchTower cfg)
  runReaderT (buildQueue checks) cfg

tryToEnter :: ConfigWatchFlag -> INotify -> FilePath -> String -> IO ()
tryToEnter isWatched watcher dir tgvar = do
  (mConfigPath, checks) <- partition (== configName) <$> listDirectory dir
  case mConfigPath of
    [] -> case isWatched of
      ConfigWatched -> return ()
      ConfigNonWatched -> missingConfigCase watcher dir
    (configPath:_) -> do
      mSettings <- readSettings dir tgvar configPath
      case mSettings of
        Nothing -> missingConfigCase watcher dir
        Just cfg -> enter watcher dir checks cfg

trackDatabase :: FilePath -> String -> FilePath -> IO ()
trackDatabase baseDir tgvar dbDir = do
  let dir = baseDir </> dbDir
  watcher <- initINotify
  tryToEnter ConfigNonWatched watcher dir tgvar

runApp :: Options -> IO ()
runApp Options{..} = do
  databaseDirs <- listDirectory optionsDir
  mapM_ (trackDatabase optionsDir optionsToken) databaseDirs
