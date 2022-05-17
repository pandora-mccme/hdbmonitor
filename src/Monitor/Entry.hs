{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
module Monitor.Entry where

import GHC.Conc (labelThread)

import Control.Concurrent
import Control.Concurrent.Async

import System.Directory
import System.Exit
import System.FilePath
import System.INotify

import qualified Data.ByteString.Char8 as BSC

import Monitor.Options (Options(..))
import Monitor.Config
import Monitor.Queue
import Monitor.DataModel

updateEventVariety :: [EventVariety]
updateEventVariety = [Modify, Move, MoveIn, MoveOut, Create, Delete, DeleteSelf, MoveSelf]

jobAction :: (?mutex :: Mutexes) => INotify -> FilePath -> String -> JobAction -> Settings -> FilePath -> IO ()
jobAction watcher dir tgvar action cfg path = if notHidden path
  then if path == configName
    then do
      flip runReaderT cfg . getMonitor $ destroyQueue
      logMessage ("Monitor at " <> dir <> " is stopped due to configuration change. All jobs are removed, monitor will be restarted.")
      tryToEnter dir tgvar
      killINotify watcher
    else
      label path . async . flip runReaderT cfg . getMonitor $ case action of
        Start -> startJob (dir </> path)
        Restart -> restartJob (dir </> path)
        Remove -> removeJob (dir </> path)
  else pure ()

watchTower :: (?mutex :: Mutexes) => INotify -> FilePath -> String -> Settings -> Event -> IO ()
watchTower watcher dir _ cfg DeletedSelf = logMessage ("Monitor at " <> dir <> " is stopped due to directory deletion." )
                                        >> runReaderT (getMonitor $ destroyMonitor watcher) cfg
watchTower watcher dir _ cfg (MovedSelf _) = logMessage ("Monitor at " <> dir <> " is stopped due to directory move." )
                                          >> runReaderT (getMonitor $ destroyMonitor watcher) cfg
watchTower watcher dir tgvar cfg (Modified False (Just path)) =
  jobAction watcher dir tgvar Restart cfg (BSC.unpack path)
watchTower watcher dir tgvar cfg (Deleted False path) =
  jobAction watcher dir tgvar Remove cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (MovedOut False path _) =
  jobAction watcher dir tgvar Remove cfg $ BSC.unpack path
watchTower watcher dir tgvar cfg (MovedIn False path _) =
  jobAction watcher dir tgvar Start cfg (BSC.unpack path)
watchTower watcher dir tgvar cfg (Created False path) =
  jobAction watcher dir tgvar Start cfg $ BSC.unpack path
watchTower _ _ _ _ _ = pure ()

enter :: (?mutex :: Mutexes) => INotify -> FilePath -> [FilePath] -> String -> Settings -> IO ()
enter watcher dir checks tgvar cfg = do
  void $ addWatch watcher updateEventVariety (BSC.pack dir) (watchTower watcher dir tgvar cfg)
  mapM_ (\f -> void . async $ runReaderT (getMonitor $ startJob f) cfg) checks

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

readConfigTillSuccess :: (?mutex :: Mutexes) => INotify -> FilePath -> String -> MVar () -> IO Settings
readConfigTillSuccess watcher dir tgvar configChange = do
  wd <- addWatch watcher updateEventVariety (BSC.pack dir)
    (modifiedCfg configChange)
  cfg <- readConfig dir configChange tgvar
  logMessage $ "Successfully read configuration at " <> dir
  removeWatch wd
  return cfg

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _ = True

isCheck :: FilePath -> Bool
isCheck f = notHidden f && (f /= configName) -- && takeExtension f == ".sql"

readInitialData :: FilePath -> IO [FilePath]
readInitialData dir = do
  contents <- listDirectory dir
  files <- filterM doesFileExist . map (dir </>) . filter isCheck $ contents
  subdirs <- filterM doesDirectoryExist . map (dir </>) $ contents
  print files
  case subdirs of
    [] -> return files
    lst -> mapM (readInitialData) lst
       >>= \nested -> return (files ++ concat nested)

readMonitor :: (?mutex :: Mutexes) => FilePath -> String -> IO (Settings, [FilePath])
readMonitor dir tgvar = do
  configReadMVar <- newEmptyMVar
  cfg <- withINotify (\ino -> readConfigTillSuccess ino dir tgvar configReadMVar)
  checks <- readInitialData dir
  return (cfg, checks)

tryToEnter :: (?mutex :: Mutexes) => FilePath -> String -> IO ()
tryToEnter dir tgvar = do
  (cfg, checks) <- readMonitor dir tgvar
  ino <- initINotify
  enter ino dir checks tgvar cfg

trackDatabase :: (?mutex :: Mutexes) => String -> FilePath -> IO ()
trackDatabase tgvar dbDir = do
  logMessage $ "Started tracking directory " <> dbDir <> " in separate thread."
  tryToEnter dbDir tgvar

-- FIXME: excess thread spawn?
watchNewTrack :: (?mutex :: Mutexes) => FilePath -> String -> Event -> IO ()
watchNewTrack _ _ DeletedSelf = die "Configuration directory deleted, exiting."
watchNewTrack dir tgvar (MovedIn True path _) =
  trackDatabase tgvar $ dir </> BSC.unpack path
watchNewTrack dir tgvar (Created True path) =
  trackDatabase tgvar $ dir </> BSC.unpack path
watchNewTrack _ _ _ = pure ()

label :: String -> IO (Async ()) -> IO ()
label lab action = do
  asyn <- action
  labelThread (asyncThreadId asyn) lab

runApp :: Options -> IO ()
runApp Options{..} = do
  dbMutex <- newMVar ()
  stdoutMutex <- newMVar ()
  let ?mutex = Mutexes{..} in do
    logMessage "dbmonitor process started."
    mDatabaseDirs <- listDirectory optionsDir
    databaseDirs <- filterM doesDirectoryExist (map (optionsDir </>) . filter notHidden $ mDatabaseDirs)
    mainWatcher@(INotify _ _ _ _ eventsThread) <- initINotify
    void $ addWatch mainWatcher [MoveIn, Create, DeleteSelf] (BSC.pack optionsDir)
            ( label optionsDir . async . watchNewTrack optionsDir optionsToken
            )
    mapM_ (void . async . trackDatabase optionsToken) databaseDirs
    wait eventsThread
    logMessage "inotify died"
    killINotify mainWatcher
