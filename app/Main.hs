{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Text qualified as T
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem.FileReader.Dynamic (runFileReaderDynamicIO)
import Effectful.FileSystem.FileWriter.Dynamic (runFileWriterDynamicIO)
import Effectful.FileSystem.HandleWriter.Dynamic
  ( die,
    runHandleWriterDynamicIO,
    withBinaryFile,
  )
import Effectful.FileSystem.PathReader.Dynamic (runPathReaderDynamicIO)
import Effectful.FileSystem.PathReader.Dynamic qualified as Dir
import Effectful.FileSystem.PathWriter.Dynamic (runPathWriterDynamicIO)
import Effectful.FileSystem.PathWriter.Dynamic qualified as Dir
import Effectful.FileSystem.Utils (encodeFpToOsThrowM, osp, (<</>>!))
import Effectful.IORef.Static (runIORefStaticIO)
import Effectful.Optparse.Static (runOptparseStaticIO)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Reader.Static (runReader)
import Effectful.Terminal.Dynamic (runTerminalDynamicIO)
import Effectful.Time.Dynamic (runTimeDynamicIO)
import Effectful.Time.Dynamic qualified as Time
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Navi (runNavi)
import Navi.Args (Args, getArgs)
import Navi.Config
  ( Config,
    LogLoc (DefPath, File, Stdout),
    Logging,
    NoteSystem (DBus, NotifySend),
    readConfig,
  )
import Navi.Config.Types
  ( FilesSizeMode (FilesSizeModeDelete, FilesSizeModeWarn),
    defaultSizeMode,
  )
import Navi.Data.NaviLog
  ( LogEnv
      ( MkLogEnv,
        logHandle,
        logLevel,
        logNamespace,
        logQueue
      ),
  )
import Navi.Effectful.Logging (runLoggerDynamic, runLoggerDynamicNS)
import Navi.Effectful.Notify
  ( runNotifyDynamicDBusIO,
    runNotifyDynamicNotifySendIO,
  )
import Navi.Effectful.Pythia (runPythiaDynamicIO)
import Navi.Env.DBus (DBusEnv, mkDBusEnv, runMkDbusEnvIO)
import Navi.Env.NotifySend (NotifySendEnv, mkNotifySendEnv)
import Navi.Prelude
import System.IO qualified as IO

main :: IO ()
main = do
  setUncaughtExceptionHandler (IO.putStrLn . displayException)

  runNaviEff $ do
    args <- getArgs
    config <-
      tryParseConfig args
        `catchAny` writeConfigErr

    withLogEnv (config ^. #logging) $ \logEnv -> do
      let mkNaviEnv :: (LogEnv -> Config -> Eff es env) -> Eff es env
          mkNaviEnv envFn = envFn logEnv config
      case config ^. #noteSystem of
        DBus -> do
          env <- runMkDbusEnvIO $ mkNaviEnv mkDBusEnv
          void
            <$> runReader env
            $ runLoggerDynamicNS @DBusEnv
            $ runLoggerDynamic @DBusEnv
            $ runPythiaDynamicIO
            $ runTypedProcess
            $ runNotifyDynamicDBusIO (runNavi @DBusEnv)
        NotifySend -> do
          env <- mkNaviEnv mkNotifySendEnv
          void
            <$> runReader env
            $ runLoggerDynamicNS @NotifySendEnv
            $ runLoggerDynamic @NotifySendEnv
            $ runPythiaDynamicIO
            $ runTypedProcess
            $ runNotifyDynamicNotifySendIO (runNavi @NotifySendEnv)
  where
    runNaviEff =
      runEff
        . runConcurrent
        . runFileReaderDynamicIO
        . runFileWriterDynamicIO
        . runHandleWriterDynamicIO
        . runIORefStaticIO
        . runOptparseStaticIO
        . runPathReaderDynamicIO
        . runPathWriterDynamicIO
        . runTerminalDynamicIO
        . runTimeDynamicIO

tryParseConfig ::
  ( FileReaderDynamic :> es,
    IORefStatic :> es
  ) =>
  Args Identity ->
  Eff es Config
tryParseConfig =
  readConfig
    . runIdentity
    . view #configFile

withLogEnv ::
  ( Concurrent :> es,
    HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    TerminalDynamic :> es,
    TimeDynamic :> es
  ) =>
  Logging ->
  (LogEnv -> Eff es a) ->
  Eff es a
withLogEnv logging onLogEnv =
  withLogHandle logging $ \logHandle -> do
    logQueue <- newTBQueueA 1000
    onLogEnv
      $ MkLogEnv
        { logHandle,
          logLevel,
          logQueue,
          logNamespace = "main"
        }
  where
    logLevel = fromMaybe LevelError (logging ^. #severity)

withLogHandle ::
  ( HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    TerminalDynamic :> es,
    TimeDynamic :> es
  ) =>
  Logging ->
  (Maybe Handle -> Eff es a) ->
  Eff es a
withLogHandle logging onMHandle = do
  case logLoc' of
    -- Log location defined in config file as stdout.
    Stdout -> onMHandle Nothing
    -- Custom log path.
    File f -> do
      renameIfExists f
      withBinaryFile f WriteMode $ \h -> onMHandle (Just h)
    -- Use the default log path: xdgState </> navi/log
    DefPath -> do
      xdgState <- Dir.getXdgState [osp|navi/|]

      -- handle large log dir
      handleLogSize xdgState sizeMode

      currTime <- fmap replaceSpc <$> Time.getSystemTimeString
      logFile <- xdgState <</>>! (currTime <> ".log")
      stateExists <- Dir.doesDirectoryExist xdgState
      unless stateExists (Dir.createDirectoryIfMissing True xdgState)
      renameIfExists logFile
      withBinaryFile logFile WriteMode $ \h -> onMHandle (Just h)
  where
    logLoc' = fromMaybe DefPath (logging ^. #location)
    sizeMode = fromMaybe defaultSizeMode (logging ^. #sizeMode)

    replaceSpc ' ' = '_'
    replaceSpc x = x

writeConfigErr ::
  ( FileWriterDynamic :> es,
    HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  SomeException ->
  Eff es void
writeConfigErr ex = do
  xdgBase <- Dir.getXdgState [osp|navi|]
  let logFile = xdgBase </> [osp|config_fatal.log|]
  renameIfExists logFile
  writeFileUtf8 logFile $ "Couldn't read config: " <> pack (displayException ex)
  throwM ex

renameIfExists ::
  ( HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
renameIfExists fp = do
  fileExists <- Dir.doesFileExist fp
  when fileExists $ do
    fp' <- uniqName fp
    Dir.renameFile fp fp'

uniqName ::
  forall es.
  ( HandleWriterDynamic :> es,
    PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
uniqName fp = go 1
  where
    go :: Word16 -> Eff es OsPath
    go !counter
      | counter == maxBound = die $ "Failed renaming file: " <> show fp
      | otherwise = do
          fp' <- (fp <>) <$> encodeFpToOsThrowM (show counter)
          b <- Dir.doesFileExist fp'
          if b
            then go (counter + 1)
            else pure fp'

handleLogSize ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    TerminalDynamic :> es
  ) =>
  OsPath ->
  FilesSizeMode ->
  Eff es ()
handleLogSize naviState sizeMode = do
  -- NOTE: Only files should be logs
  logFiles <- Dir.listDirectory naviState
  totalBytes <-
    foldl'
      (\macc path -> liftA2 (+) macc (Dir.getFileSize (naviState </> path)))
      (pure 0)
      logFiles
  let totalBytes' = MkBytes @B @Natural (fromInteger totalBytes)

  case sizeMode of
    FilesSizeModeWarn warnSize ->
      when (totalBytes' > warnSize)
        $ putTextLn
        $ sizeWarning warnSize naviState totalBytes'
    FilesSizeModeDelete delSize ->
      when (totalBytes' > delSize) $ do
        putTextLn $ sizeWarning delSize naviState totalBytes' <> " Deleting logs."
        Dir.removeDirectoryRecursive naviState
        Dir.createDirectoryIfMissing False naviState
  where
    sizeWarning warnSize fp fileSize =
      mconcat
        [ "Warning: log dir '",
          T.pack $ show fp,
          "' has size: ",
          formatBytes fileSize,
          ", but specified threshold is: ",
          formatBytes warnSize,
          "."
        ]

    formatBytes =
      Bytes.formatSized (MkFloatingFormatter (Just 2)) Bytes.sizedFormatterNatural
        . Bytes.normalize
        -- Convert to double _before_ normalizing. We may lose some precision
        -- here, but it is better than normalizing a natural, which will
        -- truncate (i.e. greater precision loss).
        . fmap (fromIntegral @Natural @Double)
