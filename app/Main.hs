{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (..))
import Data.Functor.Identity (Identity (..))
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter (die)
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter (MonadPathWriter)
import Effects.FileSystem.PathWriter qualified as Dir
import Effects.Time (MonadTime)
import Effects.Time qualified as Time
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Navi (runNavi, runNaviT)
import Navi.Args (Args (..), getArgs)
import Navi.Config
  ( Config (..),
    LogLoc (..),
    Logging (..),
    NoteSystem (..),
    readConfig,
  )
import Navi.Config.Types (FilesSizeMode (..), defaultSizeMode)
import Navi.Data.NaviLog
  ( LogEnv (MkLogEnv),
    LogFile (MkLogFile, finalizer, handle),
    logFile,
    logLevel,
    logNamespace,
  )
import Navi.Env.DBus (mkDBusEnv)
import Navi.Env.NotifySend (mkNotifySendEnv)
import Navi.Prelude

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  args <- getArgs
  config <-
    tryParseConfig args
      `catchAny` writeConfigErr

  let mkLogEnvFn = mkLogEnv (config ^. #logging)
  bracket mkLogEnvFn closeLogging $ \logEnv -> do
    let mkNaviEnv :: forall env. _ -> IO env
        mkNaviEnv envFn = envFn logEnv config
    case config ^. #noteSystem of
      DBus -> mkNaviEnv mkDBusEnv >>= runWithEnv
      NotifySend -> mkNaviEnv mkNotifySendEnv >>= runWithEnv
  where
    runWithEnv env = absurd <$> runNaviT runNavi env
    closeLogging env = do
      let mFinalizer = env ^? #logFile %? #finalizer
      fromMaybe (pure ()) mFinalizer

tryParseConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadIORef m,
    MonadThrow m
  ) =>
  Args Identity ->
  m Config
tryParseConfig =
  readConfig
    . runIdentity
    . view #configFile

mkLogEnv ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  Logging ->
  m LogEnv
mkLogEnv logging = do
  xdgState <- Dir.getXdgState "navi/"

  -- handle large log dir
  handleLogSize xdgState sizeMode

  logFile <- case logLoc' of
    -- Use the default log path: xdgState </> navi/log
    DefPath -> do
      currTime <- fmap replaceSpc <$> Time.getSystemTimeString
      let logFile = xdgState </> (currTime <> ".log")
      stateExists <- Dir.doesDirectoryExist xdgState
      unless stateExists (Dir.createDirectoryIfMissing True xdgState)
      renameIfExists logFile
      h <- openBinaryFile logFile WriteMode
      pure $
        Just $
          MkLogFile
            { handle = h,
              finalizer = hFlush h `finally` hClose h
            }
    -- Custom log path.
    File f -> do
      renameIfExists f
      h <- openBinaryFile f WriteMode
      pure $
        Just $
          MkLogFile
            { handle = h,
              finalizer = hFlush h `finally` hClose h
            }
    -- Log location defined in config file as stdout.
    Stdout -> pure Nothing
  pure $
    MkLogEnv
      { logFile,
        logLevel,
        logNamespace = "main"
      }
  where
    logLevel = fromMaybe LevelError (logging ^. #severity)
    logLoc' = fromMaybe DefPath (logging ^. #location)
    sizeMode = fromMaybe defaultSizeMode (logging ^. #sizeMode)

    replaceSpc ' ' = '_'
    replaceSpc x = x

writeConfigErr ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadThrow m
  ) =>
  SomeException ->
  m void
writeConfigErr ex = do
  xdgBase <- Dir.getXdgState "navi/"
  let logFile = xdgBase </> "config_fatal.log"
  renameIfExists logFile
  writeFileUtf8 logFile $ "Couldn't read config: " <> pack (displayException ex)
  throwCS ex

renameIfExists ::
  ( HasCallStack,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadThrow m
  ) =>
  Path ->
  m ()
renameIfExists fp = do
  fileExists <- Dir.doesFileExist fp
  when fileExists $ do
    fp' <- uniqName fp
    Dir.renameFile fp fp'

uniqName ::
  forall m.
  ( HasCallStack,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Path ->
  m Path
uniqName fp = go 1
  where
    go :: Word16 -> m Path
    go !counter
      | counter == maxBound = die $ "Failed renaming file: " <> fp
      | otherwise = do
          let fp' = fp <> show counter
          b <- Dir.doesFileExist fp'
          if b
            then go (counter + 1)
            else pure fp'

handleLogSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  Path ->
  FilesSizeMode ->
  m ()
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
    FileSizeModeWarn warnSize ->
      when (totalBytes' > warnSize) $
        putTextLn $
          sizeWarning warnSize naviState totalBytes'
    FileSizeModeDelete delSize ->
      when (totalBytes' > delSize) $ do
        putTextLn $ sizeWarning delSize naviState totalBytes' <> " Deleting logs."
        Dir.removeDirectoryRecursive naviState
        Dir.createDirectoryIfMissing False naviState
  where
    sizeWarning warnSize fp fileSize =
      mconcat
        [ "Warning: log dir '",
          T.pack fp,
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
