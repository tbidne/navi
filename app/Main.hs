{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (..))
import Data.Functor.Identity (Identity (..))
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter (MonadHandleWriter (withBinaryFile), die)
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter (MonadPathWriter)
import Effects.FileSystem.PathWriter qualified as Dir
import Effects.FileSystem.Utils (encodeFpToOsThrowM, osp, (<</>>!))
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
import Navi.Data.NaviLog (LogEnv (..))
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

  withLogEnv (config ^. #logging) $ \logEnv -> do
    let mkNaviEnv :: (LogEnv -> Config -> IO env) -> IO env
        mkNaviEnv envFn = envFn logEnv config
    case config ^. #noteSystem of
      DBus -> mkNaviEnv mkDBusEnv >>= runWithEnv
      NotifySend -> mkNaviEnv mkNotifySendEnv >>= runWithEnv
  where
    runWithEnv env = absurd <$> runNaviT runNavi env

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

withLogEnv ::
  ( HasCallStack,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  Logging ->
  (LogEnv -> m a) ->
  m a
withLogEnv logging onLogEnv =
  withLogHandle logging $ \logHandle ->
    onLogEnv
      $ MkLogEnv
        { logHandle,
          logLevel,
          logNamespace = "main"
        }
  where
    logLevel = fromMaybe LevelError (logging ^. #severity)

withLogHandle ::
  ( HasCallStack,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  Logging ->
  (Maybe Handle -> m a) ->
  m a
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
  xdgBase <- Dir.getXdgState [osp|navi|]
  let logFile = xdgBase </> [osp|config_fatal.log|]
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
  OsPath ->
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
  OsPath ->
  m OsPath
uniqName fp = go 1
  where
    go :: Word16 -> m OsPath
    go !counter
      | counter == maxBound = die $ "Failed renaming file: " <> show fp
      | otherwise = do
          fp' <- (fp <>) <$> encodeFpToOsThrowM (show counter)
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
  OsPath ->
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
