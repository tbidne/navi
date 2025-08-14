{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Navi.Runner
  ( makeEnvAndRun,
    withEnv,
  )
where

import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter (MonadHandleWriter (withBinaryFile), die)
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter (MonadPathWriter)
import Effects.FileSystem.PathWriter qualified as Dir
import Effects.Time (MonadTime)
import Effects.Time qualified as Time
import FileSystem.OsPath (encodeThrowM, encodeValidThrowM)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Navi (NaviT, runNavi, runNaviT)
import Navi.Args (Args, getArgs)
import Navi.Config
  ( Config,
    LogLoc (DefPath, File, Stdout),
    Logging,
    NoteSystem (AppleScript, DBus, NotifySend),
    readConfig,
  )
import Navi.Config.Types
  ( FilesSizeMode
      ( FilesSizeModeDelete,
        FilesSizeModeWarn
      ),
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
import Navi.Effects (MonadSystemInfo)
import Navi.Effects.MonadNotify (MonadNotify)
import Navi.Env.AppleScript (mkAppleScriptEnv)
import Navi.Env.Core (Env)
import Navi.Env.DBus (MonadDBus, mkDBusEnv)
import Navi.Env.NotifySend (mkNotifySendEnv)
import Navi.Prelude

{- ORMOLU_DISABLE -}

makeEnvAndRun ::
  forall m.
  ( HasCallStack,
    MonadAsync m,
    MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadSystemInfo m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m,
    MonadTypedProcess m
  ) =>
  m ()
makeEnvAndRun = withEnv runWithEnv
  where
    runWithEnv env = absurd <$> runNaviT runNavi env

withEnv ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m,
    MonadTime m
  ) =>
  (Env -> m a) ->
  m a
withEnv onEnv = do
  args <- getArgs
  config <-
    tryParseConfig args
      `catchSync` writeConfigErr

  withLogEnv (config ^. #logging) $ \logEnv -> do
    let mkNaviEnv :: (Maybe LogEnv -> Config -> m env) -> m env
        mkNaviEnv envFn = envFn logEnv config
    case config ^. #noteSystem of
#if OSX
      AppleScript -> mkNaviEnv mkAppleScriptEnv >>= onEnv
      DBus () -> throwText "Detected osx, but DBus is only available on linux!"
      NotifySend -> throwText "Detected osx, but NotifySend is only available on linux!"
#else
      AppleScript -> throwText "Detected linux, but AppleScript is only available on osx!"
      DBus () -> mkNaviEnv mkDBusEnv >>= onEnv
      NotifySend -> mkNaviEnv mkNotifySendEnv >>= onEnv
#endif
  --where
  --  runWithEnv env = absurd <$> runNaviT runNavi env

{- ORMOLU_ENABLE -}

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
    MonadSTM m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  Logging ->
  (Maybe LogEnv -> m a) ->
  m a
withLogEnv logging onLogEnv = do
  case logging ^. #severity of
    Nothing -> onLogEnv Nothing
    Just logLevel -> do
      logQueue <- newTBQueueA 1000
      withLogHandle logging $ \logHandle ->
        onLogEnv
          $ Just
          $ MkLogEnv
            { logHandle,
              logLevel,
              logNamespace = "main",
              logQueue
            }

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
      xdgState <- Dir.getXdgState [osp|navi|]

      -- handle large log dir
      handleLogSize xdgState sizeMode

      currTimeOs <-
        encodeValidThrowM
          . fmap replaceSpc
          =<< Time.getSystemTimeString
      let logFile = xdgState </> currTimeOs <> [osp|.log|]
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
  writeFileUtf8 logFile $ "Couldn't read config: " <> displayExceptiont ex
  throwM ex

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
          fp' <- (fp <>) <$> encodeThrowM (show counter)
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
          showt fp,
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
