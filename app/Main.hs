{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Data.Functor.Identity (Identity (..))
import Effects.FileSystem.MonadPathReader qualified as Dir
import Effects.FileSystem.MonadPathWriter qualified as Dir
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Navi (runNavi, runNaviT)
import Navi.Args (Args (..), getArgs)
import Navi.Config (Config (..), LogLoc (..), Logging (..), NoteSystem (..), readConfig)
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
import System.Exit qualified as Exit

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)

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

tryParseConfig :: Args Identity -> IO Config
tryParseConfig =
  readConfig
    . runIdentity
    . view #configFile

mkLogEnv :: Logging -> IO LogEnv
mkLogEnv logging = do
  logFile <- case logLoc' of
    -- Use the default log path: xdgConfig </> navi/navi.log
    DefPath -> do
      xdgBase <- Dir.getXdgConfig "navi/"
      let logFile = xdgBase </> "navi.log"
      renameIfExists logFile
      h <- openBinaryFile logFile AppendMode
      pure $
        Just $
          MkLogFile
            { handle = h,
              finalizer = hFlush h `finally` hClose h
            }
    -- Custom log path.
    File f -> do
      renameIfExists f
      h <- openBinaryFile f AppendMode
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

writeConfigErr :: SomeException -> IO void
writeConfigErr ex = do
  xdgBase <- Dir.getXdgConfig "navi/"
  let logFile = xdgBase </> "config_fatal.log"
  renameIfExists logFile
  writeFileUtf8 logFile $ "Couldn't read config: " <> pack (displayException ex)
  throwWithCallStack ex

renameIfExists :: Path -> IO ()
renameIfExists fp = do
  fileExists <- Dir.doesFileExist fp
  when fileExists $ do
    fp' <- uniqName fp
    Dir.renameFile fp fp'

uniqName :: Path -> IO Path
uniqName fp = go 1
  where
    go :: Word16 -> IO Path
    go !counter
      | counter == maxBound = Exit.die $ "Failed renaming file: " <> fp
      | otherwise = do
          let fp' = fp <> show counter
          b <- Dir.doesFileExist fp'
          if b
            then go (counter + 1)
            else pure fp'
