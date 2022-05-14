{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Data.Functor.Identity (Identity (..))
import Katip
  ( ColorStrategy (..),
    Item,
    LogContexts,
    LogEnv (..),
    Severity (..),
    Verbosity (..),
  )
import Katip qualified as K
import Navi (runNavi, runNaviT)
import Navi.Args (Args (..), getArgs)
import Navi.Config (Config (..), LogLoc (..), Logging (..), NoteSystem (..), readConfig)
import Navi.Env.DBus (mkDBusEnv)
import Navi.Env.NotifySend (mkNotifySendEnv)
import Navi.Prelude
import System.Directory (XdgDirectory (XdgConfig))
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.IO qualified as IO

main :: IO ()
main = do
  args <- getArgs
  config <-
    tryParseConfig args
      `catchAny` writeConfigErr

  let mkLogEnvFn = mkLogEnv (config ^. #logging)
  bracket mkLogEnvFn K.closeScribes $ \logEnv -> do
    let mkNaviEnv :: forall env. _ -> IO env
        mkNaviEnv envFn = envFn logEnv logCtx "main" config
    case config ^. #noteSystem of
      DBus -> mkNaviEnv mkDBusEnv >>= runWithEnv
      NotifySend -> mkNaviEnv mkNotifySendEnv >>= runWithEnv
  where
    runWithEnv env = absurd <$> runNaviT runNavi env

tryParseConfig :: Args Identity -> IO (Config IORef)
tryParseConfig =
  readConfig
    . runIdentity
    . configFile

mkLogEnv :: Logging -> IO LogEnv
mkLogEnv logging = do
  let severityFn :: forall a. Item a -> IO Bool
      severityFn = K.permitItem severity'
  scribe <- case logLoc' of
    -- Use the default log path: xdgConfig </> navi/navi.log
    DefPath -> do
      xdgBase <- Dir.getXdgDirectory XdgConfig "navi/"
      let logFile = xdgBase </> "navi.log"
      renameIfExists logFile
      K.mkFileScribe logFile severityFn V2
    -- Custom log path.
    File f -> do
      renameIfExists f
      K.mkFileScribe f severityFn V2
    -- Log location defined in config file as stdout.
    Stdout -> K.mkHandleScribe ColorIfTerminal IO.stdout severityFn V2
  K.registerScribe "logger" scribe K.defaultScribeSettings =<< K.initLogEnv "navi" environment
  where
    environment = "production"
    severity' = fromMaybe ErrorS (logging ^. #severity)
    logLoc' = fromMaybe DefPath (logging ^. #location)

logCtx :: LogContexts
logCtx = K.liftPayload ()

writeConfigErr :: SomeException -> IO void
writeConfigErr ex = do
  xdgBase <- Dir.getXdgDirectory XdgConfig "navi/"
  let logFile = xdgBase </> "config_fatal.log"
  renameIfExists logFile
  writeFileUtf8 logFile $ "Couldn't read config: " <> pack (displayException ex)
  throwIO ex

renameIfExists :: FilePath -> IO ()
renameIfExists fp = do
  fileExists <- Dir.doesFileExist fp
  if fileExists
    then do
      fp' <- uniqName fp
      Dir.renameFile fp fp'
    else pure ()

uniqName :: FilePath -> IO FilePath
uniqName fp = go 1
  where
    go :: Word16 -> IO FilePath
    go !counter
      | counter == maxBound = Exit.die $ "Failed renaming file: " <> fp
      | otherwise = do
          let fp' = fp <> show counter
          b <- Dir.doesFileExist fp'
          if b
            then go (counter + 1)
            else pure fp'
