{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception qualified as Except
import Control.Monad.Reader (ReaderT (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Data.Void (absurd)
import Development.GitRev qualified as GitRev
import Katip
  ( ColorStrategy (..),
    Item,
    LogContexts,
    LogEnv (..),
    Namespace (..),
    Severity (..),
    Verbosity (..),
  )
import Katip qualified as K
import Navi (runNavi, runNaviT)
import Navi.Args (Args (..), getArgs)
import Navi.Config (Config (..), LogLoc (..), Logging (..), readConfig)
import Navi.Effects (MonadMutRef (..))
import Navi.Env (mkEnv)
import Navi.Prelude
import Optics.Operators ((^.))
import Optics.Optic ((%))
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.IO qualified as IO

main :: IO ()
main = do
  args <- getArgs

  if args ^. #displayVersion
    then putStrLn versionTxt *> Exit.exitSuccess
    else pure ()

  config <- tryOrDie =<< tryParseConfig @IORef args

  let mkLogEnvFn = mkLogEnv (args ^. #configDir % #runIdentity) (config ^. #logging)
  Except.bracket mkLogEnvFn K.closeScribes $ \logEnv -> do
    env <- tryOrDie =<< mkEnv logEnv logCtx namespace config
    absurd <$> runReaderT (runNaviT (runNavi @IORef)) env
  where
    tryOrDie = either (Exit.die . T.unpack) pure

tryParseConfig :: MonadMutRef IO ref => Args Identity -> IO (Either Text (Config ref))
tryParseConfig =
  fmap (first mkErr)
    . readConfig
    . runIdentity
    . configFile
  where
    mkErr = (<>) "Config error: " . showt

mkLogEnv :: FilePath -> Logging -> IO LogEnv
mkLogEnv dir MkLogging {severity, location} = do
  let severityFn :: forall a. Item a -> IO Bool
      severityFn = maybe (K.permitItem ErrorS) K.permitItem severity
  scribe <- case location of
    Nothing -> do
      let path = dir <> "navi.log"
      delIfExist path
      K.mkFileScribe path severityFn V2
    Just (File f) -> do
      delIfExist f
      K.mkFileScribe f severityFn V2
    Just Stdout -> K.mkHandleScribe ColorIfTerminal IO.stdout severityFn V2
  K.registerScribe "logger" scribe K.defaultScribeSettings =<< K.initLogEnv namespace environment
  where
    environment = "production"
    delIfExist fp = do
      fileExists <- Dir.doesFileExist fp
      if fileExists
        then Dir.removeFile fp
        else pure ()

namespace :: Namespace
namespace = "navi"

logCtx :: LogContexts
logCtx = K.liftPayload ()

versionTxt :: Text
versionTxt =
  T.pack $
    L.intercalate
      "\n"
      [ "Navi",
        "Version: " <> $$(PV.packageVersionStringTH "navi.cabal"),
        "Revision: " <> $(GitRev.gitHash),
        "Date: " <> $(GitRev.gitCommitDate)
      ]
