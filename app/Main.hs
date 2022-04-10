module Main (main) where

import Control.Exception qualified as Except
import Control.Monad.Reader (ReaderT (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef)
import Data.Void (absurd)
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
import Navi.Env (mkEnv)
import Navi.Prelude
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO qualified as IO

main :: IO ()
main = do
  args <- getArgs
  config <- tryParseConfig args

  let mkLogEnvFn = mkLogEnv (args ^. #configDir % #runIdentity) (config ^. #logging)
  Except.bracket mkLogEnvFn K.closeScribes $ \logEnv -> do
    env <- mkEnv logEnv logCtx namespace config
    absurd <$> runReaderT (runNaviT (runNavi @IORef)) env

tryParseConfig :: Args Identity -> IO (Config IORef)
tryParseConfig =
  readConfig
    . runIdentity
    . configFile

mkLogEnv :: FilePath -> Logging -> IO LogEnv
mkLogEnv dir MkLogging {severity, location} = do
  let severityFn :: forall a. Item a -> IO Bool
      severityFn = maybe (K.permitItem ErrorS) K.permitItem severity
  scribe <- case location of
    Nothing -> do
      let path = dir </> "navi.log"
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
