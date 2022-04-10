module Main (main) where

import Data.Functor.Identity (Identity (..))
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
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.IO qualified as IO

main :: IO ()
main = do
  args <- getArgs
  config <- tryParseConfig args

  let mkLogEnvFn = mkLogEnv (args ^. #configDir % #runIdentity) (config ^. #logging)
  bracket mkLogEnvFn K.closeScribes $ \logEnv -> do
    env <- mkEnv logEnv logCtx namespace config
    absurd <$> runNaviT runNavi env

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
      renameIfExists path
      K.mkFileScribe path severityFn V2
    Just (File f) -> do
      renameIfExists f
      K.mkFileScribe f severityFn V2
    Just Stdout -> K.mkHandleScribe ColorIfTerminal IO.stdout severityFn V2
  K.registerScribe "logger" scribe K.defaultScribeSettings =<< K.initLogEnv namespace environment
  where
    environment = "production"

namespace :: Namespace
namespace = "navi"

logCtx :: LogContexts
logCtx = K.liftPayload ()

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
