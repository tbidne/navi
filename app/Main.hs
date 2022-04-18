module Main (main) where

import Data.Functor.Identity (Identity (..))
import Katip
  ( ColorStrategy (..),
    Item,
    LogContexts,
    LogEnv (..),
    Namespace (..),
    Verbosity (..),
  )
import Katip qualified as K
import Navi (NotifySystem (..), runNavi, runNaviT)
import Navi.Args (Args (..), getArgs)
import Navi.Config (Config (..), LogLoc (..), Logging (..), readConfig)
import Navi.Env.DBus (mkDBusEnv)
import Navi.Prelude
import System.Directory (XdgDirectory (XdgConfig))
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.IO qualified as IO

main :: IO ()
main = do
  args <- getArgs
  config <- tryParseConfig args

  let mkLogEnvFn = mkLogEnv (config ^. #logging)
  bracket mkLogEnvFn K.closeScribes $ \logEnv -> do
    env <- mkDBusEnv logEnv logCtx namespace config
    absurd <$> (runNaviT @'DBus) runNavi env

tryParseConfig :: Args Identity -> IO (Config IORef)
tryParseConfig =
  readConfig
    . runIdentity
    . configFile

mkLogEnv :: Logging -> IO LogEnv
mkLogEnv MkLogging {severity, location} = do
  let severityFn :: forall a. Item a -> IO Bool
      severityFn = K.permitItem severity
  scribe <- case location of
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
