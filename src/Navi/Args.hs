{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command-line arguments.
module Navi.Args
  ( Args (..),
    getArgs,
  )
where

import Control.Applicative qualified as A
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Classes (Show1 (..))
import Data.Functor.Classes qualified as Functor
import Data.Functor.Identity (Identity (..))
import Navi.Prelude
import Optics.TH qualified as O
import Options.Applicative (Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Dir

-- | Represents command-line arguments. We use the \"higher-kinded data\"
-- approach for:
--
-- 1. Parsing optional arguments (@'Args' 'Maybe'@).
-- 2. Filling missing arguments with defaults (@'Args' 'Identity'@).
data Args f = MkArgs
  { -- | Path to the configuration file.
    configFile :: f FilePath,
    -- | Path to the configuration directory. In addition to informing
    -- on 'configFile', this is used to determine where the log file is
    -- written.
    configDir :: f FilePath
  }

O.makeFieldLabelsNoPrefix ''Args

instance (Show1 f) => Show (Args f) where
  show MkArgs {configFile, configDir} =
    "MkArgs "
      <> Functor.showsPrec1 9 configFile " "
      <> Functor.showsPrec1 9 configDir ""

-- |
--
-- Parses cli args and fills in defaults. These defaults are based on the
-- detected XDG Base Directory. The semantics are:
--
-- * No arguments provided.
--
--     * configFile:  xdgBase/config.toml
--     * configDir: xdgBase
--
-- * configFile' provided.
--
--     * configFile: configFile'
--     * configDir: xdgBase
--
-- * configDir' provided.
--
--     * configFile: configDir'/config.toml
--     * configDir: configDir'
--
-- * configFile' and configDir' provided.
--
--     * configFile: configFile'
--     * configDir: configDir'
getArgs :: MonadIO m => m (Args Identity)
getArgs = liftIO $ do
  args <- OptApp.execParser parserInfoArgs
  fillMissingDefaults args

fillMissingDefaults :: Args Maybe -> IO (Args Identity)
fillMissingDefaults MkArgs {configFile, configDir} = do
  (configFile', configDir') <- case (configFile, configDir) of
    -- No custom paths provided, use defaults
    (Nothing, Nothing) -> do
      xdgBase <- defaultXdg
      pure (xdgBase <> "config.toml", xdgBase)
    -- Custom file provided, override default file
    (Just customFile, Nothing) -> do
      xdgBase <- defaultXdg
      pure (customFile, xdgBase)
    -- Custom dir provided, override dir and file location
    (Nothing, Just customDir) -> pure (customDir <> "config.toml", customDir)
    -- Custom file and dir provided, override both
    (Just customFile, Just customDir) -> pure (customFile, customDir)

  pure $
    MkArgs
      { configFile = Identity configFile',
        configDir = Identity configDir'
      }
  where
    defaultXdg = Dir.getXdgDirectory XdgConfig "navi/"

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo (Args Maybe)
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk Nothing,
      infoHeader = Chunk Nothing,
      infoFooter = Chunk Nothing,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }

argsParser :: Parser (Args Maybe)
argsParser =
  MkArgs
    <$> configFileParser
    <*> configDirParser
      <**> OptApp.helper

configFileParser :: Parser (Maybe String)
configFileParser =
  A.optional
    ( OptApp.strOption
        ( OptApp.long "config-file"
            <> OptApp.short 'f'
            <> OptApp.help helpTxt
            <> OptApp.metavar "PATH"
        )
    )
  where
    helpTxt =
      "Path to config file. Overrides default "
        <> " <config-dir>/config.toml if <config-dir> is specified."

configDirParser :: Parser (Maybe String)
configDirParser =
  A.optional
    ( OptApp.strOption
        ( OptApp.long "config-dir"
            <> OptApp.short 'd'
            <> OptApp.help helpTxt
            <> OptApp.metavar "PATH"
        )
    )
  where
    helpTxt =
      "Path to config directory. Determines where we look for "
        <> " config.toml and output log file."
