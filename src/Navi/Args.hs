{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command-line arguments.
module Navi.Args
  ( Args (..),
    getArgs,
  )
where

import Control.Applicative qualified as A
import Data.Functor.Classes (Show1 (..))
import Data.Functor.Classes qualified as Functor
import Data.Functor.Identity (Identity (..))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Navi.Prelude
import Options.Applicative (Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | Represents command-line arguments. We use the \"higher-kinded data\"
-- approach for:
--
-- 1. Parsing optional arguments (@'Args' 'Maybe'@).
-- 2. Filling missing arguments with defaults (@'Args' 'Identity'@).
newtype Args f = MkArgs
  { -- | Path to the configuration file.
    configFile :: f FilePath
  }

makeFieldLabelsNoPrefix ''Args

instance (Show1 f) => Show (Args f) where
  show MkArgs {configFile} =
    "MkArgs "
      <> Functor.showsPrec1 9 configFile " "

-- | Parses cli args and fills in defaults. These defaults are based on the
-- detected XDG Base Directory and default names.
getArgs :: MonadIO m => m (Args Identity)
getArgs = liftIO $ do
  args <- OptApp.execParser parserInfoArgs
  fillMissingDefaults args

fillMissingDefaults :: Args Maybe -> IO (Args Identity)
fillMissingDefaults args = do
  configFile' <- case configFile of
    -- No custom paths provided, use default
    Nothing -> do
      xdgBase <- defaultXdg
      pure (xdgBase </> defConfigName)
    -- Custom config provided, override
    Just customFile -> pure customFile
  pure $
    MkArgs
      { configFile = Identity configFile'
      }
  where
    configFile = args ^. #configFile
    defaultXdg = Dir.getXdgDirectory XdgConfig "navi/"
    defConfigName = "config.toml"

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
      <**> OptApp.helper
      <**> version

version :: Parser (a -> a)
version = OptApp.infoOption txt (OptApp.long "version" <> OptApp.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "Pythia",
          "Version: " <> $$(PV.packageVersionStringTH "navi.cabal"),
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

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
      "Path to config file. Defaults to <xdgConfig>/navi/config.toml."
