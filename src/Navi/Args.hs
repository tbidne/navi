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
  {-# INLINEABLE show #-}

-- | Parses cli args and fills in defaults. These defaults are based on the
-- detected XDG Base Directory and default names.
getArgs :: MonadIO m => m (Args Identity)
getArgs = liftIO $ do
  args <- OptApp.execParser parserInfoArgs
  fillMissingDefaults args
{-# INLINEABLE getArgs #-}

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
    defConfigName = "navi-config.toml"
{-# INLINEABLE fillMissingDefaults #-}

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo (Args Maybe)
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header =
      Just $
        "Navi: A program for monitoring system status via "
          <> "desktop notifications."
    footer = Just $ fromString versNum
    desc =
      Just $
        "\nNavi allows one to easily define custom notification 'services'"
          <> " that hook into a running notification server. For example, one"
          <> " can provide a bash script that, say, queries the connection"
          <> " status of a given network device. Navi will periodically run"
          <> " this query and send a desktop notification if the status has"
          <> " changed. See github.com/tbidne/navi#README for full"
          <> " documentation."
{-# INLINEABLE parserInfoArgs #-}

argsParser :: Parser (Args Maybe)
argsParser =
  MkArgs
    <$> configFileParser
    <**> OptApp.helper
    <**> version
{-# INLINEABLE argsParser #-}

version :: Parser (a -> a)
version = OptApp.infoOption txt (OptApp.long "version" <> OptApp.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "Pythia",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]
{-# INLINEABLE version #-}

versNum :: [Char]
versNum = "Version: " <> $$(PV.packageVersionStringTH "navi.cabal")
{-# INLINEABLE versNum #-}

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
      "Path to config file. Defaults to <xdgConfig>/navi/navi-config.toml."
{-# INLINEABLE configFileParser #-}
