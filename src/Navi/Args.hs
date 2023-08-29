{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command-line arguments.
module Navi.Args
  ( Args (..),
    getArgs,
  )
where

import Control.Applicative qualified as A
import Data.Functor.Classes (Show1)
import Data.Functor.Classes qualified as Functor
import Data.Functor.Identity (Identity (Identity))
import Data.List qualified as L
import Data.Version (Version (versionBranch))
import Effectful.FileSystem.PathReader.Dynamic qualified as Dir
import Effectful.FileSystem.Utils (osp)
import Effectful.Optparse.Static (OptparseStatic, execParser, osPath)
import Navi.Prelude
import Options.Applicative
  ( Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_navi qualified as Paths

-- | Represents command-line arguments. We use the \"higher-kinded data\"
-- approach for:
--
-- 1. Parsing optional arguments (@'Args' 'Maybe'@).
-- 2. Filling missing arguments with defaults (@'Args' 'Identity'@).
newtype Args f = MkArgs
  { -- | Path to the configuration file.
    configFile :: f OsPath
  }

makeFieldLabelsNoPrefix ''Args

instance (Show1 f) => Show (Args f) where
  show MkArgs {configFile} =
    "MkArgs "
      <> Functor.showsPrec1 9 configFile " "

-- | Parses cli args and fills in defaults. These defaults are based on the
-- detected XDG Base Directory and default names.
getArgs ::
  (OptparseStatic :> es, PathReaderDynamic :> es) =>
  Eff es (Args Identity)
getArgs = do
  args <- execParser parserInfoArgs
  fillMissingDefaults args
{-# INLINEABLE getArgs #-}

fillMissingDefaults ::
  (PathReaderDynamic :> es) =>
  Args Maybe ->
  Eff es (Args Identity)
fillMissingDefaults args = do
  configFile' <- case configFile of
    -- No custom paths provided, use default
    Nothing -> do
      xdgBase <- Dir.getXdgConfig [osp|navi|]
      let defConfigName = [osp|config.toml|]
      pure (xdgBase </> defConfigName)
    -- Custom config provided, override
    Just customFile -> pure customFile
  pure
    $ MkArgs
      { configFile = Identity configFile'
      }
  where
    configFile = args ^. #configFile

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo (Args Maybe)
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header =
      Just
        $ "Navi: A program for monitoring system status via "
        <> "desktop notifications."
    footer = Just $ fromString versNum
    desc =
      Chunk.paragraph
        $ "Navi allows one to easily define custom notification 'services'"
        <> " that hook into a running notification server. For example, one"
        <> " can provide a bash script that, say, queries the connection"
        <> " status of a given network device. Navi will periodically run"
        <> " this query and send a desktop notification if the status has"
        <> " changed. See github.com/tbidne/navi#README for full"
        <> " documentation."

argsParser :: Parser (Args Maybe)
argsParser =
  MkArgs
    <$> configFileParser
    <**> OA.helper
    <**> version

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')

versNum :: [Char]
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

configFileParser :: Parser (Maybe OsPath)
configFileParser =
  A.optional
    ( OA.option
        osPath
        ( OA.long "config-file"
            <> OA.short 'c'
            <> mkHelp helpTxt
            <> OA.metavar "PATH"
        )
    )
  where
    helpTxt =
      "Path to config file. Defaults to <xdg-config>/navi/config.toml."

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
