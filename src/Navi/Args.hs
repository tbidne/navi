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
import Data.Version (showVersion)
import Effects.FileSystem.PathReader qualified as Dir
import Effects.Optparse (execParser, osPath)
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as OsString
import Navi.Args.TH qualified as TH
import Navi.Prelude
import Options.Applicative (Parser, ParserInfo (ParserInfo))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_navi qualified as Paths
import System.Info qualified as Info

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

makeFieldLabelsNoPrefix ''VersionInfo

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
  ( HasCallStack,
    MonadOptparse m,
    MonadPathReader m
  ) =>
  m (Args Identity)
getArgs = do
  args <- execParser parserInfoArgs
  fillMissingDefaults args
{-# INLINEABLE getArgs #-}

fillMissingDefaults ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Args Maybe -> m (Args Identity)
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
    footer = Just $ fromString versShort
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
    <**> OptApp.helper
    <**> version

version :: Parser (a -> a)
version = OptApp.infoOption versLong (OptApp.long "version" <> OptApp.short 'v')

versShort :: String
versShort =
  mconcat
    [ "Version: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient $ versionInfo ^. #gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Navi: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient (versionInfo ^. #gitHash),
      " - Commit date:  " <> OsString.decodeLenient (versionInfo ^. #gitCommitDate),
      " - GHC version:  " <> versionInfo ^. #ghc
    ]

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.compilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

configFileParser :: Parser (Maybe OsPath)
configFileParser =
  A.optional
    ( OptApp.option
        osPath
        ( OptApp.long "config-file"
            <> OptApp.short 'c'
            <> mkHelp helpTxt
            <> OptApp.metavar "PATH"
        )
    )
  where
    helpTxt =
      "Path to config file. Defaults to <xdg-config>/navi/config.toml."

mkHelp :: String -> OptApp.Mod f a
mkHelp =
  OptApp.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
