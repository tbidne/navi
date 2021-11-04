-- | Provides a \"shell\" effect.
module Navi.Effects.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent qualified as CC
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Prelude
import Navi.Services.Types (ServiceType (..))
import Smart.Data.Math.NonNegative (NonNegative (..))
import System.Info.Data (Command (..), QueryError (..))
import System.Info.Services.Battery.ChargeStatus qualified as SysInfo
import System.Info.Services.Battery.State qualified as SysInfo
import System.Info.Services.Network.Connection qualified as SysInfo
import System.Info.ShellApp (ShellApp (..))
import System.Info.ShellApp qualified as ShellApp
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  execSh :: ServiceType result -> m (Either QueryError result)
  readFile :: FilePath -> m (Either SomeNonPseudoException Text)
  sleep :: NonNegative Int -> m ()

instance MonadShell IO where
  execSh = execIO
  readFile = readFileIO
  sleep = CC.threadDelay . (*) 1_000_000 . unNonNegative

instance MonadShell m => MonadShell (ReaderT e m) where
  execSh = lift . execSh
  readFile = lift . readFile
  sleep = lift . sleep

readFileIO :: FilePath -> IO (Either SomeNonPseudoException Text)
readFileIO = (<<$>>) T.pack . UnexceptionalIO.fromIO . readFile'

execIO :: forall result. ServiceType result -> IO (Either QueryError result)
execIO = \case
  BatteryState bp -> SysInfo.queryBatteryState bp
  BatteryChargeStatus bp -> SysInfo.queryChargeStatus bp
  NetworkConnection cp -> SysInfo.queryConnection cp
  Single cmd trigger -> querySingle cmd trigger
  Multiple cmd triggers -> queryMultiple cmd triggers

queryMultiple :: Command -> [Text] -> IO (Either QueryError Text)
queryMultiple cmd triggers =
  let shellApp = multipleShellApp cmd triggers
   in ShellApp.runShellApp shellApp

multipleShellApp :: Command -> [Text] -> ShellApp Text
multipleShellApp cmd triggers =
  MkShellApp
    { command = cmd,
      parser = parseMultiple triggers
    }

parseMultiple :: [Text] -> Text -> Either QueryError Text
parseMultiple keys = first toEventErr . AP.parseOnly parseTxt
  where
    toEventErr = MkQueryError "Multiple" "Parse error" . T.pack
    parseTxt = AP.choice (fmap AP.string keys)

querySingle :: Command -> Text -> IO (Either QueryError Text)
querySingle cmd trigger =
  let shellApp = singleShellApp cmd trigger
   in ShellApp.runShellApp shellApp

singleShellApp :: Command -> Text -> ShellApp Text
singleShellApp cmd trigger =
  MkShellApp
    { command = cmd,
      parser = parseSingle trigger
    }

parseSingle :: Text -> Text -> Either QueryError Text
parseSingle trigger = first toEventErr . AP.parseOnly parseTxt
  where
    toEventErr = MkQueryError "Single" "Parse error" . T.pack
    parseTxt :: Parser Text
    parseTxt = AP.string trigger
