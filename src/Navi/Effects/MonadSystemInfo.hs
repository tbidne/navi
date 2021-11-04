-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Control.Monad.Reader (MonadTrans (..), ReaderT)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Prelude
import Navi.Services.Types (ServiceType (..))
import System.Info.Data (Command (..), QueryError (..))
import System.Info.Services.Battery.ChargeStatus qualified as SysInfo
import System.Info.Services.Battery.State qualified as SysInfo
import System.Info.Services.Network.Connection qualified as SysInfo
import System.Info.ShellApp (ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | This class represents an effect of querying system information.
class Monad m => MonadSystemInfo m where
  query :: ServiceType result -> m (Either QueryError result)

instance MonadSystemInfo IO where
  query = \case
    BatteryState bp -> SysInfo.queryBatteryState bp
    BatteryChargeStatus bp -> SysInfo.queryChargeStatus bp
    NetworkConnection cp -> SysInfo.queryConnection cp
    Single cmd -> querySingle cmd
    Multiple cmd -> queryMultiple cmd

instance MonadSystemInfo m => MonadSystemInfo (ReaderT e m) where
  query = lift . query

queryMultiple :: Command -> IO (Either QueryError Text)
queryMultiple cmd =
  let shellApp = multipleShellApp cmd
   in ShellApp.runShellApp shellApp

multipleShellApp :: Command -> ShellApp Text
multipleShellApp cmd =
  MkShellApp
    { command = cmd,
      parser = parseMultiple
    }

parseMultiple :: Text -> Either QueryError Text
parseMultiple = first toEventErr . AP.parseOnly parseTxt
  where
    toEventErr = MkQueryError "Multiple" "Parse error" . T.pack
    parseTxt = AP.takeText

querySingle :: Command -> IO (Either QueryError Text)
querySingle cmd =
  let shellApp = singleShellApp cmd
   in ShellApp.runShellApp shellApp

singleShellApp :: Command -> ShellApp Text
singleShellApp cmd =
  MkShellApp
    { command = cmd,
      parser = parseSingle
    }

parseSingle :: Text -> Either QueryError Text
parseSingle result = first toEventErr $ AP.parseOnly parseTxt result
  where
    toEventErr err =
      MkQueryError
        "Single"
        "Parse error"
        ("Error parsing `" <> result <> "`: " <> T.pack err)
    parseTxt :: Parser Text
    parseTxt = AP.takeText
