-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Control.Monad.Reader (MonadTrans (..), ReaderT)
import Data.Attoparsec.Combinator qualified as AP
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
    Single cmd trigger -> querySingle cmd trigger
    Multiple cmd triggers -> queryMultiple cmd triggers

instance MonadSystemInfo m => MonadSystemInfo (ReaderT e m) where
  query = lift . query

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
