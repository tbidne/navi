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
import Pythia.Data (Command (..), QueryError (..))
import Pythia.Services.Battery.ChargeStatus qualified as Pythia
import Pythia.Services.Battery.State qualified as Pythia
import Pythia.Services.Network.Connection qualified as Pythia
import Pythia.ShellApp (ShellApp (..), SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp

-- | This class represents an effect of querying system information.
class Monad m => MonadSystemInfo m where
  query :: ServiceType result -> m (Either [QueryError] result)

instance MonadSystemInfo IO where
  query = \case
    BatteryState bp -> Pythia.queryBatteryState bp
    BatteryChargeStatus bp -> Pythia.queryChargeStatus bp
    NetworkConnection cp -> Pythia.queryConnection cp
    Single cmd -> querySingle cmd
    Multiple cmd -> queryMultiple cmd

instance MonadSystemInfo m => MonadSystemInfo (ReaderT e m) where
  query = lift . query

queryMultiple :: Command -> IO (Either [QueryError] Text)
queryMultiple cmd =
  let shellApp = multipleShellApp cmd
   in ShellApp.runShellApp shellApp

multipleShellApp :: Command -> ShellApp Text
multipleShellApp cmd =
  SimpleApp $
    MkSimpleShell
      { command = cmd,
        parser = parseMultiple
      }

parseMultiple :: Text -> Either QueryError Text
parseMultiple = first toEventErr . AP.parseOnly parseTxt
  where
    toEventErr = MkQueryError "Multiple" "Parse error" . T.pack
    parseTxt = AP.takeText

querySingle :: Command -> IO (Either [QueryError] Text)
querySingle cmd = do
  let shellApp = singleShellApp cmd
   in ShellApp.runShellApp shellApp

singleShellApp :: Command -> ShellApp Text
singleShellApp cmd =
  SimpleApp $
    MkSimpleShell
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
