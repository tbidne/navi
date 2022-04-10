-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Control.Monad.Reader (MonadTrans (..), ReaderT)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Event.Types (EventErr (..))
import Navi.Prelude
import Navi.Services.Types (ServiceType (..))
import Pythia (PythiaException (..))
import Pythia qualified
import Pythia.Data.Command (Command (..))
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp

-- | This class represents an effect of querying system information.
class Monad m => MonadSystemInfo m where
  query :: ServiceType result -> m result

instance MonadSystemInfo IO where
  query :: ServiceType result -> IO result
  query = \case
    BatteryPercentage bp ->
      rethrowPythia "Battery Percentage" $ Pythia.queryBattery bp
    BatteryStatus bp ->
      rethrowPythia "Battery Status" $ view #status <$> Pythia.queryBattery bp
    NetworkInterface device cp -> rethrowPythia "NetInterface" $ do
      rethrowPythia "NetInterface" $ Pythia.queryNetInterface device cp
    Single cmd -> querySingle cmd
    Multiple cmd -> queryMultiple cmd

rethrowPythia :: MonadCatch m => Text -> m a -> m a
rethrowPythia n io =
  io `catch` \(e :: PythiaException) ->
    throw $
      MkEventErr
        { name = n,
          short = "PythiaException",
          long = T.pack $ displayException e
        }

instance MonadSystemInfo m => MonadSystemInfo (ReaderT e m) where
  query = lift . query

queryMultiple :: Command -> IO Text
queryMultiple cmd =
  let shellApp = multipleShellApp cmd
   in ShellApp.runSimple shellApp

multipleShellApp :: Command -> SimpleShell EventErr Text
multipleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      parser = parseMultiple,
      liftShellEx = liftEventErr "Multiple"
    }

parseMultiple :: Text -> Either EventErr Text
parseMultiple result = first toEventErr $ AP.parseOnly parseTxt result
  where
    toEventErr err =
      MkEventErr
        "Multiple"
        "Parse error"
        ("Error parsing `" <> result <> "`: <" <> T.pack err <> ">")
    parseTxt = AP.takeText

querySingle :: Command -> IO Text
querySingle cmd = do
  let shellApp = singleShellApp cmd
   in ShellApp.runSimple shellApp

singleShellApp :: Command -> SimpleShell EventErr Text
singleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      parser = parseSingle,
      liftShellEx = liftEventErr "Single"
    }

parseSingle :: Text -> Either EventErr Text
parseSingle result = first toEventErr $ AP.parseOnly parseTxt result
  where
    toEventErr err =
      MkEventErr
        "Single"
        "Parse error"
        ("Error parsing `" <> result <> "`: <" <> T.pack err <> ">")
    parseTxt :: Parser Text
    parseTxt = AP.takeText

liftEventErr :: Exception e => Text -> e -> EventErr
liftEventErr n e =
  MkEventErr
    { name = n,
      short = "Command error",
      long = T.pack $ displayException e
    }
