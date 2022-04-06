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
import Pythia.Data.Command (Command (..))
import Pythia.Services.Battery qualified as Pythia
import Pythia.Services.NetInterface qualified as Pythia
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp

-- | This class represents an effect of querying system information.
class Monad m => MonadSystemInfo m where
  query :: ServiceType result -> m result

instance MonadSystemInfo IO where
  query :: ServiceType result -> IO result
  query = \case
    BatteryPercentage bp -> Pythia.queryBatteryConfig bp
    BatteryStatus bp ->
      view #status <$> Pythia.queryBatteryConfig bp
    NetworkInterface cp -> do
      ifs <-
        view #unNetInterfaces <$> Pythia.queryNetInterfacesConfig cp

      let d = cp ^. #interfaceDevice
          dName = show d
      case headMaybe ifs of
        Nothing ->
          throw $
            MkEventErr
              "NetInterfaces"
              "Query Error"
              ("No device found matching: " <> showt dName)
        Just i -> pure i
    Single cmd -> querySingle cmd
    Multiple cmd -> queryMultiple cmd

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
