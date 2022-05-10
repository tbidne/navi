-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Data.Text qualified as T
import Navi.Event.Types (EventError (..))
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

rethrowPythia :: MonadUnliftIO m => Text -> m a -> m a
rethrowPythia n io =
  io `catch` \(e :: PythiaException) ->
    throwIO $
      MkEventError
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

multipleShellApp :: Command -> SimpleShell EventError Text
multipleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      parser = parseMultiple,
      liftShellEx = liftEventError "Multiple"
    }

parseMultiple :: Text -> Either EventError Text
parseMultiple = Right

querySingle :: Command -> IO Text
querySingle cmd = do
  let shellApp = singleShellApp cmd
   in ShellApp.runSimple shellApp

singleShellApp :: Command -> SimpleShell EventError Text
singleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      parser = parseSingle,
      liftShellEx = liftEventError "Single"
    }

parseSingle :: Text -> Either EventError Text
parseSingle = Right

liftEventError :: Exception e => Text -> e -> EventError
liftEventError n e =
  MkEventError
    { name = n,
      short = "Command error",
      long = T.pack $ displayException e
    }
