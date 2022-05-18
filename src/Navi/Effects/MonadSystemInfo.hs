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
import Pythia.Internal.ShellApp (SimpleShell (..))
import Pythia.Internal.ShellApp qualified as ShellApp

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
    NetworkInterface device cp ->
      rethrowPythia "NetInterface" $ Pythia.queryNetInterface device cp
    Single cmd -> querySingle cmd
    Multiple cmd -> queryMultiple cmd
  {-# INLINEABLE query #-}

rethrowPythia :: Text -> IO a -> IO a
rethrowPythia n io =
  io `catch` \(e :: PythiaException) ->
    throwM $
      MkEventError
        { name = n,
          short = "PythiaException",
          long = pack $ displayException e
        }
{-# INLINEABLE rethrowPythia #-}

instance MonadSystemInfo m => MonadSystemInfo (ReaderT e m) where
  query = lift . query
  {-# INLINEABLE query #-}

queryMultiple :: Command -> IO Text
queryMultiple cmd =
  let shellApp = multipleShellApp cmd
   in T.strip <$> ShellApp.runSimple shellApp
{-# INLINEABLE queryMultiple #-}

multipleShellApp :: Command -> SimpleShell EventError Text
multipleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      parser = parseMultiple,
      liftShellEx = liftEventError "Multiple"
    }
{-# INLINEABLE multipleShellApp #-}

parseMultiple :: Text -> Either EventError Text
parseMultiple = Right
{-# INLINEABLE parseMultiple #-}

querySingle :: Command -> IO Text
querySingle cmd = do
  let shellApp = singleShellApp cmd
   in T.strip <$> ShellApp.runSimple shellApp
{-# INLINEABLE querySingle #-}

singleShellApp :: Command -> SimpleShell EventError Text
singleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      parser = parseSingle,
      liftShellEx = liftEventError "Single"
    }
{-# INLINEABLE singleShellApp #-}

parseSingle :: Text -> Either EventError Text
parseSingle = Right
{-# INLINEABLE parseSingle #-}

liftEventError :: Exception e => Text -> e -> EventError
liftEventError n e =
  MkEventError
    { name = n,
      short = "Command error",
      long = pack $ displayException e
    }
{-# INLINEABLE liftEventError #-}
