{-# LANGUAGE CPP #-}

-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Data.Text qualified as T
import Navi.Event.Types (EventError (MkEventError, long, name, short))
import Navi.Prelude
import Navi.Services.Types
  ( ServiceType
      ( BatteryPercentage,
        BatteryStatus,
        Multiple,
        NetworkInterface,
        Single
      ),
  )
import Navi.Utils qualified as U
import Pythia qualified
import Pythia.Data.Command (Command)
import Pythia.Internal.ShellApp
  ( SimpleShell
      ( MkSimpleShell,
        command,
        isSupported,
        parser
      ),
  )
import Pythia.Internal.ShellApp qualified as ShellApp

{- HLINT ignore MonadSystemInfo "Redundant bracket" -}

-- | This class represents an effect of querying system information.
class (Monad m) => MonadSystemInfo m where
  query :: (HasCallStack) => ServiceType result -> m result

instance MonadSystemInfo IO where
  query :: ServiceType result -> IO result
  query = \case
    BatteryPercentage bp ->
      rethrowPythia "Battery Percentage" $ Pythia.queryBattery bp
    BatteryStatus bp ->
      rethrowPythia "Battery Status" $ view #status <$> Pythia.queryBattery bp
    NetworkInterface device cp ->
      rethrowPythia "NetInterface" $ Pythia.queryNetInterface device cp
    Single cmd -> rethrowPythia "Single" $ querySingle cmd
    Multiple cmd -> rethrowPythia "Multiple" $ queryMultiple cmd

rethrowPythia :: Text -> IO a -> IO a
rethrowPythia n io =
  io `catchSync` \e ->
    throwM
      $ MkEventError
        { name = n,
          short = "PythiaException",
          long = pack $ U.displayInner e
        }

instance (MonadSystemInfo m) => MonadSystemInfo (ReaderT e m) where
  query = lift . query
  {-# INLINEABLE query #-}

queryMultiple :: Command -> IO Text
queryMultiple cmd =
  let shellApp = multipleShellApp cmd
   in T.strip <$> ShellApp.runSimple shellApp

multipleShellApp :: (Applicative f) => Command -> SimpleShell f EventError Text
multipleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      isSupported = pure True,
      parser = parseMultiple
    }

parseMultiple :: Text -> Either EventError Text
parseMultiple = Right

querySingle :: Command -> IO Text
querySingle cmd = do
  let shellApp = singleShellApp cmd
   in T.strip <$> ShellApp.runSimple shellApp

singleShellApp :: (Applicative f) => Command -> SimpleShell f EventError Text
singleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      isSupported = pure True,
      parser = parseSingle
    }

parseSingle :: Text -> Either EventError Text
parseSingle = Right
