{-# LANGUAGE CPP #-}

-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Navi.Data.NaviNote (CustomResult, parseCustomResult)
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
    Single cmd -> rethrowPythia "Single" $ querySimple cmd
    Multiple cmd -> rethrowPythia "Multiple" $ querySimple cmd

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

querySimple :: Command -> IO CustomResult
querySimple cmd = do
  result <- ShellApp.runSimple shellApp
  pure $ parseCustomResult result
  where
    shellApp = mkApp cmd

mkApp :: (Applicative f) => Command -> SimpleShell f EventError Text
mkApp cmd =
  MkSimpleShell
    { command = cmd,
      isSupported = pure True,
      parser = Right
    }
