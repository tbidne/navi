{-# LANGUAGE CPP #-}

-- | Provides an effect for querying system information.
module Navi.Effects.MonadSystemInfo
  ( MonadSystemInfo (..),
  )
where

import Navi.Data.CommandResult (CommandResult)
import Navi.Data.CommandResultParser (CommandResultParser)
import Navi.Data.PollInterval (PollInterval)
import Navi.Event.Types (EventError (MkEventError, long, name, short))
import Navi.Prelude
import Navi.Services.Types
  ( ServiceType
      ( BatteryPercentage,
        BatteryStatus,
        Multiple,
        NetworkInterface
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
  query :: (HasCallStack) => ServiceType result -> m (result, Maybe PollInterval)

instance MonadSystemInfo IO where
  query :: ServiceType result -> IO (result, Maybe PollInterval)
  query = \case
    BatteryPercentage bp ->
      rethrowPythia "Battery Percentage" $ (,Nothing) <$> Pythia.queryBattery bp
    BatteryStatus bp ->
      rethrowPythia "Battery Status" $ (,Nothing) . view #status <$> Pythia.queryBattery bp
    NetworkInterface device cp ->
      rethrowPythia "NetInterface" $ (,Nothing) <$> Pythia.queryNetInterface device cp
    Multiple cmd parser -> rethrowPythia "Multiple" $ querySimple cmd parser

rethrowPythia :: Text -> IO a -> IO a
rethrowPythia n io =
  io `catchSync` \e ->
    throwM
      $ MkEventError
        { name = n,
          short = "PythiaException",
          long = packText $ U.displayInner e
        }

instance (MonadSystemInfo m) => MonadSystemInfo (ReaderT e m) where
  query = lift . query
  {-# INLINEABLE query #-}

querySimple :: Command -> CommandResultParser -> IO (CommandResult, Maybe PollInterval)
querySimple cmd parser = (\cr -> (cr, cr ^. #pollInterval)) <$> ShellApp.runSimple shellApp
  where
    shellApp = mkApp cmd (parser ^. #unCommandResultParser)

mkApp ::
  (Applicative f) =>
  Command ->
  (Text -> Either EventError CommandResult) ->
  SimpleShell f EventError CommandResult
mkApp cmd parser =
  MkSimpleShell
    { command = cmd,
      isSupported = pure True,
      parser
    }
