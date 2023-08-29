{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides an effect for querying system information.
module Navi.Effectful.Pythia
  ( -- * Effect
    PythiaDynamic (..),
    query,

    -- ** Handler
    runPythiaDynamicIO,
  )
where

import Data.Text qualified as T
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem.FileReader.Dynamic (runFileReaderDynamicIO)
import Effectful.FileSystem.PathReader.Dynamic (runPathReaderDynamicIO)
import Effectful.Process.Typed (runTypedProcess)
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

data PythiaDynamic :: Effect where
  Query :: ServiceType result -> PythiaDynamic m result

type instance DispatchOf PythiaDynamic = Dynamic

-- | Runs 'PythiaDynamic' in IO.
runPythiaDynamicIO ::
  (IOE :> es) =>
  Eff (PythiaDynamic : es) a ->
  Eff es a
runPythiaDynamicIO = reinterpret (unsafeEff_ . runPythiaIO) $ \_ -> \case
  Query (BatteryPercentage bp) ->
    rethrowPythia "Battery Percentage" $ Pythia.queryBattery bp
  Query (BatteryStatus bp) ->
    rethrowPythia "Battery Status" $ view #status <$> Pythia.queryBattery bp
  Query (NetworkInterface device cp) ->
    rethrowPythia "NetInterface" $ Pythia.queryNetInterface device cp
  Query (Single cmd) -> rethrowPythia "Single" $ querySingle cmd
  Query (Multiple cmd) -> rethrowPythia "Multiple" $ queryMultiple cmd
  where
    runPythiaIO =
      runEff
        . runConcurrent
        . runFileReaderDynamicIO
        . runPathReaderDynamicIO
        . runTypedProcess

query ::
  (PythiaDynamic :> es) =>
  ServiceType result ->
  Eff es result
query = send . Query

rethrowPythia :: Text -> Eff es a -> Eff es a
rethrowPythia n io =
  io `catchAny` \e ->
    throwM
      $ MkEventError
        { name = n,
          short = "PythiaException",
          long = pack $ displayException e
        }

queryMultiple ::
  ( Concurrent :> es,
    TypedProcess :> es
  ) =>
  Command ->
  Eff es Text
queryMultiple cmd =
  let shellApp = multipleShellApp cmd
   in T.strip <$> ShellApp.runSimple shellApp

multipleShellApp :: Command -> SimpleShell es EventError Text
multipleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      isSupported = pure True,
      parser = parseMultiple
    }

parseMultiple :: Text -> Either EventError Text
parseMultiple = Right

querySingle ::
  ( Concurrent :> es,
    TypedProcess :> es
  ) =>
  Command ->
  Eff es Text
querySingle cmd = do
  let shellApp = singleShellApp cmd
   in T.strip <$> ShellApp.runSimple shellApp

singleShellApp :: Command -> SimpleShell es EventError Text
singleShellApp cmd =
  MkSimpleShell
    { command = cmd,
      isSupported = pure True,
      parser = parseSingle
    }

parseSingle :: Text -> Either EventError Text
parseSingle = Right
