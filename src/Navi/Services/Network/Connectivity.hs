-- | This module provides a service for network connectivity.
module Navi.Services.Network.Connectivity
  ( toNetworkConnectivityEvent,
  )
where

import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..), Event (..))
import Navi.Prelude
import Navi.Services.Network.Connectivity.Toml (NetworkConnectivityToml, ProgramToml (..))
import Navi.Services.Types (ServiceType (..))
import Optics.Core ((%), (^.))
import System.Info.Services.Network.Connection (ConnState (..), Connection, Device (..), Program (..))

-- | Transforms toml configuration data into an 'AnyEvent'.
toNetworkConnectivityEvent ::
  (MonadMutRef m ref) =>
  NetworkConnectivityToml ->
  m (AnyEvent ref)
toNetworkConnectivityEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  pure $
    MkAnyEvent $
      MkEvent
        { name = "Network Connectivity",
          serviceType = cmd,
          raiseAlert = toNote toml,
          repeatEvent = repeatEvt,
          errorNote = errorNote
        }
  where
    device = MkDevice $ toml ^. #deviceName
    cmd = NetworkConnection $ case toml ^. #programToml of
      NetworkManagerToml -> NetworkManager device
      CustomToml t -> Custom device t

toNote :: NetworkConnectivityToml -> Connection -> Maybe NaviNote
toNote noteToml conn =
  Just $
    MkNaviNote
      { summary = "Network Connectivity",
        body = Just body,
        urgency = Nothing,
        timeout = noteToml ^. #mTimeout,
        image = noteToml ^. #mImage
      }
  where
    deviceTxt = conn ^. (#device % #unDevice)
    nameTxt = fromMaybe "Unknown" $ conn ^. #name
    body = "Device " <> deviceTxt <> stateTxt
    stateTxt = case conn ^. #state of
      Connected -> " is connected to: " <> nameTxt
      Disconnected -> " is disconnected from: " <> nameTxt
      Unavailable -> " is unavailable"
      Unmanaged -> " is unmanaged"
      UnknownState txt -> " is in an unknown state: " <> txt
