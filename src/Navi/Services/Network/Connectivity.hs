-- | This module provides a service for network connectivity.
module Navi.Services.Network.Connectivity
  ( toEvent,
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
import Pythia.Services.Network.Connection (ConnState (..), Connection, Device (..), NetConnApp (..))

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent ::
  (MonadMutRef m ref) =>
  NetworkConnectivityToml ->
  m (AnyEvent ref)
toEvent toml = do
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
      NetworkManagerToml -> NetConNmCli device
      CustomToml t -> NetConCustom device t

toNote :: NetworkConnectivityToml -> Connection -> Maybe NaviNote
toNote noteToml conn =
  Just $
    MkNaviNote
      { summary = "Network Connectivity",
        body = Just body,
        urgency = Nothing,
        timeout = noteToml ^. #mTimeout
      }
  where
    deviceTxt = conn ^. (#connDevice % #unDevice)
    nameTxt = fromMaybe "Unknown" $ conn ^. #connName
    body = "Device " <> deviceTxt <> stateTxt
    stateTxt = case conn ^. #connState of
      Connected -> " is connected to: " <> nameTxt
      Disconnected -> " is disconnected from: " <> nameTxt
      Unavailable -> " is unavailable"
      Unmanaged -> " is unmanaged"
      UnknownState txt -> " is in an unknown state: " <> txt
