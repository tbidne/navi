-- | This module provides a service for network connectivity.
module Navi.Services.Network.NetInterfaces
  ( toEvent,
  )
where

import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..), Event (..))
import Navi.Prelude
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml, ProgramToml (..))
import Navi.Services.Types (ServiceType (..))
import Optics.Core ((%), (^.))
import Pythia.Data.RunApp qualified as Pythia
import Pythia.Services.NetInterface (Device (..), NetInterface (..), NetInterfaceApp (..), NetInterfaceConfig (..), NetInterfaceState (..))

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent ::
  (MonadMutRef m ref) =>
  NetInterfacesToml ->
  m (AnyEvent ref)
toEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  pure $
    MkAnyEvent $
      MkEvent
        { name = "Network Interface",
          serviceType = cmd,
          raiseAlert = toNote toml,
          repeatEvent = repeatEvt,
          errorNote = errorNote
        }
  where
    device = MkDevice $ toml ^. #deviceName
    cmd = NetworkInterface $ case toml ^. #programToml of
      NetworkManagerToml -> MkNetInterfaceConfig (Pythia.Single NetInterfaceNmCli) (Just device)
      CustomToml _ -> MkNetInterfaceConfig (Pythia.Single NetInterfaceIp) (Just device)

-- TODO: remove custom

toNote :: NetInterfacesToml -> NetInterface -> Maybe NaviNote
toNote noteToml conn =
  Just $
    MkNaviNote
      { summary = "Network Connectivity",
        body = Just body,
        urgency = Nothing,
        timeout = noteToml ^. #mTimeout
      }
  where
    deviceTxt = conn ^. (#idevice % #unDevice)
    nameTxt = fromMaybe "Unknown" $ conn ^. #iname
    body = "Device " <> deviceTxt <> stateTxt
    stateTxt = case conn ^. #istate of
      Up -> " is connected to: " <> nameTxt
      Down -> " is disconnected from: " <> nameTxt
      UnknownState txt -> " is in an unknown state: " <> txt
