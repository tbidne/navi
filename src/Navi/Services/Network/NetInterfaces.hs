-- | This module provides a service for network connectivity.
module Navi.Services.Network.NetInterfaces
  ( toEvent,
  )
where

import Navi.Data.NaviNote
  ( NaviNote
      ( MkNaviNote,
        body,
        summary,
        timeout,
        urgency
      ),
  )
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
  )
import Navi.Prelude
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml)
import Navi.Services.Types (ServiceType (NetworkInterface))
import Pythia.Services.NetInterface
  ( Device (..),
    NetInterface (..),
    NetInterfaceState (..),
  )

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent ::
  (MonadIORef m) =>
  NetInterfacesToml ->
  m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  pure
    $ MkAnyEvent
    $ MkEvent
      { name = "network-interface",
        serviceType = cmd,
        pollInterval = pi,
        raiseAlert = toNote toml,
        repeatEvent = repeatEvent,
        errorNote = errorNote
      }
  where
    device = MkDevice $ toml ^. #deviceName
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
    cmd =
      NetworkInterface device (toml ^. #app)
{-# INLINEABLE toEvent #-}

toNote :: NetInterfacesToml -> NetInterface -> Maybe NaviNote
toNote noteToml conn =
  Just
    $ MkNaviNote
      { summary = "Network Connectivity",
        body = Just body,
        urgency = Nothing,
        timeout = noteToml ^. #mTimeout
      }
  where
    deviceTxt = conn ^. (#device % #unDevice)
    nameTxt = fromMaybe "Unknown" $ conn ^. #name
    body = "Device " <> deviceTxt <> stateTxt
    stateTxt = case conn ^. #state of
      NetStateUp -> " is connected to: " <> nameTxt
      NetStateDown -> " is disconnected from: " <> nameTxt
      NetStateUnknown txt -> " is in an unknown state: " <> txt
