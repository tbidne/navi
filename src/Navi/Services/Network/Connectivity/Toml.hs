{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the network
-- connectivity service.
module Navi.Services.Network.Connectivity.Toml
  ( NetworkConnectivityToml (..),
    networkConnectivityCodec,
  )
where

import DBus.Notify (Icon)
import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Navi.Services.Network.Types (NetworkCommand (..))
import Navi.Services.Network.Types qualified as NTypes
import Optics.TH qualified as O
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the network connectivity service.
data NetworkConnectivityToml = MkNetworkConnectivityToml
  { -- | Determines how we should query the system for network information.
    networkCommand :: NetworkCommand,
    -- | The name of the network device, corresponding to the output from
    -- 'networkCommand'. For \"standard\" formats like ifconfig or
    -- NetworkManager, this might be something like wlp0s20f3 or enp0s31f6.
    deviceName :: Text,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout,
    -- | The image.
    mImage :: Maybe Icon
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''NetworkConnectivityToml

-- | Codec for 'BatteryStatusToml'.
networkConnectivityCodec :: TomlCodec NetworkConnectivityToml
networkConnectivityCodec =
  MkNetworkConnectivityToml
    <$> NTypes.networkCommandCodec .= networkCommand
    <*> Toml.text "device" .= deviceName
    <*> Toml.dioptional EToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout
    <*> Toml.dioptional (NaviNote.appImageKeyCodec "image") .= mImage
