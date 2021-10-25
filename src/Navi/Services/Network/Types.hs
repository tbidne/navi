{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types used by network services.
module Navi.Services.Network.Types
  ( NetworkCommand (..),
    networkCommandCodec,
    Connection (..),
    ConnType (..),
    ConnState (..),
  )
where

import Navi.Prelude
import Optics.TH qualified as O
import Toml (TomlCodec)
import Toml qualified

-- | Determines how we should query the system for network information.
data NetworkCommand
  = NetworkManager
  | Custom Text
  deriving (Show)

-- | Codec for 'NetworkCommand'.
networkCommandCodec :: TomlCodec NetworkCommand
networkCommandCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure NetworkManager
  where
    showBatteryType NetworkManager = "networkmanager"
    showBatteryType (Custom t) = t
    parseBatteryType "networkmanager" = Right NetworkManager
    parseBatteryType t = Right $ Custom t

-- | Various connection types.
data ConnType
  = Ethernet
  | Wifi
  | Wifi_P2P
  | Loopback
  | Tun
  | UnknownType Text
  deriving (Eq, Show)

-- | Various connection states.
data ConnState
  = Connected
  | Disconnected
  | Unavailable
  | Unmanaged
  | UnknownState Text
  deriving (Eq, Show)

-- | Full connection data.
data Connection = MkConnection
  { -- | The device name.
    device :: Text,
    -- | The connection type.
    ctype :: ConnType,
    -- | The connection state.
    state :: ConnState,
    -- | The name of the connection (e.g. Wifi SSID).
    name :: Maybe Text
  }
  deriving (Eq, Show)

O.makeFieldLabelsNoPrefix ''Connection
