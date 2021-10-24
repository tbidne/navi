{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

data ConnType
  = Ethernet
  | Wifi
  | Wifi_P2P
  | Loopback
  | Tun
  | UnknownType Text
  deriving (Eq, Show)

data ConnState
  = Connected
  | Disconnected
  | Unavailable
  | Unmanaged
  | UnknownState Text
  deriving (Eq, Show)

data Connection = MkConnection
  { device :: Text,
    ctype :: ConnType,
    state :: ConnState,
    name :: Maybe Text
  }
  deriving (Eq, Show)

O.makeFieldLabelsNoPrefix ''Connection
