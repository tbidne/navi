{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the network
-- connectivity service.
module Navi.Services.Network.NetInterfaces.Toml
  ( NetInterfacesToml (..),
    netInterfacesCodec,
  )
where

import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Pythia.Services.NetInterface (NetInterfaceApp (..), RunApp (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the network connectivity service.
data NetInterfacesToml = MkNetInterfacesToml
  { -- | Determines how we should query the system for network information.
    app :: RunApp NetInterfaceApp,
    -- | The name of the network device. For \"standard\" formats like
    -- ifconfig or NetworkManager, this might be something like
    -- wlp0s20f3 or enp0s31f6.
    deviceName :: Text,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''NetInterfacesToml

-- | Codec for 'NetInterfacesToml'.
netInterfacesCodec :: TomlCodec NetInterfacesToml
netInterfacesCodec =
  MkNetInterfacesToml
    <$> appCodec .= app
    <*> Toml.text "device" .= deviceName
    <*> Toml.dioptional EToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

appCodec :: TomlCodec (RunApp NetInterfaceApp)
appCodec = Toml.dimap f g mappCodec
  where
    f Many = Nothing
    f (Single x) = Just x
    g Nothing = Many
    g (Just x) = Single x

mappCodec :: TomlCodec (Maybe NetInterfaceApp)
mappCodec =
  Toml.dioptional $ Toml.textBy showBatteryType parseBatteryType "app"
  where
    showBatteryType NetInterfaceNmCli = "nmcli"
    showBatteryType NetInterfaceIp = "ip"
    parseBatteryType "nmcli" = Right NetInterfaceNmCli
    parseBatteryType "ip" = Right NetInterfaceIp
    parseBatteryType t = Left t
