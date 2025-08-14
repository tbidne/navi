{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the network
-- connectivity service.
module Navi.Services.Network.NetInterfaces.Toml
  ( NetInterfacesToml (..),
  )
where

import Navi.Data.NaviNote (Timeout, timeoutOptDecoder)
import Navi.Data.PollInterval (PollInterval, pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    RepeatEventToml,
    errorNoteOptDecoder,
    repeatEventOptDecoder,
  )
import Navi.Prelude
import Pythia.Services.NetInterface
  ( NetInterfaceApp
      ( NetInterfaceAppIp,
        NetInterfaceAppNmCli
      ),
  )

-- | TOML for the network connectivity service.
data NetInterfacesToml = MkNetInterfacesToml
  { -- | Determines how we should query the system for network information.
    app :: NetInterfaceApp,
    -- | The name of the network device. For \"standard\" formats like
    -- ifconfig or NetworkManager, this might be something like
    -- wlp0s20f3 or enp0s31f6.
    deviceName :: Text,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEventToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''NetInterfacesToml

-- | @since 0.1
instance DecodeTOML NetInterfacesToml where
  tomlDecoder =
    MkNetInterfacesToml
      <$> getFieldWith decodeNetInterfaceApp "app"
      <*> getField "device"
      <*> pollIntervalOptDecoder
      <*> repeatEventOptDecoder
      <*> errorNoteOptDecoder
      <*> timeoutOptDecoder

decodeNetInterfaceApp :: Decoder NetInterfaceApp
decodeNetInterfaceApp =
  tomlDecoder >>= \case
    "nmcli" -> pure NetInterfaceAppNmCli
    "ip" -> pure NetInterfaceAppIp
    bad ->
      fail
        $ unpackText
        $ mconcat
          [ "Unexpected net-interface app: ",
            bad,
            ". Expected one of <nmcli | ip>."
          ]
