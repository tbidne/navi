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
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEventToml
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ NetInterfaceApp, b ~ NetInterfaceApp) =>
  LabelOptic "app" k NetInterfacesToml NetInterfacesToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfacesToml a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkNetInterfacesToml b a2 a3 a4 a5 a6)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "deviceName" k NetInterfacesToml NetInterfacesToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfacesToml a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkNetInterfacesToml a1 b a3 a4 a5 a6)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe ErrorNoteToml, b ~ Maybe ErrorNoteToml) =>
  LabelOptic "errorNote" k NetInterfacesToml NetInterfacesToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfacesToml a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkNetInterfacesToml a1 a2 b a4 a5 a6)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe Timeout, b ~ Maybe Timeout) =>
  LabelOptic "mTimeout" k NetInterfacesToml NetInterfacesToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfacesToml a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkNetInterfacesToml a1 a2 a3 b a5 a6)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe PollInterval, b ~ Maybe PollInterval) =>
  LabelOptic "pollInterval" k NetInterfacesToml NetInterfacesToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfacesToml a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkNetInterfacesToml a1 a2 a3 a4 b a6)
          (f a5)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe RepeatEventToml, b ~ Maybe RepeatEventToml) =>
  LabelOptic "repeatEvent" k NetInterfacesToml NetInterfacesToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfacesToml a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkNetInterfacesToml a1 a2 a3 a4 a5 b)
          (f a6)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML NetInterfacesToml where
  tomlDecoder = do
    app <- getFieldWith decodeNetInterfaceApp "app"
    deviceName <- getField "device"
    errorNote <- errorNoteOptDecoder
    mTimeout <- timeoutOptDecoder
    pollInterval <- pollIntervalOptDecoder
    repeatEvent <- repeatEventOptDecoder
    pure
      $ MkNetInterfacesToml
        { app,
          deviceName,
          errorNote,
          mTimeout,
          pollInterval,
          repeatEvent
        }

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
