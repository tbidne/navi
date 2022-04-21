{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.Status.Toml
  ( BatteryStatusToml (..),
    BatteryStatusNoteToml (..),
    batteryStatusCodec,
  )
where

import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml, word16Codec)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Navi.Services.Battery.Common (appCodec)
import Pythia.Services.Battery (BatteryApp (..), RunApp (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the battery status notification.
newtype BatteryStatusNoteToml = MkBatteryStatusNoteToml
  { -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryStatusNoteToml

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    app :: RunApp BatteryApp,
    -- | The poll interval.
    pollInterval :: Maybe Word16,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The alert for this service.
    note :: BatteryStatusNoteToml
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryStatusToml

-- | Codec for 'BatteryStatusToml'.
batteryStatusCodec :: TomlCodec BatteryStatusToml
batteryStatusCodec =
  MkBatteryStatusToml
    <$> appCodec .= app
    <*> Toml.dioptional (word16Codec "poll-interval") .= pollInterval
    <*> Toml.dioptional EToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> batteryStatusNoteCodec .= note

batteryStatusNoteCodec :: TomlCodec BatteryStatusNoteToml
batteryStatusNoteCodec =
  MkBatteryStatusNoteToml
    <$> Toml.dioptional NaviNote.timeoutCodec .= mTimeout
