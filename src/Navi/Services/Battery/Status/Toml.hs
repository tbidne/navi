{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.Status.Toml
  ( BatteryStatusToml (..),
  )
where

import Navi.Data.NaviNote (Timeout, timeoutOptDecoder)
import Navi.Data.PollInterval (PollInterval (..), pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    RepeatEventToml,
    errorNoteOptDecoder,
    repeatEventOptDecoder,
  )
import Navi.Prelude
import Navi.Services.Battery.Common (batteryAppDecoder)
import Pythia.Services.Battery (BatteryApp)

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    app :: BatteryApp,
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
  (k ~ A_Lens, a ~ BatteryApp, b ~ BatteryApp) =>
  LabelOptic "app" k BatteryStatusToml BatteryStatusToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryStatusToml a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkBatteryStatusToml b a2 a3 a4 a5)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe ErrorNoteToml, b ~ Maybe ErrorNoteToml) =>
  LabelOptic "errorNote" k BatteryStatusToml BatteryStatusToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryStatusToml a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkBatteryStatusToml a1 b a3 a4 a5)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe Timeout, b ~ Maybe Timeout) =>
  LabelOptic "mTimeout" k BatteryStatusToml BatteryStatusToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryStatusToml a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkBatteryStatusToml a1 a2 b a4 a5)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe PollInterval, b ~ Maybe PollInterval) =>
  LabelOptic "pollInterval" k BatteryStatusToml BatteryStatusToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryStatusToml a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkBatteryStatusToml a1 a2 a3 b a5)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe RepeatEventToml, b ~ Maybe RepeatEventToml) =>
  LabelOptic "repeatEvent" k BatteryStatusToml BatteryStatusToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryStatusToml a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkBatteryStatusToml a1 a2 a3 a4 b)
          (f a5)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML BatteryStatusToml where
  tomlDecoder = do
    app <- getFieldWith batteryAppDecoder "app"
    errorNote <- errorNoteOptDecoder
    mTimeout <- timeoutOptDecoder
    pollInterval <- pollIntervalOptDecoder
    repeatEvent <- repeatEventOptDecoder
    pure
      $ MkBatteryStatusToml
        { app,
          errorNote,
          mTimeout,
          pollInterval,
          repeatEvent
        }
