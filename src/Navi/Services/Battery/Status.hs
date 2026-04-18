-- | This module provides a service for alerts related to battery statuses.
module Navi.Services.Battery.Status
  ( BatteryStatusToml,
    toEvent,
  )
where

import Effects.Notify qualified as Notify
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote,
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Battery.Status.Toml (BatteryStatusToml)
import Navi.Services.Types (ServiceType (BatteryStatus))
import Pythia.Services.Battery
  ( BatteryApp,
    BatteryStatus
      ( Charging,
        Discharging,
        Full,
        Pending
      ),
  )

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent ::
  (MonadIORef m) =>
  BatteryStatusToml ->
  m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkStatusEvent to cfg pi repeatEvent errorNote
  pure $ MkAnyEvent evt
  where
    cfg = toml ^. #app
    to = toml ^. #mTimeout
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

mkStatusEvent ::
  Maybe NotifyTimeout ->
  BatteryApp ->
  PollInterval ->
  RepeatEvent BatteryStatus ->
  ErrorNote ->
  Event BatteryStatus BatteryStatus
mkStatusEvent to cfg pi repeatEvent errorNote =
  MkEvent
    { name = "battery-status",
      serviceType = BatteryStatus cfg,
      pollInterval = pi,
      raiseAlert = toNote to,
      repeatEvent = repeatEvent,
      errorNote = errorNote
    }

toNote :: Maybe NotifyTimeout -> BatteryStatus -> Maybe (BatteryStatus, Note)
toNote timeout status = (status,) <$> toNote' timeout (fromStatus status)
  where
    fromStatus Charging = "Battery charging"
    fromStatus Discharging = "Battery discharging"
    fromStatus Full = "Battery full"
    fromStatus Pending = "Battery pending"

toNote' :: Maybe NotifyTimeout -> Text -> Maybe Note
toNote' timeout msg =
  Just
    . Notify.setBody (Just msg)
    . Notify.setTimeout timeout
    $ Notify.mkNote "Battery Status"
