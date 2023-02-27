-- | This module provides a service for alerts related to battery statuses.
module Navi.Services.Battery.Status
  ( BatteryStatusToml,
    toEvent,
  )
where

import Navi.Data.NaviNote (NaviNote, Timeout)
import Navi.Data.NaviNote qualified as NNote
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (..),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Battery.Status.Toml (BatteryStatusToml)
import Navi.Services.Types (ServiceType (..))
import Pythia.Services.Battery (BatteryApp, BatteryStatus (..))

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
  Maybe Timeout ->
  BatteryApp ->
  PollInterval ->
  RepeatEvent BatteryStatus ->
  ErrorNote ->
  Event BatteryStatus
mkStatusEvent to cfg pi repeatEvent errorNote =
  MkEvent
    { name = "battery-status",
      serviceType = BatteryStatus cfg,
      pollInterval = pi,
      raiseAlert = toNote to,
      repeatEvent = repeatEvent,
      errorNote = errorNote
    }

toNote :: Maybe Timeout -> BatteryStatus -> Maybe NaviNote
toNote timeout status = toNote' timeout $ fromStatus status
  where
    fromStatus Charging = "Battery charging"
    fromStatus Discharging = "Battery discharging"
    fromStatus Full = "Battery full"
    fromStatus Pending = "Battery pending"

toNote' :: Maybe Timeout -> Text -> Maybe NaviNote
toNote' timeout msg =
  Just $
    NNote.MkNaviNote
      { NNote.summary = "Battery Status",
        NNote.body = Just msg,
        NNote.urgency = Nothing,
        NNote.timeout = timeout
      }
