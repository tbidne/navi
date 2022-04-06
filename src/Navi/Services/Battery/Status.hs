-- | This module provides a service for alerts related to battery statuses.
module Navi.Services.Battery.Status
  ( BatteryStatusToml,
    BatteryStatusToml.batteryStatusCodec,
    toEvent,
  )
where

import Navi.Data.NaviNote (NaviNote, Timeout)
import Navi.Data.NaviNote qualified as NNote
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (..),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Battery.Status.Toml (BatteryStatusNoteToml (..), BatteryStatusToml)
import Navi.Services.Battery.Status.Toml qualified as BatteryStatusToml
import Navi.Services.Types (ServiceType (..))
import Pythia.Services.Battery (BatteryConfig, BatteryStatus (..))

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent ::
  (MonadMutRef m ref) =>
  BatteryStatusToml ->
  m (AnyEvent ref)
toEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkStatusEvent note program repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    program = toml ^. #program
    note = toml ^. #note

mkStatusEvent ::
  BatteryStatusNoteToml ->
  BatteryConfig ->
  RepeatEvent ref BatteryStatus ->
  ErrorNote ref ->
  Event ref BatteryStatus
mkStatusEvent noteToml program repeatEvent errorNote =
  MkEvent
    { name = "Battery Status",
      serviceType = BatteryStatus program,
      raiseAlert = toNote noteToml,
      repeatEvent = repeatEvent,
      errorNote = errorNote
    }

toNote :: BatteryStatusNoteToml -> BatteryStatus -> Maybe NaviNote
toNote noteToml status = toNote' timeout $ fromStatus status
  where
    timeout = noteToml ^. #mTimeout

    fromStatus Charging = "Battery charging"
    fromStatus Discharging = "Battery discharging"
    fromStatus Full = "Battery full"
    fromStatus Pending = "Battery pending"
    fromStatus (Unknown txt) = "Unknown status: " <> txt

toNote' :: Maybe Timeout -> Text -> Maybe NaviNote
toNote' timeout msg =
  Just $
    NNote.MkNaviNote
      { NNote.summary = "Battery Status",
        NNote.body = Just msg,
        NNote.urgency = Nothing,
        NNote.timeout = timeout
      }
