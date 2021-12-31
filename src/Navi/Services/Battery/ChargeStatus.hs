-- | This module provides a service for alerts related to battery statuses.
module Navi.Services.Battery.ChargeStatus
  ( BatteryChargeStatusToml,
    BatteryChargeStatusToml.batteryChargeStatusCodec,
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
import Navi.Services.Battery.ChargeStatus.Toml (BatteryChargeStatusNoteToml (..), BatteryChargeStatusToml)
import Navi.Services.Battery.ChargeStatus.Toml qualified as BatteryChargeStatusToml
import Navi.Services.Types (ServiceType (..))
import Optics.Operators ((^.))
import Pythia.Services.Battery.ChargeStatus (BatteryChargeStatusApp, ChargeStatus (..))

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent ::
  (MonadMutRef m ref) =>
  BatteryChargeStatusToml ->
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
  BatteryChargeStatusNoteToml ->
  BatteryChargeStatusApp ->
  RepeatEvent ref ChargeStatus ->
  ErrorNote ref ->
  Event ref ChargeStatus
mkStatusEvent noteToml program repeatEvent errorNote =
  MkEvent
    { name = "Battery Charge Status",
      serviceType = BatteryChargeStatus program,
      raiseAlert = toNote noteToml,
      repeatEvent = repeatEvent,
      errorNote = errorNote
    }

toNote :: BatteryChargeStatusNoteToml -> ChargeStatus -> Maybe NaviNote
toNote noteToml status = toNote' timeout $ fromStatus status
  where
    timeout = noteToml ^. #mTimeout

    fromStatus Charging = "Battery charging"
    fromStatus Discharging = "Battery discharging"
    fromStatus Full = "Battery full"
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
