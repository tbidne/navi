-- | This module provides a service for alerts related to battery statuses.
module Navi.Services.Battery.ChargeStatus
  ( BatteryStatusToml,
    BatteryStatusToml.batteryStatusCodec,
    toBatteryStatusEvent,
  )
where

import DBus.Notify (Icon)
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
import Navi.Services.Battery.ChargeStatus.Toml (BatteryStatusNoteToml (..), BatteryStatusToml)
import Navi.Services.Battery.ChargeStatus.Toml qualified as BatteryStatusToml
import Navi.Services.Types (ServiceType (..))
import Optics.Operators ((^.))
import System.Info.Services.Battery.ChargeStatus (ChargeStatus (..), Program)

-- | Transforms toml configuration data into an 'AnyEvent'.
toBatteryStatusEvent :: (MonadMutRef m ref) => BatteryStatusToml -> m (AnyEvent ref)
toBatteryStatusEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkStatusEvent note program repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    program = toml ^. #program
    note = toml ^. #note

mkStatusEvent ::
  BatteryStatusNoteToml ->
  Program ->
  RepeatEvent ref ChargeStatus ->
  ErrorNote ref ->
  Event ref ChargeStatus
mkStatusEvent noteToml program repeatEvent errorNote =
  MkEvent
    { name = "Battery Status",
      serviceType = BatteryChargeStatus program,
      raiseAlert = toNote noteToml,
      repeatEvent = repeatEvent,
      errorNote = errorNote
    }

toNote :: BatteryStatusNoteToml -> ChargeStatus -> Maybe NaviNote
toNote noteToml status = toNote' timeout $ fromStatus status
  where
    timeout = noteToml ^. #mTimeout
    mChargingImage = noteToml ^. #mChargingImage
    mDischargingImage = noteToml ^. #mChargingImage
    mFullImage = noteToml ^. #mFullImage

    fromStatus Charging = ("Battery charging", mChargingImage)
    fromStatus Discharging = ("Battery discharging", mDischargingImage)
    fromStatus Full = ("Battery full", mFullImage)
    fromStatus (Unknown txt) = ("Unknown status: " <> txt, Nothing)

toNote' :: Maybe Timeout -> (Text, Maybe Icon) -> Maybe NaviNote
toNote' timeout (msg, icon) =
  Just $
    NNote.MkNaviNote
      { NNote.summary = "Battery Status",
        NNote.body = Just msg,
        NNote.image = icon,
        NNote.urgency = Nothing,
        NNote.timeout = timeout
      }
