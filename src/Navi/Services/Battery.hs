module Navi.Services.Battery
  ( BatteryToml,
    BatteryToml.batteryCodec,
    toBatteryEvent,
  )
where

import DBus.Notify
  ( Body (..),
    Hint (..),
    Note (..),
    Timeout (..),
  )
import Navi.Data.BoundedN qualified as BoundedN
import Navi.Effects (MonadMutRef)
import Navi.Event qualified as Event
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..))
import Navi.Prelude
import Navi.Services.Battery.Event qualified as BatteryEvent
import Navi.Services.Battery.Toml (BatteryLevelNoteToml (..), BatteryToml (..))
import Navi.Services.Battery.Toml qualified as BatteryToml
import Navi.Services.Battery.Types (BatteryLevel)

toBatteryEvent :: (MonadMutRef m ref) => BatteryToml -> m (AnyEvent ref)
toBatteryEvent MkBatteryToml {alerts, repeatEvent, errorEvent, batteryType} = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal errorEvent
  let evt = BatteryEvent.mkBatteryEvent lvlNoteList batteryType repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    lvlNoteList = toNote <$> alerts

toNote :: BatteryLevelNoteToml -> (BatteryLevel, Note)
toNote MkBatteryLevelNoteToml {level, urgency, mIcon, mTimeout} =
  (level, Event.mkNote mIcon summary body hints timeout)
  where
    body =
      Just $
        Text $
          "Power is less than "
            <> show (BoundedN.unBoundedN level)
            <> "%"
    summary = "Battery"
    hints = [Urgency urgency]
    timeout = fromMaybe (Milliseconds 10000) mTimeout
