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
import Navi.Data.BoundedN (BoundedN (..))
import Navi.Event qualified as Event
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (Event (..))
import Navi.MonadNavi (MonadNavi)
import Navi.Prelude
import Navi.Services.Battery.Event qualified as BatteryEvent
import Navi.Services.Battery.Toml (BatteryLevelNoteToml (..), BatteryToml (..))
import Navi.Services.Battery.Toml qualified as BatteryToml
import Navi.Services.Battery.Types (BatteryLevel)

toBatteryEvent :: MonadNavi m => BatteryToml -> m (Event m)
toBatteryEvent (MkBatteryToml lu re ee) = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal re
  errorNote <- EventToml.mErrorNoteTomlToVal ee
  let evt = BatteryEvent.mkBatteryEvent lvlNoteList repeatEvt errorNote
  pure evt
  where
    lvlNoteList = toNote <$> lu

toNote :: BatteryLevelNoteToml -> (BatteryLevel, Note)
toNote (MkBatteryLevelNoteToml b@(MkBoundedN n) urgency icon mTimeout) = (b, Event.mkNote icon summary body hints timeout)
  where
    body = Just $ Text $ "Power is less than " <> show n <> "%"
    summary = "Battery"
    hints = [Urgency urgency]
    timeout = fromMaybe (Milliseconds 10000) mTimeout
