module Unit.Navi.Services.Battery.Percentage.Toml
  ( tests,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Navi.Data.NaviNote (Timeout (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEventToml (..))
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml (..),
    BatteryPercentageToml (..),
  )
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..), Percentage (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Battery.Percentage.Toml"
    [ parsesAlerts,
      appTests,
      repeatEventTests,
      errorNoteTests
    ]

parsesAlerts :: TestTree
parsesAlerts =
  parsesExpected
    "Parses alerts"
    alertTxt
    (alert1 :| [alert2, alert3])
    (view #alerts)
  where
    alertTxt =
      T.unlines
        [ "[[alert]]",
          "percent = 50",
          "[[alert]]",
          "percent = 20",
          "urgency = \"critical\"",
          "[[alert]]",
          "percent = 10",
          "urgency = \"critical\"",
          "timeout = 15"
        ]
    alert1 =
      MkBatteryPercentageNoteToml
        { percentage = MkPercentage $ Interval.unsafeLRInterval 50,
          urgency = Nothing,
          mTimeout = Nothing
        }
    alert2 =
      MkBatteryPercentageNoteToml
        { percentage = MkPercentage $ Interval.unsafeLRInterval 20,
          urgency = Just Critical,
          mTimeout = Nothing
        }
    alert3 =
      MkBatteryPercentageNoteToml
        { percentage = MkPercentage $ Interval.unsafeLRInterval 10,
          urgency = Just Critical,
          mTimeout = Just (Seconds 15)
        }

appTests :: TestTree
appTests =
  testGroup
    "Parses app"
    [ parsesApp "acpi" BatteryAppAcpi,
      parsesApp "sysfs" BatteryAppSysFs,
      parsesApp "upower" BatteryAppUPower,
      parsesExpected "<none>" "[[alert]]\npercent=50" Many (view #app)
    ]

parsesApp :: Text -> BatteryApp -> TestTree
parsesApp appName app =
  parsesExpected
    (unpack appName)
    txt
    (Single app)
    (view #app)
  where
    txt =
      T.unlines
        [ "app = \"" <> appName <> "\"",
          "[[alert]]",
          "percent = 50"
        ]

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" "[[alert]]\npercent=50" Nothing (view #repeatEvent)
    ]

parsesRepeatEvent :: String -> Text -> RepeatEventToml -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #repeatEvent)
  where
    txt =
      T.unlines
        [ "repeat-events = " <> flag,
          "[[alert]]",
          "percent = 50"
        ]

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected "<none>" "[[alert]]\npercent=50" Nothing (view #errorNote)
    ]

parsesErrorEvent :: String -> Text -> ErrorNoteToml -> TestTree
parsesErrorEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #errorNote)
  where
    txt =
      T.unlines
        [ "error-events = \"" <> flag <> "\"",
          "[[alert]]",
          "percent = 50"
        ]

parsesExpected :: (Eq a, Show a) => String -> Text -> a -> (BatteryPercentageToml -> a) -> TestTree
parsesExpected = decodeExpected
