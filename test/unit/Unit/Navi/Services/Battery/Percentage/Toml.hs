module Unit.Navi.Services.Battery.Percentage.Toml
  ( tests,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Navi.Data.NaviNote (Timeout (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEvtToml (..))
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml (..),
    BatteryPercentageToml (..),
  )
import Navi.Services.Battery.Percentage.Toml qualified as Percentage.Toml
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..), BatteryPercentage (..))
import Toml qualified
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
    [alert1, alert2, alert3]
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
          "timeout = \"15\""
        ]
    alert1 =
      MkBatteryPercentageNoteToml
        { percentage = MkBatteryPercentage $ Interval.unsafeLRInterval 50,
          urgency = Nothing,
          mTimeout = Nothing
        }
    alert2 =
      MkBatteryPercentageNoteToml
        { percentage = MkBatteryPercentage $ Interval.unsafeLRInterval 20,
          urgency = Just Critical,
          mTimeout = Nothing
        }
    alert3 =
      MkBatteryPercentageNoteToml
        { percentage = MkBatteryPercentage $ Interval.unsafeLRInterval 10,
          urgency = Just Critical,
          mTimeout = Just (Seconds 15)
        }

appTests :: TestTree
appTests =
  testGroup
    "Parses app"
    [ parsesApp "acpi" BatteryAcpi,
      parsesApp "sysfs" BatterySysFs,
      parsesApp "upower" BatteryUPower,
      parsesExpected "<none>" "" Many (view #app)
    ]

parsesApp :: Text -> BatteryApp -> TestTree
parsesApp appName app =
  parsesExpected
    (T.unpack appName)
    ("app = \"" <> appName <> "\"")
    (Single app)
    (view #app)

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" "" Nothing (view #repeatEvent)
    ]

parsesRepeatEvent :: String -> Text -> RepeatEvtToml -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    ("repeat-events = " <> flag)
    (Just ret)
    (view #repeatEvent)

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected "<none>" "" Nothing (view #errorNote)
    ]

parsesErrorEvent :: String -> Text -> ErrorNoteToml -> TestTree
parsesErrorEvent desc flag ret =
  parsesExpected
    desc
    ("error-events = \"" <> flag <> "\"")
    (Just ret)
    (view #errorNote)

parsesExpected :: (Eq a, Show a) => String -> Text -> a -> (BatteryPercentageToml -> a) -> TestTree
parsesExpected desc txt expected tomlFn = testCase desc $ do
  let eResult = Toml.decodeExact Percentage.Toml.batteryPercentageCodec txt
  case eResult of
    Left err -> assertFailure $ "Parsing config fails: " <> show err
    Right result -> expected @=? tomlFn result
