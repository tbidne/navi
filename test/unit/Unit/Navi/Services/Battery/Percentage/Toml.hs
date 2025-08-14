module Unit.Navi.Services.Battery.Percentage.Toml
  ( tests,
  )
where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Set qualified as Set
import Data.Text qualified as T
import Navi.Data.NaviNote (Timeout (Seconds))
import Navi.Event.Toml
  ( ErrorNoteToml
      ( ErrNoteAllowRepeatsToml,
        ErrNoteNoRepeatsToml,
        NoErrNoteToml
      ),
    MultiRepeatEventToml (MultiAllowRepeatsToml, MultiNoRepeatsToml, MultiSomeRepeatsToml),
  )
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml
      ( MkBatteryPercentageNoteToml,
        mTimeout,
        percentage,
        urgency
      ),
    BatteryPercentageToml,
    PercentageData (PercentageExact, PercentageRange),
  )
import Pythia.Data.Percentage (Percentage)
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Services.Battery
  ( BatteryApp
      ( BatteryAppAcpi,
        BatteryAppSysFs,
        BatteryAppUPower
      ),
  )
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
        [ "app = \"upower\"",
          "[[alert]]",
          "percent = 50",
          "[[alert]]",
          "percent = 20",
          "urgency = \"critical\"",
          "[[alert]]",
          "lower = 0",
          "upper = 10",
          "urgency = \"critical\"",
          "timeout = 15"
        ]
    alert1 =
      MkBatteryPercentageNoteToml
        { percentage = PercentageExact $ Percentage.unsafePercentage 50,
          urgency = Nothing,
          mTimeout = Nothing
        }
    alert2 =
      MkBatteryPercentageNoteToml
        { percentage = PercentageExact $ Percentage.unsafePercentage 20,
          urgency = Just Critical,
          mTimeout = Nothing
        }
    alert3 =
      MkBatteryPercentageNoteToml
        { percentage = PercentageRange (Percentage.unsafePercentage 0) (Percentage.unsafePercentage 10),
          urgency = Just Critical,
          mTimeout = Just (Seconds 15)
        }

appTests :: TestTree
appTests =
  testGroup
    "Parses app"
    [ parsesApp "acpi" BatteryAppAcpi,
      parsesApp "sysfs" BatteryAppSysFs,
      parsesApp "upower" BatteryAppUPower
    ]

parsesApp :: Text -> BatteryApp -> TestTree
parsesApp appName app =
  parsesExpected
    (unpackText appName)
    txt
    app
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
    [ parsesRepeatEvent "off" "false" MultiNoRepeatsToml,
      parsesRepeatEvent "on" "true" MultiAllowRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #repeatEvent),
      parsesRepeatEvent "<some>" "[0, 20]" (MultiSomeRepeatsToml expectedSome)
    ]
  where
    txt =
      T.unlines
        [ "app = \"upower\"",
          "[[alert]]",
          "percent = 50"
        ]

    expectedSome = Set.fromList [Percentage.unsafePercentage 0, Percentage.unsafePercentage 20]

parsesRepeatEvent :: String -> Text -> MultiRepeatEventToml Percentage -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #repeatEvent)
  where
    txt =
      T.unlines
        [ "app = \"upower\"",
          "repeat-events = " <> flag,
          "[[alert]]",
          "percent = 50",
          "[[alert]]",
          "lower = 20",
          "upper = 30",
          "[[alert]]",
          "lower = 10",
          "upper = 20",
          "[[alert]]",
          "lower = 0",
          "upper = 10"
        ]

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected
        "<none>"
        "app=\"upower\"\n[[alert]]\npercent=50"
        Nothing
        (view #errorNote)
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
        [ "app = \"upower\"",
          "error-events = \"" <> flag <> "\"",
          "[[alert]]",
          "percent = 50"
        ]

parsesExpected :: (Eq a, Show a) => String -> Text -> a -> (BatteryPercentageToml -> a) -> TestTree
parsesExpected = decodeExpected
