module Unit.Navi.Services.Battery.Status.Toml
  ( tests,
  )
where

import Navi.Data.NaviNote (Timeout (Seconds))
import Navi.Event.Toml
  ( ErrorNoteToml
      ( ErrNoteAllowRepeatsToml,
        ErrNoteNoRepeatsToml,
        NoErrNoteToml
      ),
    RepeatEventToml (AllowRepeatsToml, NoRepeatsToml),
  )
import Navi.Services.Battery.Status.Toml (BatteryStatusToml)
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
    "Navi.Services.Battery.Status.Toml"
    [ noteTests,
      appTests,
      repeatEventTests,
      errorNoteTests
    ]

noteTests :: TestTree
noteTests =
  testGroup
    "Parses note"
    [ parsesExpected "with timeout" "app=\"upower\"\ntimeout = 5" timeout (view #mTimeout),
      parsesExpected "no timeout" "app=\"upower\"\n" noTimeout (view #mTimeout)
    ]
  where
    timeout = Just $ Seconds 5
    noTimeout = Nothing

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
    ("app = \"" <> appName <> "\"")
    app
    (view #app)

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" "app=\"upower\"\n" Nothing (view #repeatEvent)
    ]

parsesRepeatEvent :: String -> Text -> RepeatEventToml -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    ("app=\"upower\"\nrepeat-events = " <> flag)
    (Just ret)
    (view #repeatEvent)

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected "<none>" "app=\"upower\"\n" Nothing (view #errorNote)
    ]

parsesErrorEvent :: String -> Text -> ErrorNoteToml -> TestTree
parsesErrorEvent desc flag ret =
  parsesExpected
    desc
    ("app=\"upower\"\nerror-events = \"" <> flag <> "\"")
    (Just ret)
    (view #errorNote)

parsesExpected :: (Eq a, Show a) => String -> Text -> a -> (BatteryStatusToml -> a) -> TestTree
parsesExpected = decodeExpected
