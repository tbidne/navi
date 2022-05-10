module Unit.Navi.Services.Battery.Status.Toml
  ( tests,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (Timeout (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEventToml (..))
import Navi.Services.Battery.Status.Toml (BatteryStatusToml (..), batteryStatusCodec)
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..))
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
    [ parsesExpected "with timeout" "timeout = \"5\"" timeout (view #mTimeout),
      parsesExpected "no timeout" "" noTimeout (view #mTimeout)
    ]
  where
    timeout = Just $ Seconds 5
    noTimeout = Nothing

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

parsesRepeatEvent :: String -> Text -> RepeatEventToml -> TestTree
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

parsesExpected :: (Eq a, Show a) => String -> Text -> a -> (BatteryStatusToml -> a) -> TestTree
parsesExpected = decodeExpected batteryStatusCodec
