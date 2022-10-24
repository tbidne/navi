module Unit.Navi.Services.Network.NetInterfaces.Toml
  ( tests,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (Timeout (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEventToml (..))
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml (..))
import Pythia.Services.NetInterface (NetInterfaceApp (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Network.NetInterfaces.Toml"
    [ appTests,
      repeatEventTests,
      errorNoteTests,
      timeoutTests,
      parsesExpected "device" "app=\"nmcli\"\ndevice = \"my-device\"" "my-device" (view #deviceName)
    ]

timeoutTests :: TestTree
timeoutTests =
  testGroup
    "Parses timeout"
    [ parsesExpected "with timeout" toLines timeout (view #mTimeout),
      parsesExpected "no timeout" "app=\"nmcli\"\ndevice = \"my-device\"" Nothing (view #mTimeout)
    ]
  where
    timeout = Just $ Seconds 5
    toLines =
      T.unlines
        [ "app=\"nmcli\"",
          "timeout = 5",
          "device = \"my-device\""
        ]

appTests :: TestTree
appTests =
  testGroup
    "Parses app"
    [ parsesApp "ip" NetInterfaceAppIp,
      parsesApp "nmcli" NetInterfaceAppNmCli
    ]

parsesApp :: Text -> NetInterfaceApp -> TestTree
parsesApp appName app =
  parsesExpected
    (unpack appName)
    txt
    app
    (view #app)
  where
    txt =
      T.unlines
        [ "app = \"" <> appName <> "\"",
          "device = \"my-device\""
        ]

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" "app=\"nmcli\"\ndevice = \"my-device\"" Nothing (view #repeatEvent)
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
        [ "app=\"nmcli\"",
          "repeat-events = " <> flag,
          "device = \"my-device\""
        ]

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected "<none>" "app=\"nmcli\"\ndevice = \"my-device\"" Nothing (view #errorNote)
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
        [ "app=\"nmcli\"",
          "error-events = \"" <> flag <> "\"",
          "device = \"my-device\""
        ]

parsesExpected :: (Eq a, Show a) => String -> Text -> a -> (NetInterfacesToml -> a) -> TestTree
parsesExpected = decodeExpected
