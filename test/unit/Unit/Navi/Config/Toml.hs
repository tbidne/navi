module Unit.Navi.Config.Toml
  ( tests,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Config.Toml (ConfigToml (..))
import Navi.Config.Types (LogLoc (..), Logging (..))
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml (..),
    BatteryPercentageToml (..),
  )
import Navi.Services.Battery.Status.Toml (BatteryStatusToml (..))
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml (..))
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..), Percentage (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config.Toml"
    [ parsesFull,
      logTests
    ]

parsesFull :: TestTree
parsesFull = parsesConfig fullConfig expectedFull "Parses full config"

logTests :: TestTree
logTests =
  testGroup
    "Parses log options"
    [ parsesConfig logDebugConfig expectedDebug "Parses log debug config",
      parsesConfig logInfoConfig expectedInfo "Parses log info config",
      parsesConfig logErrorConfig expectedError "Parses log error config"
    ]
  where
    expectedDebug =
      MkConfigToml
        { logToml =
            Just $
              MkLogging
                { severity = Just DebugS,
                  location = Nothing
                },
          noteSystemToml = Nothing,
          singleToml = [],
          multipleToml = [],
          batteryPercentageToml = Nothing,
          batteryStatusToml = expectedBatteryStatus,
          netInterfacesToml = []
        }
    expectedInfo = set (#logToml %? #severity % _Just) InfoS expectedDebug
    expectedError = set (#logToml %? #severity % _Just) ErrorS expectedDebug

parsesConfig :: Text -> ConfigToml -> String -> TestTree
parsesConfig config expected desc = testCase desc $ do
  let eResult = decode config
  case eResult of
    Left err -> assertFailure $ "Parse failed: " <> show err
    Right result -> expected @=? result

expectedFull :: ConfigToml
expectedFull =
  MkConfigToml
    { logToml =
        Just $
          MkLogging
            { severity = Just DebugS,
              location = Just $ File "some-file"
            },
      noteSystemToml = Nothing,
      singleToml = expectedSingle,
      multipleToml = [],
      batteryPercentageToml = expectedBatteryPercentage,
      batteryStatusToml = expectedBatteryStatus,
      netInterfacesToml = expectedNetInterfaces
    }

expectedSingle :: [SingleToml]
expectedSingle =
  [ MkSingleToml
      { command = "  some multiline cmd\n  end cmd\n",
        name = Just "a-single",
        triggerVal = "true",
        pollInterval = Just $ MkPollInterval 121,
        note = note,
        repeatEventCfg = Nothing,
        errEventCfg = Nothing
      }
  ]
  where
    note =
      MkNaviNote
        { summary = "Some single",
          body = Just "A body",
          urgency = Nothing,
          timeout = Just $ Seconds 15
        }

expectedBatteryPercentage :: Maybe BatteryPercentageToml
expectedBatteryPercentage =
  Just $
    MkBatteryPercentageToml
      { app = Many,
        pollInterval = Just $ MkPollInterval 15,
        repeatEvent = Nothing,
        errorNote = Nothing,
        alerts = alert1 :| [alert2]
      }
  where
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

expectedBatteryStatus :: Maybe BatteryStatusToml
expectedBatteryStatus =
  Just $
    MkBatteryStatusToml
      { app = Single BatteryAppAcpi,
        pollInterval = Nothing,
        repeatEvent = Nothing,
        errorNote = Nothing,
        mTimeout = Just $ Seconds 5
      }

expectedNetInterfaces :: [NetInterfacesToml]
expectedNetInterfaces =
  [ MkNetInterfacesToml
      { app = Many,
        deviceName = "my-device",
        pollInterval = Nothing,
        repeatEvent = Nothing,
        errorNote = Nothing,
        mTimeout = Nothing
      }
  ]

fullConfig :: Text
fullConfig =
  T.unlines
    [ "[logging]",
      "severity = \"debug\"",
      "location = \"some-file\"",
      "",
      "[battery-status]",
      "app = \"acpi\"",
      "timeout = 5",
      "",
      "[battery-percentage]",
      "poll-interval = 15",
      "",
      "[[battery-percentage.alert]]",
      "percent = 50",
      "[[battery-percentage.alert]]",
      "percent = 20",
      "urgency = \"critical\"",
      "",
      "[[net-interface]]",
      "device = \"my-device\"",
      "",
      "[[single]]",
      "name = \"a-single\"",
      "command = \"\"\"",
      "  some multiline cmd",
      "  end cmd",
      "\"\"\"",
      "trigger = \"true\"",
      "poll-interval = \"1m61s\"",
      "",
      "[single.note]",
      "summary = \"Some single\"",
      "body = \"A body\"",
      "timeout = 15"
    ]

logDebugConfig :: Text
logDebugConfig =
  T.unlines
    [ "[logging]",
      "severity = \"debug\"",
      "",
      "[battery-status]",
      "app = \"acpi\"",
      "timeout = 5"
    ]

logInfoConfig :: Text
logInfoConfig =
  T.unlines
    [ "[logging]",
      "severity = \"info\"",
      "",
      "[battery-status]",
      "app = \"acpi\"",
      "timeout = 5"
    ]

logErrorConfig :: Text
logErrorConfig =
  T.unlines
    [ "[logging]",
      "severity = \"error\"",
      "",
      "[battery-status]",
      "app = \"acpi\"",
      "timeout = 5"
    ]
