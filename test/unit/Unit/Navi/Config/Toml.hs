{-# LANGUAGE QuasiQuotes #-}

module Unit.Navi.Config.Toml
  ( tests,
  )
where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Navi.Config.Toml
  ( ConfigToml
      ( MkConfigToml,
        batteryPercentageToml,
        batteryStatusToml,
        logToml,
        multipleToml,
        netInterfacesToml,
        noteSystemToml,
        singleToml
      ),
  )
import Navi.Config.Types
  ( FilesSizeMode (FilesSizeModeWarn),
    LogLoc (File),
    Logging
      ( MkLogging,
        location,
        severity,
        sizeMode
      ),
  )
import Navi.Data.NaviNote
  ( NaviNote
      ( MkNaviNote,
        body,
        summary,
        timeout,
        urgency
      ),
    Timeout (Seconds),
  )
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml
      ( MkBatteryPercentageNoteToml,
        mTimeout,
        percentage,
        urgency
      ),
    BatteryPercentageToml
      ( MkBatteryPercentageToml,
        alerts,
        app,
        errorNote,
        pollInterval,
        repeatEvent
      ),
  )
import Navi.Services.Battery.Status.Toml
  ( BatteryStatusToml
      ( MkBatteryStatusToml,
        app,
        errorNote,
        mTimeout,
        pollInterval,
        repeatEvent
      ),
  )
import Navi.Services.Custom.Single.Toml
  ( SingleToml
      ( MkSingleToml,
        command,
        errEventCfg,
        name,
        note,
        pollInterval,
        repeatEventCfg,
        triggerVal
      ),
  )
import Navi.Services.Network.NetInterfaces.Toml
  ( NetInterfacesToml
      ( MkNetInterfacesToml,
        app,
        deviceName,
        errorNote,
        mTimeout,
        pollInterval,
        repeatEvent
      ),
  )
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Services.Battery (BatteryApp (BatteryAppAcpi, BatteryAppUPower))
import Pythia.Services.NetInterface (NetInterfaceApp (NetInterfaceAppNmCli))
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
            Just
              $ MkLogging
                { severity = Just LevelDebug,
                  location = Nothing,
                  sizeMode = Nothing
                },
          noteSystemToml = Nothing,
          singleToml = [],
          multipleToml = [],
          batteryPercentageToml = Nothing,
          batteryStatusToml = expectedBatteryStatus,
          netInterfacesToml = []
        }
    expectedInfo = set' (#logToml %? #severity % _Just) LevelInfo expectedDebug
    expectedError = set' (#logToml %? #severity % _Just) LevelError expectedDebug

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
        Just
          $ MkLogging
            { severity = Just LevelDebug,
              location = Just $ File [osp|some-file|],
              sizeMode = Just (FilesSizeModeWarn (MkBytes 50_000_000))
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
  Just
    $ MkBatteryPercentageToml
      { app = BatteryAppUPower,
        pollInterval = Just $ MkPollInterval 15,
        repeatEvent = Nothing,
        errorNote = Nothing,
        alerts = alert1 :| [alert2]
      }
  where
    alert1 =
      MkBatteryPercentageNoteToml
        { percentage = Percentage.unsafePercentage 50,
          urgency = Nothing,
          mTimeout = Nothing
        }
    alert2 =
      MkBatteryPercentageNoteToml
        { percentage = Percentage.unsafePercentage 20,
          urgency = Just Critical,
          mTimeout = Nothing
        }

expectedBatteryStatus :: Maybe BatteryStatusToml
expectedBatteryStatus =
  Just
    $ MkBatteryStatusToml
      { app = BatteryAppAcpi,
        pollInterval = Nothing,
        repeatEvent = Nothing,
        errorNote = Nothing,
        mTimeout = Just $ Seconds 5
      }

expectedNetInterfaces :: [NetInterfacesToml]
expectedNetInterfaces =
  [ MkNetInterfacesToml
      { app = NetInterfaceAppNmCli,
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
      "size-mode = \"warn 50 mb\"",
      "",
      "[battery-status]",
      "app = \"acpi\"",
      "timeout = 5",
      "",
      "[battery-percentage]",
      "app = \"upower\"",
      "poll-interval = 15",
      "",
      "[[battery-percentage.alert]]",
      "percent = 50",
      "[[battery-percentage.alert]]",
      "percent = 20",
      "urgency = \"critical\"",
      "",
      "[[net-interface]]",
      "app = \"nmcli\"",
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
