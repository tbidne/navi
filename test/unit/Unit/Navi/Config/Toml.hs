module Unit.Navi.Config.Toml
  ( tests,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Config.Toml (ConfigToml (..))
import Navi.Config.Toml qualified as Config.Toml
import Navi.Config.Types (LogLoc (..), Logging (..))
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Services.Battery.Percentage.Toml qualified as Battery.Percentage
import Navi.Services.Battery.Status.Toml qualified as Battery.Status
import Navi.Services.Custom.Single.Toml qualified as Single
import Navi.Services.Network.NetInterfaces.Toml qualified as NetInterfaces
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..), BatteryPercentage (..))
import Toml qualified
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config.Toml"
    [ parsesConfig
    ]

parsesConfig :: TestTree
parsesConfig = testCase "Parses config" $ do
  let eResult = Toml.decodeExact Config.Toml.configCodec fullConfig
  case eResult of
    Left err -> assertFailure $ "Parsing config fails: " <> show err
    Right result -> expected @=? result

expected :: ConfigToml
expected =
  MkConfigToml
    { logToml =
        Just $
          MkLogging
            { severity = DebugS,
              location = File "some-file"
            },
      singleToml = expectedSingle,
      multipleToml = [],
      batteryPercentageToml = expectedBatteryPercentage,
      batteryStatusToml = expectedBatteryStatus,
      netInterfacesToml = expectedNetInterfaces
    }

expectedSingle :: [Single.SingleToml]
expectedSingle =
  [ Single.MkSingleToml
      { Single.command = "  some multiline cmd\n  end cmd\n",
        Single.triggerVal = "true",
        Single.pollInterval = Just $ MkPollInterval 121,
        Single.note = note,
        Single.repeatEvtCfg = Nothing,
        Single.errEvtCfg = Nothing
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

expectedBatteryPercentage :: Maybe Battery.Percentage.BatteryPercentageToml
expectedBatteryPercentage =
  Just $
    Battery.Percentage.MkBatteryPercentageToml
      { Battery.Percentage.app = Many,
        Battery.Percentage.pollInterval = Just $ MkPollInterval 15,
        Battery.Percentage.repeatEvent = Nothing,
        Battery.Percentage.errorNote = Nothing,
        Battery.Percentage.alerts = alert1 :| [alert2]
      }
  where
    alert1 =
      Battery.Percentage.MkBatteryPercentageNoteToml
        { Battery.Percentage.percentage = MkBatteryPercentage $ Interval.unsafeLRInterval 50,
          Battery.Percentage.urgency = Nothing,
          Battery.Percentage.mTimeout = Nothing
        }
    alert2 =
      Battery.Percentage.MkBatteryPercentageNoteToml
        { Battery.Percentage.percentage = MkBatteryPercentage $ Interval.unsafeLRInterval 20,
          Battery.Percentage.urgency = Just Critical,
          Battery.Percentage.mTimeout = Nothing
        }

expectedBatteryStatus :: Maybe Battery.Status.BatteryStatusToml
expectedBatteryStatus =
  Just $
    Battery.Status.MkBatteryStatusToml
      { Battery.Status.app = Single BatteryAcpi,
        Battery.Status.pollInterval = Nothing,
        Battery.Status.repeatEvent = Nothing,
        Battery.Status.errorNote = Nothing,
        Battery.Status.note = Battery.Status.MkBatteryStatusNoteToml (Just $ Seconds 5)
      }

expectedNetInterfaces :: [NetInterfaces.NetInterfacesToml]
expectedNetInterfaces =
  [ NetInterfaces.MkNetInterfacesToml
      { NetInterfaces.app = Many,
        NetInterfaces.deviceName = "my-device",
        NetInterfaces.pollInterval = Nothing,
        NetInterfaces.repeatEvent = Nothing,
        NetInterfaces.errorNote = Nothing,
        NetInterfaces.mTimeout = Nothing
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
      "timeout = \"5\"",
      "",
      "[battery-percentage]",
      "poll-interval = \"15\"",
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
      "timeout = \"15\""
    ]
