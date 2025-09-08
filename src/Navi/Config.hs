-- | This modules provides functionality for parsing configuration data
-- from a toml file.
module Navi.Config
  ( -- * Config
    readConfig,
    ConfigErr (..),
    Config (..),

    -- * Logging
    Logging (..),
    LogLoc (..),

    -- * Note System
    NoteSystem (..),
  )
where

import Data.Maybe (catMaybes)
import Navi.Config.Toml (ConfigToml (..))
import Navi.Config.Types
  ( Config (..),
    ConfigErr (..),
    LogLoc (..),
    Logging (..),
    NoteSystem (..),
    defaultLogging,
    defaultNoteSystem,
  )
import Navi.Prelude
import Navi.Services.Battery.Percentage qualified as BattState
import Navi.Services.Battery.Status qualified as BattChargeStatus
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Network.NetInterfaces qualified as NetConn

-- | Parses the provided toml file into a 'Config'. Throws 'ConfigErr' if
-- anything goes wrong.
readConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadIORef m,
    MonadThrow m
  ) =>
  OsPath ->
  m Config
readConfig =
  readFileUtf8ThrowM >=> \contents -> do
    -- FIXME: Unused keys do not cause errors. This should probably be addressed
    -- upstream. See https://github.com/brandonchinn178/toml-reader/issues/12
    case decode contents of
      Left tomlErr -> throwM $ TomlError tomlErr
      Right cfg -> tomlToConfig cfg

tomlToConfig ::
  ( HasCallStack,
    MonadIORef m,
    MonadThrow m
  ) =>
  ConfigToml ->
  m Config
tomlToConfig toml = do
  multipleEvents <- traverse Multiple.toEvent multipleToml
  mBatteryLevelEvt <- traverse BattState.toEvent batteryPercentageToml
  mBatteryStatusEvt <- traverse BattChargeStatus.toEvent batteryStatusToml
  mNetInterfacesEvt <- traverse NetConn.toEvent netInterfacesToml
  let multipleEvts = multipleEvents <> mNetInterfacesEvt
      maybeEvts =
        catMaybes
          [ mBatteryLevelEvt,
            mBatteryStatusEvt
          ]
      allEvts = maybeEvts <> multipleEvts

  case allEvts of
    [] -> throwM NoEvents
    (e : es) ->
      pure
        $ MkConfig
          { events = e :| es,
            logging = logCfg,
            noteSystem = noteSysCfg
          }
  where
    logCfg = fromMaybe defaultLogging (toml ^. #logToml)
    noteSysCfg = fromMaybe defaultNoteSystem (toml ^. #noteSystemToml)
    multipleToml = toml ^. #multipleToml
    batteryPercentageToml = toml ^. #batteryPercentageToml
    batteryStatusToml = toml ^. #batteryStatusToml
    netInterfacesToml = toml ^. #netInterfacesToml
