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
import Data.Text.Encoding qualified as TEnc
import Effects.MonadFsReader (MonadFsReader (readFile))
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
import Navi.Effects (MonadMutRef)
import Navi.Prelude
import Navi.Services.Battery.Percentage qualified as BattState
import Navi.Services.Battery.Status qualified as BattChargeStatus
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Custom.Single qualified as Single
import Navi.Services.Network.NetInterfaces qualified as NetConn

-- | Parses the provided toml file into a 'Config'. Throws 'ConfigErr' if
-- anything goes wrong.
readConfig ::
  ( HasCallStack,
    MonadCallStack m,
    MonadMutRef ref m,
    MonadFsReader m
  ) =>
  FilePath ->
  m (Config ref)
readConfig =
  readFile >=> \contents ->
    case TEnc.decodeUtf8' contents of
      Left ex -> throwWithCallStack ex
      Right tomlContents -> do
        case decode tomlContents of
          Left tomlErr -> throwWithCallStack $ TomlError tomlErr
          Right cfg -> tomlToConfig cfg
{-# INLINEABLE readConfig #-}

tomlToConfig ::
  ( HasCallStack,
    MonadCallStack m,
    MonadMutRef ref m
  ) =>
  ConfigToml ->
  m (Config ref)
tomlToConfig toml = do
  singleEvents <- traverse Single.toEvent singleToml
  multipleEvents <- traverse Multiple.toEvent multipleToml
  mBatteryLevelEvt <- traverse BattState.toEvent batteryPercentageToml
  mBatteryStatusEvt <- traverse BattChargeStatus.toEvent batteryStatusToml
  mNetInterfacesEvt <- traverse NetConn.toEvent netInterfacesToml
  let multipleEvts =
        singleEvents
          <> multipleEvents
          <> mNetInterfacesEvt
      maybeEvts =
        catMaybes
          [ mBatteryLevelEvt,
            mBatteryStatusEvt
          ]
      allEvts = maybeEvts <> multipleEvts

  case allEvts of
    [] -> throwWithCallStack NoEvents
    (e : es) ->
      pure $
        MkConfig
          { events = e :| es,
            logging = logCfg,
            noteSystem = noteSysCfg
          }
  where
    logCfg = fromMaybe defaultLogging (toml ^. #logToml)
    noteSysCfg = fromMaybe defaultNoteSystem (toml ^. #noteSystemToml)
    singleToml = toml ^. #singleToml
    multipleToml = toml ^. #multipleToml
    batteryPercentageToml = toml ^. #batteryPercentageToml
    batteryStatusToml = toml ^. #batteryStatusToml
    netInterfacesToml = toml ^. #netInterfacesToml
{-# INLINEABLE tomlToConfig #-}
