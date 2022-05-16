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
import Navi.Config.Toml qualified as ConfigToml
import Navi.Config.Types
  ( Config (..),
    ConfigErr (..),
    LogLoc (..),
    Logging (..),
    NoteSystem (..),
    defaultLogging,
    defaultNoteSystem,
  )
import Navi.Effects (MonadMutRef, MonadShell (..))
import Navi.Prelude
import Navi.Services.Battery.Percentage qualified as BattState
import Navi.Services.Battery.Status qualified as BattChargeStatus
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Custom.Single qualified as Single
import Navi.Services.Network.NetInterfaces qualified as NetConn
import Toml qualified

-- | Parses the provided toml file into a 'Config'. Throws 'ConfigErr' if
-- anything goes wrong.
readConfig ::
  (MonadMutRef ref m, MonadShell m, MonadCatch m) =>
  FilePath ->
  m (Config ref)
readConfig path = do
  eContents <- try $ readFile path
  case eContents of
    Left ex -> throwM $ FileErr ex
    Right contents -> do
      case Toml.decodeExact ConfigToml.configCodec contents of
        Left tomlErrs -> throwM $ TomlError tomlErrs
        Right cfg -> tomlToConfig cfg
{-# INLINEABLE readConfig #-}

tomlToConfig :: (MonadMutRef ref m, MonadThrow m) => ConfigToml -> m (Config ref)
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
    [] -> throwM NoEvents
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
