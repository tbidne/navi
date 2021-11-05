-- | This modules provides functionality for parsing configuration data
-- from a toml file.
module Navi.Config
  ( Config (..),
    Logging (..),
    LogLoc (..),
    ConfigErr (..),
    readConfig,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Navi.Config.Toml (ConfigToml (..))
import Navi.Config.Toml qualified as ConfigToml
import Navi.Config.Types
  ( Config (..),
    ConfigErr (..),
    LogLoc (..),
    Logging (..),
  )
import Navi.Effects (MonadMutRef, MonadShell (..))
import Navi.Prelude
import Navi.Services.Battery.ChargeStatus qualified as BattChargeStatus
import Navi.Services.Battery.State qualified as BattState
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Custom.Single qualified as Single
import Navi.Services.Network.Connectivity qualified as NetConn
import Optics.Operators ((^.))
import Toml qualified

-- | Parses the provided toml file into a 'Config'.
readConfig :: (MonadMutRef m ref, MonadShell m) => FilePath -> m (Either ConfigErr (Config ref))
readConfig path = do
  eContents <- readFile path
  case eContents of
    Left ex -> pure $ toFileErr ex
    Right contents -> do
      case Toml.decodeExact ConfigToml.configCodec contents of
        Left tomlErrs -> pure $ toTomlErr tomlErrs
        Right cfg -> do
          maybe (Left NoEvents) Right <$> tomlToConfig cfg
  where
    toFileErr = Left . FileErr
    toTomlErr = Left . TomlError

tomlToConfig :: (MonadMutRef m ref) => ConfigToml -> m (Maybe (Config ref))
tomlToConfig toml = do
  singleEvents <- traverse Single.toEvent singleToml
  multipleEvents <- traverse Multiple.toEvent multipleToml
  mBatteryLevelEvt <- traverse BattState.toEvent batteryStateToml
  mBatteryStatusEvt <- traverse BattChargeStatus.toEvent batteryStatusToml
  mNetworkConnectivityEvt <- traverse NetConn.toEvent netConnToml
  let multipleEvts =
        singleEvents
          <> multipleEvents
          <> mNetworkConnectivityEvt
      maybeEvts =
        mToList
          [ mBatteryLevelEvt,
            mBatteryStatusEvt
          ]
      allEvts = maybeEvts <> multipleEvts

  pure $ case allEvts of
    [] -> Nothing
    (e : es) ->
      Just $
        MkConfig
          { pollInterval = pollToml,
            events = e :| es,
            logging = logToml
          }
  where
    mToList = fromMaybe [] . sequenceA
    pollToml = toml ^. #pollToml
    logToml = toml ^. #logToml
    singleToml = toml ^. #singleToml
    multipleToml = toml ^. #multipleToml
    batteryStateToml = toml ^. #batteryStateToml
    batteryStatusToml = toml ^. #batteryChargeStatusToml
    netConnToml = toml ^. #networkConnectivityToml
