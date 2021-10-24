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
import Navi.Services.Battery.Level qualified as BatteryLevel
import Navi.Services.Battery.Status qualified as BatteryStatus
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Custom.Single qualified as Single
import Navi.Services.Network.Connectivity qualified as NetworkConnectivity
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
  singleEvents <-
    traverse Single.toSingleEvent singleToml
  multipleEvents <-
    traverse Multiple.toMultipleEvent multipleToml
  mBatteryLevelEvt <-
    traverse BatteryLevel.toBatteryLevelEvent batteryLevelToml
  mBatteryStatusEvt <-
    traverse BatteryStatus.toBatteryStatusEvent batteryStatusToml
  mNetworkConnectivityEvt <-
    traverse
      NetworkConnectivity.toNetworkConnectivityEvent
      networkConnectivityToml
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
    batteryLevelToml = toml ^. #batteryLevelToml
    batteryStatusToml = toml ^. #batteryStatusToml
    networkConnectivityToml = toml ^. #networkConnectivityToml
