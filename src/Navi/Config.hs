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
import Navi.Services.Battery qualified as Battery
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Custom.Single qualified as Single
import Toml qualified

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
tomlToConfig
  MkConfigToml
    { pollToml,
      singleToml,
      multipleToml,
      batteryToml,
      logToml
    } = do
    singleEvents <- traverse Single.toSingleEvent singleToml
    multipleEvents <- traverse Multiple.toMultipleEvent multipleToml
    mBatteryEvt <- maybe (pure Nothing) (fmap Just . Battery.toBatteryEvent) batteryToml
    let evts = singleEvents <> multipleEvents
        allEvts = maybe evts (: evts) mBatteryEvt

    pure $ case allEvts of
      [] -> Nothing
      (e : es) ->
        Just $
          MkConfig
            { pollInterval = pollToml,
              events = e :| es,
              logging = logToml
            }
