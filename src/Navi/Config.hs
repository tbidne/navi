module Navi.Config
  ( Config (..),
    ConfigErr (..),
    readConfig,
  )
where

import Control.Exception (Exception (..))
import Data.List.NonEmpty (NonEmpty (..))
import Navi.Config.Toml (ConfigToml (..))
import Navi.Config.Toml qualified as ConfigToml
import Navi.Data.NonNegative (NonNegative)
import Navi.Effects (MonadMutRef, MonadShell (..))
import Navi.Event (AnyEvent)
import Navi.Prelude
import Navi.Services.Battery qualified as Battery
import Navi.Services.Custom.Multiple qualified as Multiple
import Navi.Services.Custom.Single qualified as Single
import Toml (TomlDecodeError)
import Toml qualified
import UnexceptionalIO (SomeNonPseudoException)

data Config ref = MkConfig
  { pollInterval :: NonNegative,
    events :: NonEmpty (AnyEvent ref)
  }

data ConfigErr
  = FileErr SomeNonPseudoException
  | TomlError [TomlDecodeError]
  | NoEvents
  deriving (Show)

instance Exception ConfigErr

readConfig :: (MonadMutRef m ref, MonadShell m) => FilePath -> m (Either ConfigErr (Config ref))
readConfig path = do
  eContents <- readFile path
  case eContents of
    Left ex -> pure $ toFileErr ex
    Right contents -> do
      case Toml.decode ConfigToml.configCodec contents of
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
      batteryToml
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
              events = e :| es
            }
