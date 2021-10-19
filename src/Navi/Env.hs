module Navi.Env
  ( Env (..),
    mkEnv,
  )
where

import Control.Exception (Exception (..))
import DBus.Client (Client)
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Data.NonNegative (NonNegative)
import Navi.Effects (MonadNotify (..))
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

data Env ref = MkEnv
  { pollInterval :: NonNegative,
    events :: NonEmpty (AnyEvent ref),
    client :: Client
  }

mkEnv :: MonadNotify m => Config ref -> m (Either Text (Env ref))
mkEnv config = do
  eClient <- initConn
  pure $ case eClient of
    Left err -> Left $ mkErr err
    Right client ->
      Right $
        MkEnv
          { pollInterval = Config.pollInterval config,
            events = Config.events config,
            client = client
          }
  where
    mkErr = (<>) "Error initiating notifications: " . T.pack . displayException
