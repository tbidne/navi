-- | This module provides the 'Env' type.
module Navi.Env
  ( Env (..),
    mkEnv,
  )
where

import Control.Exception (Exception (..))
import DBus.Client (Client)
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Katip (LogContexts, LogEnv, Namespace)
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Data.NonNegative (NonNegative)
import Navi.Effects (MonadNotify (..))
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

-- | 'Env' holds all of our environment data that is used while running navi.
data Env ref = MkEnv
  { pollInterval :: NonNegative,
    events :: NonEmpty (AnyEvent ref),
    client :: Client,
    logEnv :: LogEnv,
    logCtx :: LogContexts,
    logNamespace :: Namespace
  }
  deriving (Generic)

-- | Creates an 'Env' from the provided log types and configuration data.
mkEnv ::
  MonadNotify m =>
  LogEnv ->
  LogContexts ->
  Namespace ->
  Config ref ->
  m (Either Text (Env ref))
mkEnv logEnv logContext namespace config = do
  eClient <- initConn
  pure $ case eClient of
    Left err -> Left $ mkErr err
    Right client ->
      Right $
        MkEnv
          { pollInterval = Config.pollInterval config,
            events = Config.events config,
            client = client,
            logEnv = logEnv,
            logCtx = logContext,
            logNamespace = namespace
          }
  where
    mkErr = (<>) "Error initiating notifications: " . T.pack . displayException
