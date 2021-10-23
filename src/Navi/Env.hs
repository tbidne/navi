-- | This module provides the 'Env' type.
module Navi.Env
  ( -- * HasX-style typeclasses
    HasPollInterval (..),
    HasEvents (..),
    HasClient (..),
    HasLogEnv (..),
    HasLogContexts (..),
    HasLogNamespace (..),

    -- * Concrete env for running Navi.
    Env (..),
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
import Optics.Generic (GField (..))
import Optics.Getter as O

-- | Retrieves the poll interval.
class HasPollInterval env where
  getPollInterval :: env -> NonNegative

-- | Retrieves the events.
class HasEvents ref env | env -> ref where
  getEvents :: env -> NonEmpty (AnyEvent ref)

-- | Retrieves the notification client.
class HasClient env where
  getClient :: env -> Client

-- | Retrieves the log environment.
class HasLogEnv env where
  getLogEnv :: env -> LogEnv
  setLogEnv :: env -> LogEnv -> env
  overLogEnv :: env -> (LogEnv -> LogEnv) -> env -> env

-- | Retrieves the log context.
class HasLogContexts env where
  getLogContexts :: env -> LogContexts
  setLogContexts :: env -> LogContexts -> env
  overLogContexts :: env -> (LogContexts -> LogContexts) -> env -> env

-- | Retrieves the log namespace.
class HasLogNamespace env where
  getLogNamespace :: env -> Namespace
  setLogNamespace :: env -> Namespace -> env
  overLogNamespace :: env -> (Namespace -> Namespace) -> env -> env

-- | 'Env' holds all of our environment data that is used while running navi.
data Env ref = MkEnv
  { pollInterval :: NonNegative,
    events :: NonEmpty (AnyEvent ref),
    client :: Client,
    kLogEnv :: LogEnv,
    kLogCtx :: LogContexts,
    kLogNamespace :: Namespace
  }
  deriving (Generic)

instance HasPollInterval (Env ref) where
  getPollInterval = O.view $ gfield @"pollInterval"

instance HasEvents ref (Env ref) where
  getEvents = O.view $ gfield @"events"

instance HasClient (Env ref) where
  getClient = O.view $ gfield @"client"

instance HasLogEnv (Env ref) where
  getLogEnv = O.view $ gfield @"kLogEnv"
  setLogEnv env le = env {kLogEnv = le}
  overLogEnv env f = \env' -> env' {kLogEnv = le'}
    where
      le' = f $ kLogEnv env

instance HasLogContexts (Env ref) where
  getLogContexts = O.view $ gfield @"kLogCtx"
  setLogContexts env ctx = env {kLogCtx = ctx}
  overLogContexts env f = \env' -> env' {kLogCtx = ctx'}
    where
      ctx' = f $ kLogCtx env

instance HasLogNamespace (Env ref) where
  getLogNamespace = O.view $ gfield @"kLogNamespace"
  setLogNamespace env ns = env {kLogNamespace = ns}
  overLogNamespace env f = \env' -> env' {kLogNamespace = ns'}
    where
      ns' = f $ kLogNamespace env

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
            kLogEnv = logEnv,
            kLogCtx = logContext,
            kLogNamespace = namespace
          }
  where
    mkErr = (<>) "Error initiating notifications: " . T.pack . displayException
