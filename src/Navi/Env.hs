{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

import DBus.Client (Client)
import Data.List.NonEmpty (NonEmpty)
import Katip (LogContexts, LogEnv, Namespace)
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Effects (MonadNotify (..))
import Navi.Event.Types (AnyEvent)
import Navi.Prelude
import Numeric.Data.NonNegative (NonNegative)

-- | Retrieves the poll interval.
class HasPollInterval env where
  getPollInterval :: env -> NonNegative Int

-- | Retrieves the events.
class HasEvents ref env | env -> ref where
  getEvents :: env -> NonEmpty (AnyEvent ref)

-- | Retrieves the notification client.
class HasClient env where
  getClient :: env -> Client

-- | Retrieves the log environment.
class HasLogEnv env where
  getLogEnv :: env -> LogEnv
  setLogEnv :: LogEnv -> env -> env
  overLogEnv :: (LogEnv -> LogEnv) -> env -> env

-- | Retrieves the log context.
class HasLogContexts env where
  getLogContexts :: env -> LogContexts
  setLogContexts :: LogContexts -> env -> env
  overLogContexts :: (LogContexts -> LogContexts) -> env -> env

-- | Retrieves the log namespace.
class HasLogNamespace env where
  getLogNamespace :: env -> Namespace
  setLogNamespace :: Namespace -> env -> env
  overLogNamespace :: (Namespace -> Namespace) -> env -> env

-- | 'Env' holds all of our environment data that is used while running navi.
data Env ref = MkEnv
  { pollInterval :: NonNegative Int,
    events :: NonEmpty (AnyEvent ref),
    client :: Client,
    logEnv :: LogEnv,
    logCtx :: LogContexts,
    logNamespace :: Namespace
  }

makeFieldLabelsNoPrefix ''Env

instance HasPollInterval (Env ref) where
  getPollInterval = view #pollInterval

instance HasEvents ref (Env ref) where
  getEvents = view #events

instance HasClient (Env ref) where
  getClient = view #client

instance HasLogEnv (Env ref) where
  getLogEnv = view #logEnv
  setLogEnv = set #logEnv
  overLogEnv = over #logEnv

instance HasLogContexts (Env ref) where
  getLogContexts = view #logCtx
  setLogContexts = set #logCtx
  overLogContexts = over #logCtx

instance HasLogNamespace (Env ref) where
  getLogNamespace = view #logNamespace
  setLogNamespace = set #logNamespace
  overLogNamespace = over #logNamespace

-- | Creates an 'Env' from the provided log types and configuration data.
mkEnv ::
  MonadNotify m =>
  LogEnv ->
  LogContexts ->
  Namespace ->
  Config ref ->
  m (Env ref)
mkEnv logEnv logContext namespace config = do
  client <- initConn
  pure $
    MkEnv
      { pollInterval = Config.pollInterval config,
        events = Config.events config,
        client = client,
        logEnv = logEnv,
        logCtx = logContext,
        logNamespace = namespace
      }
