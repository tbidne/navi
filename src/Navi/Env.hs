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

import Control.Exception (Exception (..))
import DBus.Client (Client)
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Katip (LogContexts, LogEnv, Namespace)
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Effects (MonadNotify (..))
import Navi.Event.Types (AnyEvent)
import Navi.Prelude
import Optics.Getter as O
import Optics.Setter as O
import Optics.TH qualified as O
import Refined (NonNegative, Refined)

-- | Retrieves the poll interval.
class HasPollInterval env where
  getPollInterval :: env -> Refined NonNegative Int

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
  { pollInterval :: Refined NonNegative Int,
    events :: NonEmpty (AnyEvent ref),
    client :: Client,
    logEnv :: LogEnv,
    logCtx :: LogContexts,
    logNamespace :: Namespace
  }

O.makeFieldLabelsNoPrefix ''Env

instance HasPollInterval (Env ref) where
  getPollInterval = O.view #pollInterval

instance HasEvents ref (Env ref) where
  getEvents = O.view #events

instance HasClient (Env ref) where
  getClient = O.view #client

instance HasLogEnv (Env ref) where
  getLogEnv = O.view #logEnv
  setLogEnv = O.set #logEnv
  overLogEnv = O.over #logEnv

instance HasLogContexts (Env ref) where
  getLogContexts = O.view #logCtx
  setLogContexts = O.set #logCtx
  overLogContexts = O.over #logCtx

instance HasLogNamespace (Env ref) where
  getLogNamespace = O.view #logNamespace
  setLogNamespace = O.set #logNamespace
  overLogNamespace = O.over #logNamespace

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
