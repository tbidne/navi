{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasPollInterval (..),
    HasEvents (..),
    HasLogEnv (..),
    HasLogContexts (..),
    HasLogNamespace (..),

    -- * Concrete Env
    Env (..),
  )
where

import Katip (LogContexts, LogEnv, Namespace)
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

-- | Retrieves the poll interval.
class HasPollInterval env where
  getPollInterval :: env -> Word16

-- | Retrieves the events.
class HasEvents ref env | env -> ref where
  getEvents :: env -> NonEmpty (AnyEvent ref)

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
  { pollInterval :: Word16,
    events :: NonEmpty (AnyEvent ref),
    logEnv :: LogEnv,
    logCtx :: LogContexts,
    logNamespace :: Namespace
  }

makeFieldLabelsNoPrefix ''Env

instance HasPollInterval (Env ref) where
  getPollInterval = view #pollInterval

instance HasEvents ref (Env ref) where
  getEvents = view #events

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
