{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasEvents (..),
    HasLogEnv (..),
    HasLogContexts (..),
    HasLogNamespace (..),
    HasLogQueue (..),
    HasNoteQueue (..),

    -- * Concrete Env
    Env (..),
  )
where

import Katip (LogContexts, LogEnv, Namespace)
import Navi.Data.NaviLog (NaviLog)
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.NaviQueue (NaviQueue)
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

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

-- | Retrieves the log queue.
class HasLogQueue env where
  getLogQueue :: env -> NaviQueue (NaviLog, Namespace)

-- | Retrieves the note queue.
class HasNoteQueue env where
  getNoteQueue :: env -> NaviQueue NaviNote

-- | 'Env' holds all of our environment data that is used while running navi.
data Env ref = MkEnv
  { events :: !(NonEmpty (AnyEvent ref)),
    logEnv :: !LogEnv,
    logCtx :: !LogContexts,
    logNamespace :: !Namespace,
    logQueue :: !(NaviQueue (NaviLog, Namespace)),
    noteQueue :: !(NaviQueue NaviNote)
  }

makeFieldLabelsNoPrefix ''Env

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

instance HasLogQueue (Env ref) where
  getLogQueue = view #logQueue

instance HasNoteQueue (Env ref) where
  getNoteQueue = view #noteQueue
