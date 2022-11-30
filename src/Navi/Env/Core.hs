{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasEvents (..),
    HasLogEnv (..),
    HasLogNamespace (..),
    HasLogQueue (..),
    HasNoteQueue (..),

    -- * Concrete Env
    Env (..),
  )
where

import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote)
import Navi.Effects.MonadLoggerContext (Namespace)
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

-- | Retrieves the events.
class HasEvents ref env | env -> ref where
  getEvents :: env -> NonEmpty (AnyEvent ref)

-- | Retrieves the log environment.
class HasLogEnv env where
  getLogEnv :: env -> LogEnv
  localLogEnv :: (LogEnv -> LogEnv) -> env -> env

-- | Retrieves the log namespace.
class HasLogNamespace env where
  getLogNamespace :: env -> Namespace
  localLogNamespace :: (Namespace -> Namespace) -> env -> env

-- | Retrieves the log queue.
class HasLogQueue env where
  getLogQueue :: env -> TBQueue LogStr

-- | Retrieves the note queue.
class HasNoteQueue env where
  getNoteQueue :: env -> TBQueue NaviNote

-- | 'Env' holds all of our environment data that is used while running navi.
data Env ref = MkEnv
  { events :: !(NonEmpty (AnyEvent ref)),
    logEnv :: !LogEnv,
    logNamespace :: !Namespace,
    logQueue :: !(TBQueue LogStr),
    noteQueue :: !(TBQueue NaviNote)
  }

makeFieldLabelsNoPrefix ''Env

instance HasEvents ref (Env ref) where
  getEvents = view #events
  {-# INLINEABLE getEvents #-}

instance HasLogEnv (Env ref) where
  getLogEnv = view #logEnv
  {-# INLINEABLE getLogEnv #-}
  localLogEnv = over' #logEnv
  {-# INLINEABLE localLogEnv #-}

instance HasLogNamespace (Env ref) where
  getLogNamespace = view #logNamespace
  {-# INLINEABLE getLogNamespace #-}
  localLogNamespace = over' #logNamespace
  {-# INLINEABLE localLogNamespace #-}

instance HasLogQueue (Env ref) where
  getLogQueue = view #logQueue
  {-# INLINEABLE getLogQueue #-}

instance HasNoteQueue (Env ref) where
  getNoteQueue = view #noteQueue
  {-# INLINEABLE getNoteQueue #-}
