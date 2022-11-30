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
import Navi.Data.NaviQueue (NaviQueue)
import Navi.Effects.MonadLoggerContext (Namespace)
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

-- | Retrieves the log namespace.
class HasLogNamespace env where
  getLogNamespace :: env -> Namespace
  setLogNamespace :: Namespace -> env -> env
  overLogNamespace :: (Namespace -> Namespace) -> env -> env

-- | Retrieves the log queue.
class HasLogQueue env where
  getLogQueue :: env -> NaviQueue LogStr

-- | Retrieves the note queue.
class HasNoteQueue env where
  getNoteQueue :: env -> NaviQueue NaviNote

-- | 'Env' holds all of our environment data that is used while running navi.
data Env ref = MkEnv
  { events :: !(NonEmpty (AnyEvent ref)),
    logEnv :: !LogEnv,
    logNamespace :: !Namespace,
    logQueue :: !(NaviQueue LogStr),
    noteQueue :: !(NaviQueue NaviNote)
  }

makeFieldLabelsNoPrefix ''Env

instance HasEvents ref (Env ref) where
  getEvents = view #events
  {-# INLINEABLE getEvents #-}

instance HasLogEnv (Env ref) where
  getLogEnv = view #logEnv
  {-# INLINEABLE getLogEnv #-}
  setLogEnv = set' #logEnv
  {-# INLINEABLE setLogEnv #-}
  overLogEnv = over' #logEnv
  {-# INLINEABLE overLogEnv #-}

instance HasLogNamespace (Env ref) where
  getLogNamespace = view #logNamespace
  {-# INLINEABLE getLogNamespace #-}
  setLogNamespace = set' #logNamespace
  {-# INLINEABLE setLogNamespace #-}
  overLogNamespace = over' #logNamespace
  {-# INLINEABLE overLogNamespace #-}

instance HasLogQueue (Env ref) where
  getLogQueue = view #logQueue
  {-# INLINEABLE getLogQueue #-}

instance HasNoteQueue (Env ref) where
  getNoteQueue = view #noteQueue
  {-# INLINEABLE getNoteQueue #-}
