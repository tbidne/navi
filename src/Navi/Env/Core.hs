{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasEvents (..),
    HasLogEnv (..),
    HasLogQueue (..),
    HasNoteQueue (..),

    -- * Concrete Env
    Env (..),
  )
where

import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote)
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

-- | Retrieves the events.
class HasEvents env where
  getEvents :: env -> NonEmpty AnyEvent

-- | Retrieves the log environment.
class HasLogEnv env where
  getLogEnv :: env -> LogEnv
  localLogEnv :: (LogEnv -> LogEnv) -> env -> env

-- | Retrieves the log queue.
class HasLogQueue env where
  getLogQueue :: env -> TBQueue LogStr

-- | Retrieves the note queue.
class HasNoteQueue env where
  getNoteQueue :: env -> TBQueue NaviNote

-- | 'Env' holds all of our environment data that is used while running navi.
data Env = MkEnv
  { events :: NonEmpty AnyEvent,
    logEnv :: LogEnv,
    logQueue :: TBQueue LogStr,
    noteQueue :: TBQueue NaviNote
  }

makeFieldLabelsNoPrefix ''Env

instance HasEvents Env where
  getEvents = view #events

instance HasLogEnv Env where
  getLogEnv = view #logEnv
  localLogEnv = over' #logEnv

instance HasLogQueue Env where
  getLogQueue = view #logQueue

instance HasNoteQueue Env where
  getNoteQueue = view #noteQueue

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k Env Env x y
  where
  labelOptic =
    lensVL $ \f (MkEnv a1 a2 a3 a4) ->
      fmap
        (\b -> MkEnv a1 (set' #logNamespace b a2) a3 a4)
        (f (a2 ^. #logNamespace))
  {-# INLINE labelOptic #-}
