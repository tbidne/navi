{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasEvents (..),
    HasLogEnv (..),
    HasLogQueue (..),
    HasNoteQueue (..),
    sendNoteQueue,

    -- * Concrete Env
    Env (..),
  )
where

import Navi.Config.Phase (ConfigPhase (ConfigPhaseEnv))
import Navi.Config.Types (NoteSystem)
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

  -- TODO: Is this needed?
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
    noteQueue :: TBQueue NaviNote,
    notifySystem :: NoteSystem ConfigPhaseEnv
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
    lensVL $ \f (MkEnv a1 a2 a3 a4 a5) ->
      fmap
        (\b -> MkEnv a1 (set' #logNamespace b a2) a3 a4 a5)
        (f (a2 ^. #logNamespace))
  {-# INLINE labelOptic #-}

-- | Convenience function for retrieving a 'TBQueue'
-- 'NaviNote' from the @env@ and sending the note.
sendNoteQueue ::
  ( HasCallStack,
    HasNoteQueue env,
    MonadReader env m,
    MonadSTM m
  ) =>
  NaviNote ->
  m ()
sendNoteQueue naviNote =
  asks getNoteQueue >>= (`writeTBQueueA` naviNote)
{-# INLINEABLE sendNoteQueue #-}
