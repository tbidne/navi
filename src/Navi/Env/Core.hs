{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasEvents (..),
    HasLogEnv (..),
    HasNoteQueue (..),
    sendNoteQueue,

    -- ** Deriving
    TopField (..),
    CoreEnvField (..),

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

-- | 'Env' holds all of our environment data that is used while running navi.
data Env = MkEnv
  { events :: NonEmpty AnyEvent,
    logEnv :: Maybe LogEnv,
    noteQueue :: TBQueue NaviNote,
    notifySystem :: NoteSystem ConfigPhaseEnv
  }

instance
  (k ~ A_Lens, a ~ NonEmpty AnyEvent, b ~ NonEmpty AnyEvent) =>
  LabelOptic "events" k Env Env a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe LogEnv, b ~ Maybe LogEnv) =>
  LabelOptic "logEnv" k Env Env a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TBQueue NaviNote, b ~ TBQueue NaviNote) =>
  LabelOptic "noteQueue" k Env Env a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NoteSystem ConfigPhaseEnv, b ~ NoteSystem ConfigPhaseEnv) =>
  LabelOptic "notifySystem" k Env Env a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

deriving via (TopField Env) instance HasEvents Env

deriving via (TopField Env) instance HasLogEnv Env

deriving via (TopField Env) instance HasNoteQueue Env

-- | Used for deriving instances from the top level field name e.g.
-- 'events :: NonEmpty AnyEvent'.
type TopField :: Type -> Type
newtype TopField a = MkTopField a

-- | Used for deriving instances for types with a field 'coreEnv :: Env'.
type CoreEnvField :: Type -> Type
newtype CoreEnvField a = MkCoreEnvField a

-- | Retrieves the events.
class HasEvents env where
  getEvents :: env -> NonEmpty AnyEvent

-- | Retrieves the log environment.
class HasLogEnv env where
  getLogEnv :: env -> Maybe LogEnv

-- | Retrieves the note queue.
class HasNoteQueue env where
  getNoteQueue :: env -> TBQueue NaviNote

-- NOTE: For some reason, we cannot really compose these optics together
-- e.g. view (#coreEnv % #events) fails to typecheck. Probably there's a
-- way to do this with castOptic, but reusing the instance itself is easy.

instance
  (Is k A_Getter, LabelOptic' "events" k a (NonEmpty AnyEvent)) =>
  HasEvents (TopField a)
  where
  getEvents (MkTopField x) = view #events x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a Env) =>
  HasEvents (CoreEnvField a)
  where
  getEvents (MkCoreEnvField x) = getEvents $ view #coreEnv x

instance
  (Is k A_Getter, LabelOptic' "logEnv" k a (Maybe LogEnv)) =>
  HasLogEnv (TopField a)
  where
  getLogEnv (MkTopField x) = view #logEnv x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a Env) =>
  HasLogEnv (CoreEnvField a)
  where
  getLogEnv (MkCoreEnvField x) = getLogEnv $ view #coreEnv x

instance
  (Is k A_Getter, LabelOptic' "noteQueue" k a (TBQueue NaviNote)) =>
  HasNoteQueue (TopField a)
  where
  getNoteQueue (MkTopField x) = view #noteQueue x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a Env) =>
  HasNoteQueue (CoreEnvField a)
  where
  getNoteQueue (MkCoreEnvField x) = getNoteQueue $ view #coreEnv x

instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
  LabelOptic "namespace" k Env Env x y
  where
  labelOptic =
    lensVL $ \f (MkEnv a1 a2 a3 a4) ->
      fmap
        (\b -> MkEnv a1 (set' (_Just % #logNamespace) b a2) a3 a4)
        (f $ fromMaybe "" (a2 ^? _Just % #logNamespace))
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
