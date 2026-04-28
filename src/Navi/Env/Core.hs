{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core 'Env' type for Navi.
module Navi.Env.Core
  ( -- * HasX-style Typeclasses
    HasEvents (..),
    HasLogEnv (..),
    HasNoteQueue (..),
    HasNotifyEnv (..),
    sendNoteQueue,

    -- ** Deriving
    TopField (..),
    CoreEnvField (..),

    -- * Concrete Env
    Env (..),
  )
where

import Data.Coerce (coerce)
import Navi.Data.NaviLog (LogEnv)
import Navi.Event.Types (AnyEvent)
import Navi.Prelude

-- | 'Env' holds all of our environment data that is used while running navi.
data Env nenv = MkEnv
  { events :: NonEmpty AnyEvent,
    logEnv :: Maybe LogEnv,
    noteQueue :: TBQueue Note,
    notifyEnv :: nenv
  }

instance
  (k ~ A_Lens, a ~ NonEmpty AnyEvent, b ~ NonEmpty AnyEvent) =>
  LabelOptic "events" k (Env nenv) (Env nenv) a b
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
  LabelOptic "logEnv" k (Env nenv) (Env nenv) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TBQueue Note, b ~ TBQueue Note) =>
  LabelOptic "noteQueue" k (Env nenv) (Env nenv) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ nenv, b ~ nenv) =>
  LabelOptic "notifyEnv" k (Env nenv) (Env nenv) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkEnv a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

deriving via (TopField (Env nenv)) instance HasEvents (Env nenv)

deriving via (TopField (Env nenv)) instance HasLogEnv (Env nenv)

deriving via (TopField (Env nenv)) instance HasNoteQueue (Env nenv)

-- NOTE: [Derived notify env]
--
-- For some reason, our attempt at a derived instance:
--
--    deriving via (TopField (Env nenv)) instance HasNotifyEnv (Env nenv) nenv
--
-- does not work. It /should/ be generating the below instance (that's what
-- the other derivations produce), but instead it is trying to generate:
--
--   instance HasNotifyEnv (Env nenv) nenv where
--     getNotifyEnv =
--       coerce
--         @(Env nenv -> TopField (Env nenv))
--         @(Env nenv[sk:0] -> nenv)
--         (getNotifyEnv @(Env nenv) @(TopField (Env nenv)))
--
-- In other words, the instance is backwards? This is maybe a GHC bug, consider
-- reducing and reporting it.
instance HasNotifyEnv (Env nenv) nenv where
  getNotifyEnv =
    coerce
      @(TopField (Env nenv) -> nenv)
      @(Env nenv -> nenv)
      getNotifyEnv

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
  getNoteQueue :: env -> TBQueue Note

class HasNotifyEnv env nenv where
  getNotifyEnv :: env -> nenv

-- NOTE: For some reason, we cannot really compose these optics together
-- e.g. view (#coreEnv % #events) fails to typecheck. Probably there's a
-- way to do this with castOptic, but reusing the instance itself is easy.

instance
  (Is k A_Getter, LabelOptic' "events" k a (NonEmpty AnyEvent)) =>
  HasEvents (TopField a)
  where
  getEvents (MkTopField x) = view #events x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a (Env nenv)) =>
  HasEvents (CoreEnvField a)
  where
  getEvents (MkCoreEnvField x) = getEvents $ view #coreEnv x

instance
  (Is k A_Getter, LabelOptic' "logEnv" k a (Maybe LogEnv)) =>
  HasLogEnv (TopField a)
  where
  getLogEnv (MkTopField x) = view #logEnv x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a (Env nenv)) =>
  HasLogEnv (CoreEnvField a)
  where
  getLogEnv (MkCoreEnvField x) = getLogEnv $ view #coreEnv x

instance
  (Is k A_Getter, LabelOptic' "noteQueue" k a (TBQueue Note)) =>
  HasNoteQueue (TopField a)
  where
  getNoteQueue (MkTopField x) = view #noteQueue x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a (Env nenv)) =>
  HasNoteQueue (CoreEnvField a)
  where
  getNoteQueue (MkCoreEnvField x) = getNoteQueue $ view #coreEnv x

instance
  (Is k A_Getter, LabelOptic' "notifyEnv" k a nenv) =>
  HasNotifyEnv (TopField a) nenv
  where
  getNotifyEnv (MkTopField x) = view #notifyEnv x

instance
  (Is k A_Getter, LabelOptic' "coreEnv" k a (Env nenv)) =>
  HasNotifyEnv (CoreEnvField a) nenv
  where
  getNotifyEnv (MkCoreEnvField x) = getNotifyEnv $ view #coreEnv x

instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
  LabelOptic "namespace" k (Env nenv) (Env nenv) x y
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
    MonadAtomic m,
    MonadReader env m
  ) =>
  Note ->
  m ()
sendNoteQueue naviNote =
  asks getNoteQueue >>= (`writeTBQueueA'` naviNote)
{-# INLINEABLE sendNoteQueue #-}
