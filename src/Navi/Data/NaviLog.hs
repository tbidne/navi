{-# LANGUAGE UndecidableInstances #-}

-- | Provides the types for logging.
module Navi.Data.NaviLog
  ( LogEnv (..),
  )
where

import Navi.Prelude

-- | Holds logging env data.
--
-- @since 0.1
data LogEnv = MkLogEnv
  { -- | Handle for file logging.
    --
    -- @since 0.1
    logHandle :: Maybe Handle,
    -- | Level in which to log.
    --
    -- @since 0.1
    logLevel :: LogLevel,
    -- | The current logging namespace.
    --
    -- @since 0.1
    logNamespace :: Namespace,
    -- | Log queue.
    --
    -- @since 0.1
    logQueue :: TBQueue LogStr
  }

instance
  (k ~ A_Lens, a ~ Maybe Handle, b ~ Maybe Handle) =>
  LabelOptic "logHandle" k LogEnv LogEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkLogEnv b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ LogLevel, b ~ LogLevel) =>
  LabelOptic "logLevel" k LogEnv LogEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkLogEnv a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Namespace, b ~ Namespace) =>
  LabelOptic "logNamespace" k LogEnv LogEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkLogEnv a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TBQueue LogStr, b ~ TBQueue LogStr) =>
  LabelOptic "logQueue" k LogEnv LogEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkLogEnv a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}
