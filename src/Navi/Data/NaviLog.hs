{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the types for logging.
module Navi.Data.NaviLog
  ( LogEnv (..),
  )
where

import Effects.LoggerNS (Namespace)
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
    logNamespace :: Namespace
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''LogEnv
