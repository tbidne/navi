{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the types for logging.
module Navi.Data.NaviLog
  ( LogFile (..),
    LogEnv (..),
  )
where

import Effects.LoggerNamespace (Namespace)
import Navi.Prelude

-- | Data for file logging.
--
-- @since 0.1
data LogFile = MkLogFile
  { -- | File handle.
    --
    -- @since 0.1
    handle :: !Handle,
    -- Finalizer to run e.g. flush/close.
    --
    -- @since 0.1
    finalizer :: IO ()
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''LogFile

-- | Holds logging env data.
--
-- @since 0.1
data LogEnv = MkLogEnv
  { -- | Data for file logging.
    --
    -- @since 0.1
    logFile :: !(Maybe LogFile),
    -- | Level in which to log.
    --
    -- @since 0.1
    logLevel :: !LogLevel,
    -- | The current logging namespace.
    --
    -- @since 0.1
    logNamespace :: !Namespace
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''LogEnv
