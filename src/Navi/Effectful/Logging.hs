{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides logging handlers.
module Navi.Effectful.Logging
  ( runLoggerDynamic,
    runLoggerDynamicNS,
  )
where

import Effectful.Dispatch.Dynamic (localSeqUnlift)
import Effectful.LoggerNS.Dynamic
  ( LoggerNSDynamic (GetNamespace, LocalNamespace),
    defaultLogFormatter,
    formatLog,
  )
import Effectful.Reader.Static (local)
import Navi.Env.Core (HasLogEnv (getLogEnv, localLogEnv))
import Navi.Prelude

-- | Runs 'LoggerDynamic'.
runLoggerDynamic ::
  forall env es a.
  ( Concurrent :> es,
    HasLogEnv env,
    LoggerNSDynamic :> es,
    Reader env :> es,
    TimeDynamic :> es
  ) =>
  Eff (LoggerDynamic : es) a ->
  Eff es a
runLoggerDynamic = interpret $ \_ -> \case
  LoggerLog loc _src lvl msg -> do
    logEnv <- asks @env getLogEnv
    let logQueue = view #logQueue logEnv
        logLevel = view #logLevel logEnv
    when (logLevel <= lvl) $ do
      formatted <- formatLog (defaultLogFormatter loc) lvl msg
      writeTBQueueA logQueue formatted

-- | Runs 'LoggerNSDynamic'.
runLoggerDynamicNS ::
  forall env es a.
  ( HasLogEnv env,
    Reader env :> es
  ) =>
  Eff (LoggerNSDynamic : es) a ->
  Eff es a
runLoggerDynamicNS = interpret $ \env -> \case
  GetNamespace -> asks @env (view #logNamespace . getLogEnv)
  LocalNamespace f m -> localSeqUnlift env $ \runner ->
    local @env (localLogEnv (over' #logNamespace f)) (runner m)
