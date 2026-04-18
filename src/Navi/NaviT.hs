{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import Effects.Logger.Namespace
  ( defaultLogFormatter,
    formatLog,
  )
import Effects.Time (MonadTime)
import Navi.Effects.MonadSystemInfo (MonadSystemInfo)
import Navi.Env.Core
  ( Env,
    HasLogEnv (getLogEnv),
  )
import Navi.Prelude

-- | NaviT is the core type used to run the application.
type NaviT :: Type -> (Type -> Type) -> Type -> Type
newtype NaviT e m a = MkNaviT (ReaderT e m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadAtomic,
      MonadCatch,
      MonadFileReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadNotify,
      MonadProcess,
      MonadReader e,
      MonadSystemInfo,
      MonadTerminal,
      MonadTime,
      MonadThread,
      MonadThrow
    )
    via (ReaderT e m)

instance
  ( MonadAtomic m,
    MonadTime m,
    MonadThread m
  ) =>
  MonadLogger (NaviT Env m)
  where
  monadLoggerLog loc _src lvl msg = do
    mLogEnv <- asks getLogEnv
    case mLogEnv of
      Just logEnv -> do
        let logQueue = logEnv ^. #logQueue
            logLevel = logEnv ^. #logLevel
        when (logLevel <= lvl) $ do
          formatted <- formatLog formatter lvl msg
          writeTBQueueA' logQueue formatted
      Nothing -> pure ()
    where
      formatter = set' #threadLabel True (defaultLogFormatter loc)

-- | Runs 'NaviT'.
runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr
{-# INLINEABLE runNaviT #-}
