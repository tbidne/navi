{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Concurrent qualified as CC
import Control.Monad.Reader (ReaderT (ReaderT))
import Effects.Concurrent.Async qualified as Async
import FileSystem.OsPath (decodeLenient)
import Navi qualified
import Navi.Effects (MonadNotify, MonadSystemInfo)
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Env.Core
  ( CoreEnvField (MkCoreEnvField),
    Env,
    HasEvents,
    HasLogEnv,
    HasNoteQueue,
  )
import Navi.NaviT (NaviT (MkNaviT))
import Navi.Prelude
import Navi.Runner qualified as Runner
import System.Environment qualified as SysEnv
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.IO (FilePath)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

newtype TestEnv = MkTestEnv
  { coreEnv :: Env
  }

instance
  (k ~ An_Iso, a ~ Env, b ~ Env) =>
  LabelOptic "coreEnv" k TestEnv TestEnv a b
  where
  labelOptic = iso (\(MkTestEnv a1) -> a1) MkTestEnv
  {-# INLINE labelOptic #-}

deriving via (CoreEnvField TestEnv) instance HasEvents TestEnv

deriving via (CoreEnvField TestEnv) instance HasLogEnv TestEnv

deriving via (CoreEnvField TestEnv) instance HasNoteQueue TestEnv

main :: IO ()
main = guardOrElse' "RUN_E2E" ExpectEnvSet runTests dontRun
  where
    runTests = do
      defaultMain
        $ testGroup
          "End-to-end tests"
          [ runNotifs
          ]

    dontRun = putStrLn "*** End-to-end tests disabled. Enable with RUN_E2E=1 ***"

runNotifs :: TestTree
runNotifs = testCase "Runs simple example" $ do
  runNaviTest

runNaviTest :: IO ()
runNaviTest = do
  let action = SysEnv.withArgs args $ Runner.withEnv $ \env -> do
        runTestIO Navi.runNavi (MkTestEnv env)

  Async.race_
    (CC.threadDelay 10_000_000)
    action
  where
    args = ["-c", configPath]

newtype TestIO a = MkTestIO (ReaderT TestEnv IO a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIORef,
      MonadHandleWriter,
      MonadMask,
      MonadReader TestEnv,
      MonadSTM,
      MonadSystemInfo,
      MonadTerminal,
      MonadThread,
      MonadThrow
    )

instance MonadLogger TestIO where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadNotify TestIO where
  sendNote = hoistNaviT . sendNote

hoistNaviT :: NaviT Env IO a -> TestIO a
hoistNaviT (MkNaviT r) = MkTestIO $ ReaderT $ \env -> runReaderT r (env ^. #coreEnv)

instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
  LabelOptic "namespace" k TestEnv TestEnv x y
  where
  labelOptic =
    lensVL $ \f env ->
      fmap
        (const env)
        (f "")
  {-# INLINE labelOptic #-}

runTestIO :: TestIO a -> TestEnv -> IO a
runTestIO (MkTestIO rdr) = runReaderT rdr

configPath :: FilePath
configPath =
  decodeLenient
    $ [osp|test|]
    </> [osp|e2e|]
    </> [osp|config|]
    <> suffix
    <> [osp|.toml|]

{- ORMOLU_DISABLE -}

suffix :: OsPath
suffix =
#if OSX
  [osp|_osx|]
#else
  [osp|_linux|]
#endif

{- ORMOLU_ENABLE -}
