module Unit.Navi.Config
  ( tests,
  )
where

import Katip (Severity (..))
import Navi.Config qualified as Config
import Navi.Config.Types (Config (..), LogLoc (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config"
    [ readsExample verifyConfig "examples/config.toml",
      readsExample verifyMultiple "examples/multiple.toml"
    ]
  where
    verifyConfig cfg =
      cfg ^. #pollInterval == 10
        && cfg ^. #logging % #severity == DebugS
        && cfg ^. #logging % #location == Stdout
        && length (cfg ^. #events) == 2

    verifyMultiple cfg =
      cfg ^. #pollInterval == 10
        && cfg ^. #logging % #severity == ErrorS
        && cfg ^. #logging % #location == DefPath
        && length (cfg ^. #events) == 1

readsExample :: (Config IORef -> Bool) -> FilePath -> TestTree
readsExample verifyCfg fp = testCase ("Reads " <> fp) $ do
  eResult <- try @_ @SomeException $ Config.readConfig @_ @IORef fp
  case eResult of
    Left ex -> assertFailure $ "Reading config failed: " <> displayException ex
    Right cfg -> assertBool "Config verification failed" $ verifyCfg cfg
