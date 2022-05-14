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
    verifyConfig cfg = do
      DebugS @=? cfg ^. #logging % #severity
      Stdout @=? cfg ^. #logging % #location
      3 @=? length (cfg ^. #events)

    verifyMultiple cfg = do
      ErrorS @=? cfg ^. #logging % #severity
      DefPath @=? cfg ^. #logging % #location
      1 @=? length (cfg ^. #events)

readsExample :: (Config IORef -> IO ()) -> FilePath -> TestTree
readsExample verifyCfg fp = testCase ("Reads " <> fp) $ do
  eResult <- try @_ @SomeException $ Config.readConfig @IORef fp
  case eResult of
    Left ex -> assertFailure $ "Reading config failed: " <> displayException ex
    Right cfg -> verifyCfg cfg
