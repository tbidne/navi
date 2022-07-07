module Unit.Navi.Config
  ( tests,
  )
where

import Katip (Severity (..))
import Navi.Config qualified as Config
import Navi.Config.Types (Config (..), LogLoc (..), NoteSystem (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config"
    [ readsExample verifyConfig "examples/config.toml",
      readsExample verifySimple "examples/simple.toml",
      readsExample verifyMultiple "examples/multiple.toml"
    ]
  where
    verifyConfig cfg = do
      DBus @=? cfg ^. #noteSystem
      Just DebugS @=? cfg ^. #logging % #severity
      Nothing @=? cfg ^. #logging % #location
      3 @=? length (cfg ^. #events)

    verifySimple cfg = do
      NotifySend @=? cfg ^. #noteSystem
      Just DebugS @=? cfg ^. #logging % #severity
      Just Stdout @=? cfg ^. #logging % #location
      1 @=? length (cfg ^. #events)

    verifyMultiple cfg = do
      DBus @=? cfg ^. #noteSystem
      Just ErrorS @=? cfg ^. #logging % #severity
      Just DefPath @=? cfg ^. #logging % #location
      1 @=? length (cfg ^. #events)

readsExample :: (Config IORef -> IO ()) -> FilePath -> TestTree
readsExample verifyCfg fp = testCase ("Reads " <> fp) $ do
  eResult <- tryAny $ Config.readConfig @IORef fp
  case eResult of
    Left ex -> assertFailure $ "Reading config failed: " <> displayException ex
    Right cfg -> verifyCfg cfg
