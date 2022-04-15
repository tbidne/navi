module Unit.Navi.Config
  ( tests,
  )
where

import Navi.Config qualified as Config
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config"
    [ readsExample "examples/config.toml",
      readsExample "examples/multiple.toml"
    ]

readsExample :: FilePath -> TestTree
readsExample fp = testCase ("Reads " <> fp) $ do
  eResult <- try @_ @SomeException $ Config.readConfig @_ @IORef fp
  case eResult of
    Left ex -> assertFailure $ "Reading config failed: " <> displayException ex
    Right _ -> pure ()
