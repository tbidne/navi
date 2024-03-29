module Unit.Navi.Config
  ( tests,
  )
where

import Effects.FileSystem.Utils (unsafeEncodeFpToOs)
import Navi.Config qualified as Config
import Navi.Config.Types
  ( Config,
    LogLoc (DefPath, Stdout),
    NoteSystem (DBus, NotifySend),
  )
import System.IO (FilePath)
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
      Just LevelDebug @=? cfg ^. #logging % #severity
      Nothing @=? cfg ^. #logging % #location
      5 @=? length (cfg ^. #events)

    verifySimple cfg = do
      NotifySend @=? cfg ^. #noteSystem
      Just LevelDebug @=? cfg ^. #logging % #severity
      Just Stdout @=? cfg ^. #logging % #location
      1 @=? length (cfg ^. #events)

    verifyMultiple cfg = do
      DBus @=? cfg ^. #noteSystem
      Just LevelError @=? cfg ^. #logging % #severity
      Just DefPath @=? cfg ^. #logging % #location
      1 @=? length (cfg ^. #events)

readsExample :: (Config -> IO ()) -> FilePath -> TestTree
readsExample verifyCfg fp =
  testCase ("Reads " <> fp)
    $ Config.readConfig p
    >>= verifyCfg
  where
    p = unsafeEncodeFpToOs fp
