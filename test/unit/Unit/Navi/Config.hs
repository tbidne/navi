{-# LANGUAGE QuasiQuotes #-}

module Unit.Navi.Config
  ( tests,
  )
where

import FileSystem.OsPath (unsafeDecode)
import Navi.Config qualified as Config
import Navi.Config.Types
  ( Config,
    LogLoc (DefPath, Stdout),
    NoteSystem (DBus, NotifySend),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config"
    [ readsExample verifyConfig [osp|examples/config.toml|],
      readsExample verifySimple [osp|examples/simple.toml|],
      readsExample verifyMultiple [osp|examples/multiple.toml|]
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

readsExample :: (Config -> IO ()) -> OsPath -> TestTree
readsExample verifyCfg p =
  testCase ("Reads " <> unsafeDecode p)
    $ Config.readConfig p
    >>= verifyCfg
