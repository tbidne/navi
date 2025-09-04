{-# LANGUAGE QuasiQuotes #-}

module Unit.Navi.Config
  ( tests,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import FileSystem.OsPath (unsafeDecode)
import Navi.Config qualified as Config
import Navi.Config.Types
  ( Config,
    LogLoc (DefPath, Stdout),
    NoteSystem (DBus, NotifySend),
  )
import Navi.Event.Types (AnyEvent (MkAnyEvent))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Config"
    [ readsExample verifyConfig [osp|examples/config.toml|],
      readsExample verifySimple [osp|test/unit/simple.toml|],
      readsExample verifyMultiple [osp|test/unit/multiple.toml|]
    ]
  where
    verifyConfig cfg = do
      NotifySend @=? cfg ^. #noteSystem
      Nothing @=? cfg ^. #logging % #severity
      Just Stdout @=? cfg ^. #logging % #location
      verifyEvents 8 (cfg ^. #events)

    verifySimple cfg = do
      NotifySend @=? cfg ^. #noteSystem
      Just LevelDebug @=? cfg ^. #logging % #severity
      Just Stdout @=? cfg ^. #logging % #location
      verifyEvents 1 (cfg ^. #events)

    verifyMultiple cfg = do
      DBus () @=? cfg ^. #noteSystem
      Just LevelError @=? cfg ^. #logging % #severity
      Just DefPath @=? cfg ^. #logging % #location
      verifyEvents 1 (cfg ^. #events)

readsExample :: (Config -> IO ()) -> OsPath -> TestTree
readsExample verifyCfg p =
  testCase ("Reads " <> unsafeDecode p)
    $ Config.readConfig p
    >>= verifyCfg

verifyEvents :: Int -> NonEmpty AnyEvent -> Assertion
verifyEvents expected evts = unless (expected == actual) $ do
  let msg =
        mconcat
          [ "Found ",
            show actual,
            " events, expected ",
            show expected,
            ": ",
            unpackText evtsStr
          ]
  assertFailure msg
  where
    actual = length evts

    evtsStr =
      T.intercalate ", "
        . NE.toList
        . fmap showEvt
        $ evts

    showEvt (MkAnyEvent evt) = evt ^. #name
