-- | Prelude for integration suite
module Integration.Prelude
  ( module X,
  )
where

import Control.Concurrent.STM.TVar as X (TVar, newTVar, readTVar, writeTVar)
import Data.Functor.Identity as X (Identity)
import Effects.Concurrent.STM as X
  ( atomically,
    modifyTVarA',
    newTVarA,
    readTVarA,
    writeTVarA,
  )
import Navi.Prelude as X
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))
