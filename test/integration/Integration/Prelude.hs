-- | Prelude for integration suite
module Integration.Prelude
  ( module X,
  )
where

import Data.Functor.Identity as X (Identity (..))
import Navi.Prelude as X
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))
