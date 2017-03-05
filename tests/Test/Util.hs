module Test.Util where

import Control.Applicative
import Control.Lens
import Test.Tasty
import Test.Tasty.QuickCheck

testIso ::
  (Eq a, Show a, Arbitrary a) =>
  (Eq b, Show b, Arbitrary b) =>
  TestName ->
  Iso' a b ->
  TestTree
testIso testName i =
  testGroup testName [
    testProperty "from . to = id" $
      liftA2 (==) id (review i . view i),
    testProperty "to . from = id" $
      liftA2 (==) id (view i . review i) ]

