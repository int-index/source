module Test.Util where

import Control.Lens
import Test.Tasty
import Test.Tasty.QuickCheck

testIso ::
  (Eq a, Show a, Arbitrary a) =>
  (Eq b, Show b, Arbitrary b) =>
  String ->
  String ->
  Iso' a b ->
  TestTree
testIso objNameA objNameB i =
  testGroup testName [
    testProperty "from . to = id" $
      \x -> (review i . view i) x == x,
    testProperty "to . from = id" $
      \x -> (view i . review i) x == x ]
  where
    testName = objNameA ++ " is isomorphic to " ++ objNameB

testMonotonic ::
  (Ord a, Show a, Arbitrary a, Ord b) =>
  String ->
  (a -> b) ->
  TestTree
testMonotonic objName f =
  testProperty testName $
    \x y -> compare x y == compare (f x) (f y)
  where
    testName = objName ++ " is monotonic"
