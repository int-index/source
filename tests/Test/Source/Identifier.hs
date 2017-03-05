module Test.Source.Identifier where

import Test.Tasty
import Test.Util

import Source.Identifier

testIdentifier :: TestTree
testIdentifier = testGroup "Identifier" [
  testIso "isomorphic to Name" named ]
