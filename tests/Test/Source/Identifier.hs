module Test.Source.Identifier where

import Test.Tasty
import Test.Util

import Source.Identifier

testIdentifier :: TestTree
testIdentifier = testGroup "Identifier" [
  testIso "Identifier" "Name" named,
  testMonotonic "identifierToName" identifierToName,
  testMonotonic "nameToIdentifier" nameToIdentifier ]
