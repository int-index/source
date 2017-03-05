module Main where

import Test.Tasty

import Test.Source.Identifier
import Test.Source.Language

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Source" [
  testIdentifier,
  testLanguage ]
