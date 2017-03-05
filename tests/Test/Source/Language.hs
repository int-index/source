module Test.Source.Language (testLanguage) where

import Control.Applicative
import Data.Maybe
import Data.String
import Data.Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Source.Identifier
import Source.Language.Core.Lexer

lexerInputSample1 :: Text
lexerInputSample1 = " ( \"Hello\", [a] {-comment -} ) "

lexerOutputSample1 :: [Token]
lexerOutputSample1 = [
  TokenParenthesis BracketSideOpening,
  TokenString "Hello",
  TokenComma,
  TokenSquareBracket BracketSideOpening,
  TokenIdentifier identifierZero,
  TokenSquareBracket BracketSideClosing,
  TokenParenthesis BracketSideClosing ]

testLanguage :: TestTree
testLanguage = testGroup "Language" [
  testGroup "Lexer" [
    testProperty "accepts any input" $
      isJust . tokenize' . fromString,
    testProperty "handles valid input" $
      liftA2 (==) (tokenize . detokenize) id,
    testCase "handles sample-1" $
      tokenize lexerInputSample1 @?= lexerOutputSample1 ],
  testGroup "Parser" [ ],
  testGroup "Substitution" [ ] ]
