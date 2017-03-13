module Test.Source.Language (testLanguage) where

import Control.Applicative
import Control.Lens
import Data.Maybe
import Data.String
import Data.Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Source.Identifier
import Source.Language.Core.Lexer
import Source.Language.Core.Parser
import Source.Language.Core.Syn
import Source.Value

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

parserInputSample1 :: Text
parserInputSample1 = "n = -13. m= 42.\nk =+7. p=0."

parserOutputSample1 :: Prog
parserOutputSample1 = progFromList [
  intDef "n" -13, intDef "m" 42, intDef "k" 7, intDef "p" 0 ]
  where
    intDef s n =
      ( _ExpId . named # unsafeStringToName s,
        _ExpVal . _ValueInteger # n )

testLanguage :: TestTree
testLanguage = testGroup "Language" [
  testGroup "Lexer" [
    testProperty "accepts any input" $
      isJust . tokenize' . fromString,
    testProperty "handles valid input" $
      liftA2 (==) (tokenize . detokenize) id,
    testCase "handles sample-1" $
      tokenize lexerInputSample1 @?= lexerOutputSample1 ],
  testGroup "Parser" [
    testCase "handles sample-1" $
      parse parserInputSample1 @?= Right parserOutputSample1 ],
  testGroup "Substitution" [ ] ]
