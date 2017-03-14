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
lexerInputSample1 = " ( \"Hello\", [a] \"\\\"\" :a {-comment -} ) "

lexerOutputSample1 :: [Token]
lexerOutputSample1 = [
  TokenParenthesis BracketSideOpening,
  TokenString "Hello",
  TokenComma,
  TokenSquareBracket BracketSideOpening,
  TokenExpId (ExpId identifierZero),
  TokenSquareBracket BracketSideClosing,
  TokenString "\"",
  TokenConId (ConId identifierZero),
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

parserInputSample2 :: Text
parserInputSample2 = "exp = :cons (:up 'a') ^0."

parserOutputSample2 :: Prog
parserOutputSample2 = progFromList [
  ( _ExpId . named # unsafeStringToName "exp",
    (_ExpCon . _ConId . named # unsafeStringToName "cons") :@:
    ( (_ExpCon . _ConId . named # unsafeStringToName "up") :@:
      (_ExpVal . _ValueChar # 'a') ) :@:
    (_ExpVar . _Var # 0) ) ]

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
      parse parserInputSample1 @?= Right parserOutputSample1,
    testCase "handles sample-2" $
      parse parserInputSample2 @?= Right parserOutputSample2 ],
  testGroup "Substitution" [ ] ]
