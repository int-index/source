module Test.Source.Language (testLanguage) where

import Control.Applicative
import Control.Lens
import Data.List as List
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
import Source.Language.Core.Eval
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

parserOutputSample1 :: Prog (BndrNi ()) ExpId
parserOutputSample1 = progFromList [
  intDef "n" -13, intDef "m" 42, intDef "k" 7, intDef "p" 0 ]
  where
    intDef s n =
      ( _ExpId . named # unsafeStringToName s,
        _ExpPrim . _PrimValue . _ValueInteger # n )

parserInputSample2 :: Text
parserInputSample2 = "exp = :cons (:up 'a') ^0."

parserOutputSample2 :: Prog (BndrNi ()) ExpId
parserOutputSample2 = progFromList [
  ( _ExpId . named # unsafeStringToName "exp",
    (_ExpCon . _ConId . named # unsafeStringToName "cons") :@:
    ( (_ExpCon . _ConId . named # unsafeStringToName "up") :@:
      (_ExpPrim . _PrimValue . _ValueChar # 'a') ) :@:
    (_ExpNiVar # VarNi () 0) ) ]

expChurchInt :: Int -> ExpHo b ref
expChurchInt n =
  lam $ \f ->
  lam $ \a ->
    List.foldr (:@:) a $ List.replicate n f

expChurchInt' :: Int -> Integer -> ExpHo b ref
expChurchInt' n k =
  expChurchInt n :@: eAdd :@: ExpInteger k
  where
    eAdd = lam $ \a -> ExpPrim (PrimAdd (ExpInteger 1) a)

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
  testGroup "Evaluation" [
    testProperty "handles church-encoded" $
      \(Positive n) k ->
        reduce progEmpty (expFromP @() (expChurchInt' n k)) ==
        ExpInteger (fromIntegral n + k)
    ] ]
