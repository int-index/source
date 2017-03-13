module Source.Language.Core.Lexer
  ( BracketSide(..)
  , Token(..)
  , _TokenIdentifier
  , _TokenChar
  , _TokenString
  , _TokenInteger
  , _TokenSquareBracket
  , _TokenParenthesis
  , _TokenComma
  , _TokenDot
  , _TokenEquals
  , _TokenUnknown
  , tokenize
  , tokenize'
  , detokenize
  ) where

import Control.Applicative as A
import Control.Lens
import Control.Monad
import Data.Functor
import Data.List as List
import Data.Maybe
import Data.Text as Text
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Lexer (charLiteral)

import Source.Identifier

data BracketSide =
  BracketSideOpening |
  BracketSideClosing
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary BracketSide where
  arbitrary = genericArbitrary
  shrink = genericShrink

newtype UnknownChar = UnknownChar Char
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UnknownChar where
  arbitrary = pure (UnknownChar '\0')

data Token =
  TokenIdentifier Identifier |
  TokenChar Char |
  TokenString String |
  TokenInteger Integer |
  TokenSquareBracket BracketSide |
  TokenParenthesis BracketSide |
  TokenComma |
  TokenDot |
  TokenEquals |
  TokenUnknown UnknownChar
  deriving (Eq, Ord, Show, Generic)

makePrisms ''Token

instance Arbitrary Token where
  arbitrary = genericArbitrary
  shrink = genericShrink

tokenRender :: Token -> Text
tokenRender = \case
  TokenIdentifier ident -> (Text.pack . nameToString) (ident ^. named)
  TokenChar c -> Text.pack (show c)
  TokenString s -> Text.pack (show s)
  TokenInteger n -> Text.pack (show n)
  TokenSquareBracket bs -> case bs of
    BracketSideOpening -> "["
    BracketSideClosing -> "]"
  TokenParenthesis bs -> case bs of
    BracketSideOpening -> "("
    BracketSideClosing -> ")"
  TokenComma -> ","
  TokenDot -> "."
  TokenEquals -> "="
  TokenUnknown (UnknownChar c) -> Text.singleton c

detokenize :: [Token] -> Text
detokenize = Text.unwords . List.map tokenRender

data DiscardErr = DiscardErr
  deriving (Eq, Ord, Show, Generic)

instance ErrorComponent DiscardErr where
  representFail _ = DiscardErr
  representIndentation _ _ _ = DiscardErr

instance ShowErrorComponent DiscardErr where
  showErrorComponent = show

type Lexer a = Parsec DiscardErr Text a

tokenize :: Text -> [Token]
tokenize = fromMaybe noTokenErr . tokenize'
  where
    noTokenErr =
      error "tokenize: no token could be consumed. This is a bug"

tokenize' :: Text -> Maybe [Token]
tokenize' = parseMaybe (between pSkip eof (many pToken))

pToken :: Lexer Token
pToken = (try pToken' <|> pUnknown) <* pSkip

pUnknown :: Lexer Token
pUnknown = TokenUnknown . UnknownChar <$> anyChar

pSkip :: Lexer ()
pSkip = skipMany (void spaceChar <|> pComment)

pToken' :: Lexer Token
pToken' = choice [
  pPunct,
  pInteger,
  pName,
  pString,
  pChar ] <?> "token"

pPunct :: Lexer Token
pPunct =
  char '[' $> TokenSquareBracket BracketSideOpening <|>
  char ']' $> TokenSquareBracket BracketSideClosing <|>
  char '(' $> TokenParenthesis BracketSideOpening <|>
  char ')' $> TokenParenthesis BracketSideClosing <|>
  char ',' $> TokenComma <|>
  char '.' $> TokenDot <|>
  char '=' $> TokenEquals

pInteger :: Lexer Token
pInteger = TokenInteger <$> (mkInteger <$> pSign <*> pNatural)
  where
    digits = "0123456789"
    base = fromIntegral (List.length digits)
    digitsToNumber = List.foldl1' (\acc d -> d + acc * base)

    pSign =
      char '+' $> True  <|>
      char '-' $> False <|>
      pure True

    pDigit = choice $ List.zipWith (<$) [0..] (char <$> digits)

    pNatural =
      digitsToNumber <$> some pDigit

    mkInteger sign nat =
      case sign of
        True  -> nat
        False -> negate nat

closing :: Lexer a -> Lexer ()
closing p = void p <|> eof

pName :: Lexer Token
pName = TokenIdentifier . nameToIdentifier . unsafeStringToName <$>
  some (oneOf alphabet)

pString :: Lexer Token
pString = TokenString <$>
  (char '\"' *> manyTill (charLiteral <|> anyChar) (closing (char '\"')))

pChar :: Lexer Token
pChar = TokenChar <$>
  between (char '\'') (closing (char '\'')) (charLiteral <|> anyChar)

pComment :: Lexer ()
pComment = void comment <?> "comment"
  where
    comment = open *> manyTill anyChar close
    open    = void (string "{-")
    close   = closing (string "-}")
