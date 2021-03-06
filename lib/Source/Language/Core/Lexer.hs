module Source.Language.Core.Lexer
  ( BracketSide(..)
  , _BracketSideOpening
  , _BracketSideClosing
  , Token(..)
  , _TokenExpId
  , _TokenConId
  , _TokenLam
  , _TokenVar
  , _TokenChar
  , _TokenString
  , _TokenInteger
  , _TokenSquareBracket
  , _TokenParenthesis
  , _TokenComma
  , _TokenDot
  , _TokenEquals
  , _TokenBar
  , _TokenUnknown
  , tokenize
  , tokenize'
  , detokenize
  ) where

import Control.Applicative as A hiding (many, some)
import Control.Lens
import Control.Monad
import Data.Functor
import Data.List as List
import Data.Maybe
import Data.Text as Text
import Data.Text.Lens
import Data.Void
import GHC.Generics (Generic)
import Numeric.Natural
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (charLiteral)

import Source.Identifier
import Source.Language.Core.Syn

data BracketSide =
  BracketSideOpening |
  BracketSideClosing
  deriving (Eq, Ord, Show, Generic)

makePrisms ''BracketSide

withBracketSide :: a -> a -> BracketSide -> a
withBracketSide onOpening onClosing = \case
  BracketSideOpening -> onOpening
  BracketSideClosing -> onClosing

instance Arbitrary BracketSide where
  arbitrary = genericArbitrary
  shrink = genericShrink

newtype UnknownChar = UnknownChar Char
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UnknownChar where
  arbitrary = pure (UnknownChar '\0')

data Token =
  TokenExpId ExpId |
  TokenConId ConId |
  TokenPrimId Name |
  TokenLam Name |
  TokenVar (Var Name) |
  TokenChar Char |
  TokenString String |
  TokenInteger Integer |
  TokenSquareBracket BracketSide |
  TokenParenthesis BracketSide |
  TokenComma |
  TokenDot |
  TokenEquals |
  TokenBar |
  TokenUnknown UnknownChar
  deriving (Eq, Ord, Show, Generic)

makePrisms ''Token

instance Arbitrary Token where
  arbitrary = genericArbitrary
  shrink = genericShrink

tokenRender :: Token -> Text
tokenRender = \case
  TokenExpId ident ->        nameToText (ident ^. _ExpId . named)
  TokenConId ident -> ":" <> nameToText (ident ^. _ConId . named)
  TokenPrimId name -> "#" <> nameToText name
  TokenLam name -> ">" <> nameToText name
  TokenVar (Var b n) ->
    Text.replicate (fromIntegral n + 1) "^" <>
    nameToText b
  TokenChar c -> _Text . _Show # c
  TokenString s -> _Text . _Show # s
  TokenInteger n -> _Text . _Show # n
  TokenSquareBracket bs -> withBracketSide "[" "]" bs
  TokenParenthesis bs -> withBracketSide "(" ")" bs
  TokenComma -> ","
  TokenDot -> "."
  TokenEquals -> "="
  TokenBar -> "|"
  TokenUnknown (UnknownChar c) -> Text.singleton c

detokenize :: [Token] -> Text
detokenize = Text.unwords . List.map tokenRender

type Lexer a = Parsec Void Text a

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
pUnknown = TokenUnknown . UnknownChar <$> anySingle

pSkip :: Lexer ()
pSkip = skipMany (void spaceChar <|> pComment)

pToken' :: Lexer Token
pToken' = choice [
  pPunct,
  TokenLam <$> pLam,
  TokenInteger <$> pInteger,
  TokenConId <$> pConId,
  TokenExpId <$> pExpId,
  TokenPrimId <$> pPrimId,
  TokenVar <$> pVar,
  TokenString <$> pString,
  TokenChar <$> pChar ] <?> "token"

pPunct :: Lexer Token
pPunct =
  char '[' $> TokenSquareBracket BracketSideOpening <|>
  char ']' $> TokenSquareBracket BracketSideClosing <|>
  char '(' $> TokenParenthesis BracketSideOpening <|>
  char ')' $> TokenParenthesis BracketSideClosing <|>
  char ',' $> TokenComma <|>
  char '.' $> TokenDot <|>
  char '=' $> TokenEquals <|>
  char '|' $> TokenBar

pInteger :: Lexer Integer
pInteger = pSign <*> (toInteger <$> pNatural)
  where
    pSign =
      char '+' $> id <|>
      char '-' $> negate <|>
      pure id

pNatural :: Lexer Natural
pNatural = digitsToNumber <$> some pDigit
  where
    digits = "0123456789"
    base = fromIntegral (List.length digits)
    digitsToNumber = List.foldl1' (\acc d -> d + acc * base)
    pDigit = choice $ List.zipWith (<$) [0..] (char <$> digits)

closing :: Lexer a -> Lexer ()
closing p = void p <|> eof

pName :: Lexer Name
pName = unsafeStringToName <$> some (oneOf alphabet)

pExpId :: Lexer ExpId
pExpId = ExpId . nameToIdentifier <$> pName

pConId :: Lexer ConId
pConId = ConId . nameToIdentifier <$> (char ':' *> pName)

pPrimId :: Lexer Name
pPrimId = char '#' *> pName

pLam :: Lexer Name
pLam = char '>' *> pName

pVar :: Lexer (Var Name)
pVar = char '^' *> do
  k <- fromIntegral . List.length <$> many (char '^')
  b <- pName
  pure (Var b k)

pString :: Lexer String
pString =
  char '\"' *>
  manyTill (charLiteral <|> anySingle) (closing (char '\"'))

pChar :: Lexer Char
pChar = between (char '\'') (closing (char '\'')) (charLiteral <|> anySingle)

pComment :: Lexer ()
pComment = void comment <?> "comment"
  where
    comment = open *> manyTill anySingle close
    open    = void (string "{-")
    close   = closing (string "-}")
