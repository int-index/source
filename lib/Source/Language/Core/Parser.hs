module Source.Language.Core.Parser where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Text
import Text.Earley

import Source.Identifier
import Source.Language.Core.Lexer
import Source.Language.Core.Syn
import Source.Value

tok :: Getting (First a) Token a -> Prod r e Token a
tok p = terminal (preview p)

inBrackets ::
  Getting (First ()) Token BracketSide ->
  Prod r e Token a ->
  Prod r e Token a
inBrackets p r =
  tok (p . _BracketSideOpening) *> r <* tok (p . _BracketSideClosing)

tokPrimId :: Text -> a -> Prod r e Token a
tokPrimId t a = terminal (\case
  TokenPrimId name | nameToText name == t -> Just a
  _ -> Nothing)

gProg :: Grammar r (Prod r Text Token (Prog (BndrNi Name) ExpId))
gProg = mdo
  ntExpId <- rule $ tok _TokenExpId <?> "expression identifier"
  ntConId <- rule $ tok _TokenConId <?> "constructor identifier"
  ntPrim <- rule $
    tokPrimId "add" PrimAdd <*> ntExp' <*> ntExp' <|>
    tokPrimId "subtract" PrimSubtract <*> ntExp' <*> ntExp'
  ntValue <- rule $
    review _ValueInteger <$> tok _TokenInteger <|>
    review _ValueChar <$> tok _TokenChar <|>
    review _ValueString <$> tok _TokenString <?> "value literal"
  ntVar <- rule $ tok _TokenVar <?> "variable name"
  ntLam <- rule $
    (\n e -> _ExpNiLam # (n, e)) <$>
    tok _TokenLam <*>
    ntExp
  ntExp' <- rule $
    review (_ExpPrim . _PrimValue) <$> ntValue <|>
    review _ExpCon <$> ntConId <|>
    review _ExpNiVar <$> ntVar <|>
    (inBrackets _TokenParenthesis ntExp <?> "parenthesized expression")
  ntExp <- rule $
    ntExp' <|>
    (:@:) <$> ntExp <*> ntExp' <|>
    review _ExpPrim <$> ntPrim <|>
    (ntLam <?> "lambda function")
  ntDef <- rule $
    (,) <$>
      ntExpId <* (tok _TokenEquals <?> "=") <*>
      ntExp <* (tok _TokenDot <?> "end of definition")
  ntProg <- rule $ progFromList <$> many ntDef
  return ntProg

pProg :: Parser Text [Token] (Prog (BndrNi Name) ExpId)
pProg = parser gProg

parse :: Text -> Either (Report Text [Token]) (Prog (BndrNi Name) ExpId)
parse = toEither . fullParses pProg . tokenize
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a
