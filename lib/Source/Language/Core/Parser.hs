module Source.Language.Core.Parser where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Text
import Text.Earley

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

gProg :: Grammar r (Prod r Text Token (Prog (BndrNi ()) ExpId))
gProg = mdo
  ntExpId <- rule $ tok _TokenExpId <?> "expression identifier"
  ntConId <- rule $ tok _TokenConId <?> "constructor identifier"
  ntVar <- rule $ tok _TokenVar <?> "variable index"
  ntExp' <- rule $
    review (_ExpPrim . _PrimValue . _ValueInteger) <$> tok _TokenInteger <|>
    review (_ExpPrim . _PrimValue . _ValueChar) <$> tok _TokenChar <|>
    review (_ExpPrim . _PrimValue . _ValueString) <$> tok _TokenString <|>
    review _ExpCon <$> ntConId <|>
    review _ExpNiVar <$> (VarNi () <$> ntVar) <|>
    inBrackets _TokenParenthesis ntExp
  ntExp <- rule $
    ntExp' <|>
    (:@:) <$> ntExp <*> ntExp'
  ntDef <- rule $
    (,) <$>
      ntExpId <* (tok _TokenEquals <?> "=") <*>
      ntExp <* (tok _TokenDot <?> "end of definition")
  ntProg <- rule $ progFromList <$> many ntDef
  return ntProg

pProg :: Parser Text [Token] (Prog (BndrNi ()) ExpId)
pProg = parser gProg

parse :: Text -> Either (Report Text [Token]) (Prog (BndrNi ()) ExpId)
parse = toEither . fullParses pProg . tokenize
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a
