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

gProg :: Grammar r (Prod r Text Token Prog)
gProg = mdo
  ntExpId <- rule $ tok _TokenExpId <?> "expression identifier"
  ntConId <- rule $ tok _TokenConId <?> "constructor identifier"
  ntVar <- rule $ tok _TokenVar <?> "variable index"
  ntExp' <- rule $
    ExpVal . ValueInteger <$> tok _TokenInteger <|>
    ExpVal . ValueChar <$> tok _TokenChar <|>
    ExpVal . ValueList . fmap ValueChar <$> tok _TokenString <|>
    ExpCon <$> ntConId <|>
    ExpVar <$> ntVar <|>
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

pProg :: Parser Text [Token] Prog
pProg = parser gProg

parse :: Text -> Either (Report Text [Token]) Prog
parse = toEither . fullParses pProg . tokenize
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a
