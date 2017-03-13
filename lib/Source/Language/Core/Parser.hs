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

gProg :: Grammar r (Prod r Text Token Prog)
gProg = mdo
  ntIdent <- rule $ tok _TokenIdentifier
  ntExpId <- rule $ ExpId <$> ntIdent <?>
    "expression identifier"
  ntExp <- rule $ ExpVal . ValueInteger <$> tok _TokenInteger
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
