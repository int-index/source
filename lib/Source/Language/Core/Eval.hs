module Source.Language.Core.Eval
  ( execute
  , reduce
  ) where

import Control.Monad.State
import Control.Exception
import Data.Serialize as Cereal
import GHC.Generics as Generic
import Data.Maybe

import Source.Model
import Source.Value
import Source.Language.Core.Syn

data UnboundVariable = UnboundVariable Var Ctx
  deriving (Show)

instance Exception UnboundVariable

data CtxElem = CtxElemExp Exp | CtxElemDummy
  deriving (Show, Eq, Generic)

instance Serialize CtxElem

fromCtxElem :: Var -> CtxElem -> Exp
fromCtxElem _ (CtxElemExp e) = e
fromCtxElem v CtxElemDummy = ExpVar v

newtype Ctx = Ctx [CtxElem]
  deriving (Show, Eq, Generic)

instance Serialize Ctx

ctxLookup :: Var -> Ctx -> Maybe CtxElem
ctxLookup (Var n) (Ctx es) = listToMaybe $ drop (fromIntegral n) es

ctxGet :: Var -> Ctx -> Exp
ctxGet v ctx =
  fromCtxElem v . fromMaybe (throw $ UnboundVariable v ctx) $
    ctxLookup v ctx

ctxEmpty :: Ctx
ctxEmpty = Ctx []

ctxAppend :: Exp -> Ctx -> Ctx
ctxAppend e (Ctx es) = Ctx (CtxElemExp e:es)

ctxExtend :: Ctx -> Ctx
ctxExtend (Ctx es) = Ctx (CtxElemDummy:es)

execute ::
  ExpId {- main action -} ->
  Prog ->
  State Model ()
execute _ _ = return ()

reduce :: Prog -> Exp -> Exp
reduce prog = reduce' ctxEmpty
  where
    reduce' :: Ctx -> Exp -> Exp
    reduce' ctx = \case
      e@ExpVal{} -> e
      e@ExpCon{} -> e
      ExpVar v -> ctxGet v ctx
      ExpRef expId -> reduce' ctx $ progGet expId prog
      ExpPrim p -> reducePrim (reduce' ctx <$> p)
      ExpLam e -> ExpLam (reduce' (ctxExtend ctx) e)
      -- Beta-reduction
      f :@: a ->
        let
          f' = reduce' ctx f
          a' = reduce' ctx a
        in
          case f' of
            ExpLam e -> reduce' (ctxAppend a' ctx) e
            _ -> f' :@: a'

reducePrim :: Prim Exp -> Exp
-- Integer addition.
reducePrim (PrimAdd a b) |
  ExpVal (ValueInteger n) <- a,
  ExpVal (ValueInteger m) <- b =
    ExpVal (ValueInteger (n + m))
-- Integer subtraction.
reducePrim (PrimSubtract a b) |
  ExpVal (ValueInteger n) <- a,
  ExpVal (ValueInteger m) <- b =
    ExpVal (ValueInteger (n - m))
-- Default case.
reducePrim p = ExpPrim p

