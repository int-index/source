module Source.Language.Core.Syn
  ( execute
  , reduce
  ) where

import Control.Lens
import Control.Monad.State
import Control.Exception
import GHC.Generics as Generic
import Data.Serialize as Cereal
import Data.EnumMap.Lazy as EnumMapL
import Data.Maybe

import Source.Identifier
import Source.Value
import Source.Model
import Source.Util

newtype ExpId = ExpId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize)

makePrisms ''ExpId

newtype ConId = ConId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize)

makePrisms ''ConId

-- A de Bruijn index.
newtype Var = Var Int
  deriving (Show, Eq, Generic)

instance Serialize Var

data Prim a =
  PrimAdd      a a {- integer addition -} |
  PrimSubtract a a {- integer subtraction -}
  deriving (Show, Eq, Functor, Generic)

instance Serialize a => Serialize (Prim a)

data Exp =
  ExpVal Value |
  ExpCon ConId |
  ExpPrim (Prim Exp) |
  ExpRef ExpId |
  Exp :@: Exp |
  ExpVar Var |
  ExpLam Exp
  deriving (Show, Eq, Generic)

instance Serialize Exp

newtype Prog = Prog (EnumMapL ExpId Exp)
  deriving (Show, Eq, Generic)

instance Serialize Prog

data ExpressionNotFound = ExpressionNotFound ExpId
  deriving (Show)

instance Exception ExpressionNotFound

data UnboundVariable = UnboundVariable Var Ctx
  deriving (Show)

instance Exception UnboundVariable

progLookup :: ExpId -> Prog -> Maybe Exp
progLookup expId (Prog exps) = EnumMapL.lookup expId exps

progGet :: ExpId -> Prog -> Exp
progGet expId prog =
  fromMaybe (throw $ ExpressionNotFound expId) $
    progLookup expId prog

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
ctxLookup (Var n) (Ctx es) = listToMaybe $ drop n es

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

{-
expSample :: Exp
expSample = (fn :@: arg1) :@: arg2
  where
    fn = ExpLam (ExpLam (ExpPrim (PrimSubtract (ExpVar (Var 0)) (ExpVar (Var 1)))))
    arg1 = ExpVal (ValueInteger 15)
    arg2 = ExpVal (ValueInteger 30)

expSample2 :: Int -> Exp
expSample2 n = expChurchInt n :@: (fn :@: ExpVal (ValueInteger 1)) :@: ExpVal (ValueInteger 0)
  where
    fn = ExpLam (ExpLam (ExpPrim (PrimAdd (ExpVar (Var 0)) (ExpVar (Var 1)))))

expChurchInt :: Int -> Exp
expChurchInt n = ExpLam (ExpLam (Prelude.foldr (:@:) (ExpVar (Var 0)) (replicate n (ExpVar (Var 1))) ))
-}
