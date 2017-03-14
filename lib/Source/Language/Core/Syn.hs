module Source.Language.Core.Syn
  ( ExpId(..)
  , _ExpId
  , ConId(..)
  , _ConId
  , Var(..)
  , _Var
  , Prim(..)
  , Exp(..)
  , _ExpVal
  , _ExpCon
  , _ExpPrim
  , _ExpRef
  , (.:@:)
  , _ExpVar
  , _ExpLam
  , Prog(..)
  , _Prog
  , progEmpty
  , progFromList
  , progLookup
  , progGet
  , ExpressionNotFound(..)
  ) where

import Control.Exception
import Control.Lens
import Data.EnumMap.Lazy as EnumMapL
import Data.Maybe
import Data.Serialize as Cereal
import GHC.Generics as Generic
import Numeric.Natural
import Test.QuickCheck as QC

import Source.Identifier
import Source.Util
import Source.Value

newtype ExpId = ExpId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''ExpId

newtype ConId = ConId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''ConId

-- A de Bruijn index.
newtype Var = Var Natural
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''Var

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

makePrisms ''Exp

instance Serialize Exp

newtype Prog = Prog (EnumMapL ExpId Exp)
  deriving (Show, Eq, Generic)

makePrisms ''Prog

instance Serialize Prog

progEmpty :: Prog
progEmpty = _Prog # EnumMapL.empty

progFromList :: [(ExpId, Exp)] -> Prog
progFromList defs = _Prog # EnumMapL.fromList defs

progLookup :: ExpId -> Prog -> Maybe Exp
progLookup expId prog = EnumMapL.lookup expId (prog ^. _Prog)

data ExpressionNotFound = ExpressionNotFound ExpId
  deriving (Show)

instance Exception ExpressionNotFound

progGet :: ExpId -> Prog -> Exp
progGet expId prog =
  fromMaybe (throw $ ExpressionNotFound expId) $
    progLookup expId prog

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
