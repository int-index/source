module Source.Language.Core.Syn
  ( ConId(..)
  , _ConId
  , Prim(..)
  , _PrimValue
  , _PrimAdd
  , _PrimSubtract
  , Var(..)
  , BndNi(..)
  , _BndNiVar
  , _BndNiLam
  , BndHo(..)
  , _BndHoVar
  , _BndHoLam
  , Exp(.., ExpVar, ExpLam, ExpInteger)
  , ExpNi
  , ExpHo
  , _ExpCon
  , _ExpPrim
  , _ExpRef
  , (.:@:)
  , _ExpBnd
  , _ExpVar
  , _ExpLam
  , _ExpInteger
  , expFromP
  , lam
  , ExpId(..)
  , _ExpId
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
import Data.Constraint as Constraint
import Data.Constraint.Lifting
import Data.Kind
import Data.Map as Map
import Data.Maybe
import Data.Serialize as Cereal
import GHC.Generics as Generic
import Numeric.Natural
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary.Generic

import Source.Identifier
import Source.Value
import Source.Util ()

newtype ConId = ConId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''ConId

data Prim a =
  PrimValue Value {- value constant -} |
  PrimAdd      a a {- integer addition -} |
  PrimSubtract a a {- integer subtraction -} |
  PrimWithNat  a a a {- natural number eliminator -}
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

makePrisms ''Prim

instance Serialize a => Serialize (Prim a)

instance Arbitrary a => Arbitrary (Prim a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Var b = Var b Natural
  deriving (Show, Eq, Ord, Generic)

instance Serialize b => Serialize (Var b)

instance Arbitrary b => Arbitrary (Var b) where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Binding structure for named variables with de Bruijn indices.
-- "Ni" stands for "named, indexed".
data BndNi b a =
  BndNiVar (Var b) |
  BndNiLam b a
  deriving (Eq, Show, Functor, Generic)

makePrisms ''BndNi

instance (Serialize b, Serialize a) => Serialize (BndNi b a)

instance (Arbitrary b, Arbitrary a) => Arbitrary (BndNi b a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Eq b => Lifting Eq (BndNi b) where
  lifting = Sub Dict

instance Show b => Lifting Show (BndNi b) where
  lifting = Sub Dict

instance Serialize b => Lifting Serialize (BndNi b) where
  lifting = Sub Dict

-- | Binding structure for PHOAS.
-- "Ho" stands for "higher-order".
data BndHo b a =
  BndHoVar b |
  BndHoLam (b -> a)
  deriving (Functor, Generic)

makePrisms ''BndHo

class Bnd f where
  type BndVar f :: Type
  type BndLam f :: Type -> Type
  _BndVar :: Prism' (f a) (BndVar f)
  _BndLam :: Prism' (f a) (BndLam f a)

instance Bnd (BndNi b) where
  type BndVar (BndNi b) = Var b
  type BndLam (BndNi b) = (,) b
  _BndVar = _BndNiVar
  _BndLam = _BndNiLam

instance Bnd (BndHo b) where
  type BndVar (BndHo b) = b
  type BndLam (BndHo b) = (->) b
  _BndVar = _BndHoVar
  _BndLam = _BndHoLam

data Exp f ref =
  ExpCon ConId |
  ExpPrim (Prim (Exp f ref)) |
  ExpRef ref |
  Exp f ref :@: Exp f ref |
  ExpBnd (f (Exp f ref))
  deriving (Generic)

makePrisms ''Exp

deriving instance Functor f => Functor (Exp f)

type ExpNi b = Exp (BndNi b)

instance (Arbitrary b, Arbitrary ref) => Arbitrary (ExpNi b ref) where
  arbitrary = genericArbitrary
  shrink = genericShrink

type ExpHo b = Exp (BndHo b)

pattern ExpVar :: Bnd bnd => BndVar bnd -> Exp bnd ref
pattern ExpVar v <- (preview _ExpVar -> Just v)
  where ExpVar v = review _ExpVar v

pattern ExpLam :: Bnd bnd => BndLam bnd (Exp bnd ref) -> Exp bnd ref
pattern ExpLam l <- (preview _ExpLam -> Just l)
  where ExpLam l = review _ExpLam l

_ExpVar :: Bnd bnd => Prism' (Exp bnd ref) (BndVar bnd)
_ExpVar = _ExpBnd . _BndVar

_ExpLam :: Bnd bnd => Prism' (Exp bnd ref) (BndLam bnd (Exp bnd ref))
_ExpLam = _ExpBnd . _BndLam

deriving instance (Eq ref, Eq (f (Exp f ref))) => Eq (Exp f ref)

deriving instance (Show ref, Show (f (Exp f ref))) => Show (Exp f ref)

instance (Serialize ref, Lifting Serialize f) => Serialize (Exp f ref) where
  put = Cereal.gPut . Generic.from Constraint.\\
    lifting @Serialize @f @(Exp f ref)
  get = Generic.to <$> Cereal.gGet Constraint.\\
    lifting @Serialize @f @(Exp f ref)

lam :: (ExpHo b ref -> ExpHo b ref) -> ExpHo b ref
lam l = ExpLam (\x -> l (ExpVar x))

expFromP :: forall ref . (forall b . ExpHo b ref) -> ExpNi () ref
expFromP e = expFromP' 0 e
  where
    expFromP' :: Natural -> ExpHo Natural ref -> ExpNi () ref
    expFromP' offset = \case
      ExpCon c -> ExpCon c
      ExpPrim p -> ExpPrim (expFromP' offset <$> p)
      ExpRef expId -> ExpRef expId
      fn :@: arg -> expFromP' offset fn :@: expFromP' offset arg
      ExpVar v -> ExpVar (Var () (offset - v))
      ExpLam f ->
        let offset' = offset + 1
        in ExpLam ((), expFromP' offset' (f offset'))

_ExpInteger :: Prism' (Exp f ref) Integer
_ExpInteger = _ExpPrim . _PrimValue . _ValueInteger

pattern ExpInteger :: Integer -> Exp f ref
pattern ExpInteger a <- (preview _ExpInteger -> Just a)
  where ExpInteger a = review _ExpInteger a

newtype ExpId = ExpId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''ExpId

newtype Prog f ref = Prog (Map ref (Exp f ref))
  deriving (Generic)

makePrisms ''Prog

deriving instance (Ord ref, Eq ref, Eq (f (Exp f ref))) => Eq (Prog f ref)

deriving instance (Ord ref, Show ref, Show (f (Exp f ref))) => Show (Prog f ref)

instance
  (Ord ref, Serialize ref, Lifting Serialize f) =>
    Serialize (Prog f ref) where
  put = Cereal.gPut . Generic.from Constraint.\\
    lifting @Serialize @f @(Prog f ref)
  get = Generic.to <$> Cereal.gGet Constraint.\\
    lifting @Serialize @f @(Prog f ref)

progEmpty :: Prog f ref
progEmpty = _Prog # Map.empty

progFromList :: Ord ref => [(ref, Exp f ref)] -> Prog f ref
progFromList defs = _Prog # Map.fromList defs

progLookup :: Ord ref => ref -> Prog f ref -> Maybe (Exp f ref)
progLookup expId prog = Map.lookup expId (prog ^. _Prog)

data ExpressionNotFound = ExpressionNotFound
  deriving (Show)

instance Exception ExpressionNotFound

progGet :: Ord ref => Prog f ref -> ref -> Exp f ref
progGet prog expId =
  fromMaybe (throw ExpressionNotFound) $
    progLookup expId prog
