module Source.Language.Core.Syn
  ( ConId(..)
  , _ConId
  , Prim(..)
  , _PrimValue
  , _PrimAdd
  , _PrimSubtract
  , VarNi(..)
  , BndrNi(..)
  , _BndrNiVar
  , _BndrNiLam
  , BndrHo(..)
  , _BndrHoVar
  , _BndrHoLam
  , Exp(.., ExpInteger)
  , ExpNi
  , ExpHo
  , _ExpCon
  , _ExpPrim
  , _ExpRef
  , (.:@:)
  , _ExpBndr
  , _ExpHoVar
  , _ExpHoLam
  , _ExpNiVar
  , _ExpNiLam
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
import Data.Map as Map
import Data.Maybe
import Data.Serialize as Cereal
import Generics.Deriving.Eq
import Generics.Deriving.Show
import GHC.Generics as Generic
import Numeric.Natural
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary.Generic

import Source.Identifier
import Source.Util ()
import Source.Value

newtype ConId = ConId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''ConId

data Prim a =
  PrimValue Value {- value constant -} |
  PrimAdd      a a {- integer addition -} |
  PrimSubtract a a {- integer subtraction -}
  deriving (Show, Eq, Functor, Generic)

makePrisms ''Prim

instance Serialize a => Serialize (Prim a)

data VarNi b = VarNi b Natural
  deriving (Show, Eq, Ord, Generic)

instance Serialize b => Serialize (VarNi b)

instance Arbitrary b => Arbitrary (VarNi b) where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Binding structure for named variables with de Bruijn indices.
-- "Ni" stands for "named, indexed".
data BndrNi b a =
  BndrNiVar (VarNi b) |
  BndrNiLam b a
  deriving (Eq, Show, Functor, Generic)

makePrisms ''BndrNi

instance (Serialize b, Serialize a) => Serialize (BndrNi b a)

instance Eq b => Lifting Eq (BndrNi b) where
  lifting = Sub Dict

instance Show b => Lifting Show (BndrNi b) where
  lifting = Sub Dict

instance Serialize b => Lifting Serialize (BndrNi b) where
  lifting = Sub Dict

-- | Binding structure for PHOAS.
-- "Ho" stands for "higher-order".
data BndrHo b a =
  BndrHoVar b |
  BndrHoLam (b -> a)
  deriving (Functor, Generic)

makePrisms ''BndrHo

data Exp f ref =
  ExpCon ConId |
  ExpPrim (Prim (Exp f ref)) |
  ExpRef ref |
  Exp f ref :@: Exp f ref |
  ExpBndr (f (Exp f ref))
  deriving (Generic)

makePrisms ''Exp

deriving instance Functor f => Functor (Exp f)

type ExpNi b = Exp (BndrNi b)

type ExpHo b = Exp (BndrHo b)

_ExpNiVar :: Prism' (ExpNi b ref) (VarNi b)
_ExpNiVar = _ExpBndr . _BndrNiVar

_ExpNiLam :: Prism' (ExpNi b ref) (b, ExpNi b ref)
_ExpNiLam = _ExpBndr . _BndrNiLam

_ExpHoVar :: Prism' (ExpHo b ref) b
_ExpHoVar = _ExpBndr . _BndrHoVar

_ExpHoLam :: Prism' (ExpHo b ref) (b -> ExpHo b ref)
_ExpHoLam = _ExpBndr . _BndrHoLam

instance (Eq ref, Lifting Eq f) => Eq (Exp f ref) where
  (==) = geqdefault Constraint.\\
    lifting @Eq @f @(Exp f ref)

instance (Show ref, Lifting Show f) => Show (Exp f ref) where
  showsPrec = gshowsPrecdefault Constraint.\\
    lifting @Show @f @(Exp f ref)

instance (Serialize ref, Lifting Serialize f) => Serialize (Exp f ref) where
  put = Cereal.gPut . Generic.from Constraint.\\
    lifting @Serialize @f @(Exp f ref)
  get = Generic.to <$> Cereal.gGet Constraint.\\
    lifting @Serialize @f @(Exp f ref)

lam :: (ExpHo b ref -> ExpHo b ref) -> ExpHo b ref
lam l = ExpBndr (BndrHoLam (\x -> l (ExpBndr (BndrHoVar x))))

expFromP :: forall ref . (forall b . ExpHo b ref) -> ExpNi () ref
expFromP e = expFromP' 0 e
  where
    expFromP' :: Natural -> ExpHo Natural ref -> ExpNi () ref
    expFromP' offset = \case
      ExpCon c -> ExpCon c
      ExpPrim p -> ExpPrim (expFromP' offset <$> p)
      ExpRef expId -> ExpRef expId
      fn :@: arg -> expFromP' offset fn :@: expFromP' offset arg
      ExpBndr b -> ExpBndr (bndrFromP b)
      where
        bndrFromP = \case
          BndrHoVar v -> BndrNiVar (VarNi () (offset - v))
          BndrHoLam f ->
            let offset' = offset + 1
            in BndrNiLam () (expFromP' offset' (f offset'))

pattern ExpInteger :: Integer -> Exp f ref
pattern ExpInteger n = ExpPrim (PrimValue (ValueInteger n))

newtype ExpId = ExpId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

makePrisms ''ExpId

newtype Prog f ref = Prog (Map ref (Exp f ref))
  deriving (Generic)

makePrisms ''Prog

instance (Ord ref, Eq ref, Lifting Eq f) => Eq (Prog f ref) where
  (==) = geqdefault Constraint.\\
    lifting @Eq @f @(Prog f ref)

instance (Ord ref, Show ref, Lifting Show f) => Show (Prog f ref) where
  showsPrec = gshowsPrecdefault Constraint.\\
    lifting @Show @f @(Prog f ref)

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
