module Source.Value
  ( Value(..)
  , ValueRep(..)
  , ValueInvalid(..)
  , valueInvalid
  ) where

import Data.Bool
import Data.String
import Data.Functor.Identity
import Control.Exception
import Data.Serialize as Cereal
import GHC.Generics (Generic)

data Value
  = ValueInteger Integer
  | ValueChar Char
  | ValueList [Value]
  deriving (Eq, Ord, Show, Generic)

instance Serialize Value

data ValueInvalid = ValueInvalid
  deriving (Show)

instance Exception ValueInvalid

valueInvalid :: a
valueInvalid = throw ValueInvalid

instance IsString Value where
  fromString = ValueList . fmap ValueChar

class ValueRep v where
  -- fromValue . toValue = id
  -- toValue . fromValue = id | valueInvalid
  toValue :: v -> Value
  fromValue :: Value -> v

instance ValueRep Value where
  toValue = id
  fromValue = id

instance ValueRep Integer where
  toValue = ValueInteger
  fromValue = \case
    ValueInteger n -> n
    _              -> valueInvalid

instance ValueRep Char where
  toValue = ValueChar
  fromValue = \case
    ValueChar c -> c
    _           -> valueInvalid

instance ValueRep Int where
  toValue = ValueInteger . fromIntegral
  fromValue = \case
    ValueInteger n
      | n >= fromIntegral (minBound :: Int)
      , n <= fromIntegral (maxBound :: Int)
      -> fromInteger n
    _ -> valueInvalid

instance ValueRep Bool where
  toValue = ValueInteger . bool 0 1
  fromValue = \case
    ValueInteger 0 -> False
    ValueInteger 1 -> True
    _              -> valueInvalid

instance ValueRep a => ValueRep [a] where
  toValue as = ValueList (toValue <$> as)
  fromValue = \case
    ValueList vs -> fromValue <$> vs
    _            -> valueInvalid

instance ValueRep () where
  toValue () = ValueList []
  fromValue = \case
    ValueList [] -> ()
    _            -> valueInvalid

instance (ValueRep a, ValueRep b) => ValueRep (a, b) where
  toValue (a, b) = ValueList [toValue a, toValue b]
  fromValue = \case
    ValueList [v1, v2] -> (fromValue v1, fromValue v2)
    _                  -> valueInvalid

instance ValueRep a => ValueRep (Maybe a) where
  toValue = \case
    Nothing -> ValueList []
    Just a  -> ValueList [toValue a]
  fromValue = \case
    ValueList []  -> Nothing
    ValueList [v] -> Just (fromValue v)
    _             -> valueInvalid

instance (ValueRep a, ValueRep b) => ValueRep (Either a b) where
  toValue = \case
    Left  a -> ValueList [ValueInteger 0, toValue a]
    Right b -> ValueList [ValueInteger 1, toValue b]
  fromValue = \case
    ValueList [ValueInteger 0, v] -> fromValue v
    ValueList [ValueInteger 1, v] -> fromValue v
    _                             -> valueInvalid

instance ValueRep a => ValueRep (Identity a) where
  toValue = toValue . runIdentity
  fromValue = Identity . fromValue
