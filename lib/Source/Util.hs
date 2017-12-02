{-# OPTIONS -fno-warn-orphans #-}

module Source.Util
  ( EnumMapL
  , EnumMapS
  , atomicRunStateIORef'
  , toposort
  , listLookup
  , unit
  , maybeThrowError
  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.EnumMap.Lazy as EnumMapL
import Data.EnumMap.Strict as EnumMapS
import Data.Graph (stronglyConnComp, flattenSCCs)
import Data.IORef
import Data.Maybe
import Data.Serialize as Cereal
import Data.Tuple
import Numeric.Natural
import Test.QuickCheck as QC

type EnumMapL = EnumMapL.EnumMap
type EnumMapS = EnumMapS.EnumMap

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)

toposort :: Ord k => [(a, k, [k])] -> [a]
toposort = flattenSCCs . reverse . stronglyConnComp

listLookup :: Natural -> [a] -> Maybe a
listLookup n = listToMaybe . drop (fromIntegral n)

instance
  (Enum k, Serialize k, Serialize a) =>
    Serialize (EnumMapL k a) where
  put = Cereal.put . EnumMapL.toAscList
  get = EnumMapL.fromDistinctAscList <$> Cereal.get

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral

unit :: a -> () -> a
unit = const

maybeThrowError :: MonadError e m => e -> Maybe a -> m a
maybeThrowError e = maybe (throwError e) pure
