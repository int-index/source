{-# OPTIONS -fno-warn-orphans #-}

module Source.Util
  ( EnumMapL
  , EnumMapS
  , atomicRunStateIORef'
  , toposort
  ) where

import Control.Monad.State
import Data.Serialize as Cereal
import Data.EnumMap.Lazy as EnumMapL
import Data.EnumMap.Strict as EnumMapS
import Data.MultiSet as MultiSet
import Data.IORef
import Data.Tuple
import Data.Graph (stronglyConnComp, flattenSCCs)

type EnumMapL = EnumMapL.EnumMap
type EnumMapS = EnumMapS.EnumMap

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)

toposort :: Ord k => [(a, k, [k])] -> [a]
toposort = flattenSCCs . reverse . stronglyConnComp

instance
  (Enum k, Serialize k, Serialize a) =>
    Serialize (EnumMapL k a) where
  put = Cereal.put . EnumMapL.toAscList
  get = EnumMapL.fromDistinctAscList <$> Cereal.get

instance Serialize a => Serialize (MultiSet a) where
  put = Cereal.put . MultiSet.toAscList
  get = MultiSet.fromDistinctAscList <$> Cereal.get
