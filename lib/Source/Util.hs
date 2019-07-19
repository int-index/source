{-# OPTIONS -fno-warn-orphans #-}

module Source.Util
  ( atomicRunStateIORef'
  , toposort
  , listLookup
  , unit
  , maybeThrowError
  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.Graph (stronglyConnComp, flattenSCCs)
import Data.IORef
import Data.Maybe
import Data.Tuple
import Numeric.Natural
import Test.QuickCheck as QC

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)

toposort :: Ord k => [(a, k, [k])] -> [a]
toposort = flattenSCCs . reverse . stronglyConnComp

listLookup :: Natural -> [a] -> Maybe a
listLookup n = listToMaybe . drop (fromIntegral n)

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral

unit :: a -> () -> a
unit = const

maybeThrowError :: MonadError e m => e -> Maybe a -> m a
maybeThrowError e = maybe (throwError e) pure
