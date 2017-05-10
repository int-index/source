module Source.Language.Core.Eval
  ( reduce
  , execute
  ) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Data.Bool
import Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.STRef
import Numeric.Natural
import Prelude hiding (exp)

import Source.Language.Core.Syn
import Source.Model
import Source.Util

data UnboundVariable = UnboundVariable
  deriving (Show)

instance Exception UnboundVariable

execute ::
  ExpId {- main action -} ->
  Prog (BndNi b) ExpId ->
  State Model ()
execute _ _ = return ()

type ExpNiST' b s = ExpNi b (STRef s (ExpNiST b s))

newtype ExpNiST b s = ExpNiST (ExpNiST' b s)

makePrisms ''ExpNiST

runST' :: (forall s . Proxy s -> ST s a) -> a
runST' act = runST (act Proxy)

reduce ::
  forall b ref .
  Eq b =>
  Ord ref =>
  Prog (BndNi b) ref ->
  ExpNi b ref ->
  ExpNi b ()
reduce prog exp = runST' $ \(Proxy :: Proxy s) -> mdo
  let
    Prog expMap = prog

    substRefST :: ExpNi b ref -> ExpNiST' b s
    substRefST e = lookupRefST <$> e

    lookupRefST :: ref -> STRef s (ExpNiST b s)
    lookupRefST ref =
      fromMaybe (throw ExpressionNotFound) $
        Map.lookup ref stRefMap

    forgetRefs :: ExpNiST' b s -> ExpNi b ()
    forgetRefs = void

  stRefMap <-
    traverse (newSTRef . ExpNiST . substRefST) expMap ::
      ST s (Map ref (STRef s (ExpNiST b s)))

  forgetRefs <$> reduceST (substRefST exp)

reduceST ::
  Eq b =>
  ExpNiST' b s ->
  ST s (ExpNiST' b s)
reduceST = \case
  ExpCon c -> pure (ExpCon c)
  ExpVar _ -> throw UnboundVariable
  ExpRef expRef -> do
    ExpNiST exp <- readSTRef expRef
    exp' <- reduceST exp
    exp' <$ writeSTRef expRef (ExpNiST exp')
  ExpPrim p -> reducePrimST p
  ExpLam e -> pure (ExpLam e)
  -- Beta-reduction
  f :@: a -> do
    f' <- reduceST f
    case f' of
      ExpLam (b, e) -> do
        a' <- case a of
          ExpRef r -> pure r
          _        -> newSTRef (ExpNiST a)
        reduceST $ substST b 0 a' e
      _ -> pure (f' :@: a)

reducePrimST ::
  Eq b =>
  Prim (ExpNiST' b s) ->
  ST s (ExpNiST' b s)
reducePrimST p = fmap defaultCase . runExceptT $ do
  case p of
    PrimValue{} -> throwError ()
    PrimAdd a b -> integerBinOp (+) a b
    PrimSubtract a b -> integerBinOp (-) a b
    PrimWithNat onZero onSucc a -> do
      a' <- match (^? _ExpInteger) a
      e <- case compare a' 0 of
        LT -> throwError ()
        EQ -> pure onZero
        GT -> pure $ onSucc :@: (_ExpInteger # pred a')
      lift (reduceST e)
  where

    defaultCase = either (unit (ExpPrim p)) id

    match f e = do
      a <- lift (reduceST e)
      maybeThrowError () (f a)

    integerBinOp f a b = do
      a' <- match (^? _ExpInteger) a
      b' <- match (^? _ExpInteger) b
      pure $ _ExpInteger # (f a' b')

substST
  :: Eq b
  => b
  -> Natural
  -> STRef s (ExpNiST b s)
  -> ExpNiST' b s
  -> ExpNiST' b s
substST b n x = \case
  ExpCon c -> ExpCon c
  ExpVar v ->
    if v == Var b n
      then ExpRef x
      else ExpVar v
  ExpRef expId -> ExpRef expId
  ExpPrim p -> ExpPrim (substST b n x <$> p)
  ExpLam (b', e) ->
    let n' = bool id succ (b == b') n
    in ExpLam (b', substST b n' x e)
  f :@: a -> substST b n x f :@: substST b n x a
