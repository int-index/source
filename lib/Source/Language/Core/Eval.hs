module Source.Language.Core.Eval
  ( execute
  , reduce
  ) where

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Serialize as Cereal
import Data.Void
import GHC.Generics as Generic

import Source.Language.Core.Syn
import Source.Model
import Source.Util

data UnboundVariable = UnboundVariable
  deriving (Show)

instance Exception UnboundVariable

data CtxElem b ref =
  CtxElemExp (ExpNi b ref) |
  CtxElemPadding
  deriving (Show, Eq, Generic)

instance (Serialize b, Serialize ref) => Serialize (CtxElem b ref)

fromCtxElem :: Var b -> CtxElem b ref -> ExpNi b ref
fromCtxElem v = \case
  CtxElemExp e -> e
  CtxElemPadding -> _ExpVar # v

newtype Ctx b ref =
  Ctx {
    unCtx :: Map b [CtxElem b ref]
  } deriving (Show, Eq, Generic)

makePrisms ''Ctx

instance (Ord b, Serialize b, Serialize ref) => Serialize (Ctx b ref)

ctxLookup ::
  Ord b =>
  Var b ->
  Ctx b ref ->
  Maybe (CtxElem b ref)
ctxLookup (Var b n) =
  (listLookup n <=< Map.lookup b) . unCtx

ctxGet ::
  Ord b =>
  Var b ->
  Ctx b ref ->
  ExpNi b ref
ctxGet v ctx =
  fromCtxElem v . fromMaybe (throw UnboundVariable) $
    ctxLookup v ctx

ctxEmpty :: Ctx b ref
ctxEmpty = Ctx Map.empty

ctxAt :: Ord b => b -> Lens' (Ctx b ref) [CtxElem b ref]
ctxAt b = _Ctx . at b . anon [] List.null

ctxAppend :: Ord b => b -> ExpNi b ref -> Ctx b ref -> Ctx b ref
ctxAppend b e = over (ctxAt b) (CtxElemExp e:)

ctxExtend :: Ord b => b -> Ctx b ref -> Ctx b ref
ctxExtend b = over (ctxAt b) (CtxElemPadding:)

execute ::
  ExpId {- main action -} ->
  Prog (BndNi()) ExpId ->
  State Model ()
execute _ _ = return ()

data ResolvingStrategy a b where
  ResolvingStrategyRetain :: ResolvingStrategy a a {- Retain references
  unresolved. -}
  ResolvingStrategyLookup :: ResolvingStrategy a Void {- Lookup references and
  continue to resolve recursively. -}

reduce ::
  forall b ref .
  (Ord b, Ord ref) =>
  Prog (BndNi b) ref ->
  ExpNi b ref ->
  ExpNi b Void
reduce prog = reduce' ResolvingStrategyLookup ctxEmpty
  where
    reduceCaf = reduce' ResolvingStrategyRetain ctxEmpty
    prog' = over (_Prog . mapped) reduceCaf prog
    reduce' ::
      forall ref' .
      ResolvingStrategy ref ref' ->
      Ctx b ref' ->
      ExpNi b ref ->
      ExpNi b ref'
    reduce' resolvingStrategy = fix $ \descend ->
      \ctx -> \case
        ExpCon c -> ExpCon c
        ExpVar v -> ctxGet v ctx
        ExpRef expId ->
          case resolvingStrategy of
            ResolvingStrategyRetain -> ExpRef expId
            ResolvingStrategyLookup -> descend ctx (progGet prog' expId)
        ExpPrim p -> reducePrim (descend ctx <$> p)
        ExpLam (b, e) ->
          let ctx' = ctxExtend b ctx
          in ExpLam (b, descend ctx' e)
        -- Beta-reduction
        f :@: a ->
          let
            f' = descend ctx f
            a' = descend ctx a
            rev =
              case resolvingStrategy of
                ResolvingStrategyRetain -> id
                ResolvingStrategyLookup -> vacuous
          in
            case f' of
              ExpLam (b, e) ->
                descend (ctxAppend b a' ctx) (rev e)
              _ -> f' :@: a'

reducePrim :: Prim (Exp f ref) -> Exp f ref
reducePrim (PrimAdd (ExpInteger a) (ExpInteger b)) =
  ExpInteger (a + b)
reducePrim (PrimSubtract (ExpInteger a) (ExpInteger b)) =
  ExpInteger (a - b)
reducePrim p = ExpPrim p
