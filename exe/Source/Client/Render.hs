module Source.Client.Render
  ( renderModel,
    EnableIdentifiersResolution(..),
    ActiveZone(..)
  ) where

import Control.Applicative as A
import Control.Lens
import Numeric.Natural
import Data.List as List
import Data.DList as DList
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Semigroup
import Data.String
import Data.Traversable
import Graphics.Vty as Vty

import Source.Identifier
import Source.Model
import Source.Value
import Source.Util

import Inj
import Slay.Vty as Slay
import Slay.Combinators

newtype EnableIdentifiersResolution = EnableIdentifiersResolution Bool

data ActiveZone =
  ActiveZone
    { activeZoneNodeId :: NodeId,
      activeZoneExtents :: Extents }

data Rendered =
  RenderedPrim Prim |
  RenderedActiveZone ActiveZone

instance HasExtents Rendered where
  extentsOf (RenderedPrim p) = primExtents p
  extentsOf (RenderedActiveZone az) = activeZoneExtents az

instance HasBaseline Rendered where
  baselineOf _ = NoBaseline

instance IsString Rendered where
  fromString = RenderedPrim . fromString

instance Inj Prim Rendered where
  inj = RenderedPrim

instance Inj ActiveZone Rendered where
  inj = RenderedActiveZone

renderModel
  :: EnableIdentifiersResolution
  -> Maybe Vty.Event
  -> Model
  -> (Vty.Picture, Offset -> Maybe NodeId)
renderModel
  enableIdentifiersResolution
  mLastEvent
  (Model nodes edges) =
    (pic, ptrNodeId)
  where
    pic = renderImageElements (DList.toList imageElements)
    (FindNodeId ptrNodeId, imageElements) =
      foldMapCollage renderedToAccum offsetZero rView

    rView, rLastEvent, rNodes :: Collage Rendered
    rView = vertLeft rLastEvent rNodes
    rLastEvent =
      case mLastEvent of
        Nothing -> Slay.empty
        Just lastEvent -> fromString (show lastEvent)
    rNodes = List.foldr vertLeft Slay.empty $
      renderNode enableIdentifiersResolution nodes <$>
        nodesToposort nodes edges

listLength :: [a] -> Natural
listLength = fromIntegral . List.length

listReplicate :: Natural -> a -> [a]
listReplicate = List.replicate . fromIntegral

renderNode ::
  EnableIdentifiersResolution ->
  Nodes ->
  NodeInfo ->
  Collage Rendered
renderNode
  (EnableIdentifiersResolution enableIdentifiersResolution)
  nodes
  nodeInfo =
    rNode
  where
    NodeInfo nodeId node edges = nodeInfo
    PerDirection outwardEdges inwardEdges = edges
    rNode =
      let
        rNodeId = renderIdentifier (nodeId ^. _NodeId)
        rNodeValue = renderValue (node ^. nodeValue)
        renderNodeReference nodeId' =
          if enableIdentifiersResolution
            then case nodesLookup nodeId' nodes of
              Nothing    -> renderIdentifier (nodeId' ^. _NodeId)
              Just node' -> renderValue (node' ^. nodeValue)
            else renderIdentifier (nodeId' ^. _NodeId)
        renderInwardEdge maxREdgeValueWidth edge =
          let
            rSourceNodeId = renderNodeReference (edge ^. edgeSource)
            rEdgeValue = renderValue $ edge ^. edgeValue
            rEdgeValueWidth =
              collageWidth rEdgeValue +
              collageWidth rSourceNodeId
            (padLeftWidth, padRightWidth) =
              integralDistribExcess maxREdgeValueWidth rEdgeValueWidth
            rEdgeValueWidth' = (Option . Just . Max) rEdgeValueWidth
            rEdge = List.foldr1 horizTop [
              rSourceNodeId,
              " ──",
              fromString (listReplicate padLeftWidth '─'),
              rEdgeValue,
              fromString (listReplicate padRightWidth '─'),
              "──┤" ]
          in
            (rEdgeValueWidth', rEdge)
        renderOutwardEdge maxREdgeValueWidth edge =
          let
            rTargetNodeId = renderNodeReference (edge ^. edgeTarget)
            rEdgeValue = renderValue $ edge ^. edgeValue
            rEdgeValueWidth = collageWidth rEdgeValue
            (padLeftWidth, padRightWidth) =
              integralDistribExcess maxREdgeValueWidth rEdgeValueWidth
            rEdgeValueWidth' = (Option . Just . Max) rEdgeValueWidth
            rEdge = List.foldr1 horizTop [
              "├──",
              fromString (listReplicate padLeftWidth '─'),
              rEdgeValue,
              fromString (listReplicate padRightWidth '─'),
              "── ",
              rTargetNodeId ]
          in
            (rEdgeValueWidth', rEdge)
        rEmptyEdge = "│"
        rNodeOutwardEdges = rEdges
          where
            (maxREdgeValueWidth, rEdges) =
              sequenceA $ renderOutwardEdge maxREdgeValueWidth' <$>
                outwardEdges
            maxREdgeValueWidth' = option 0 getMax maxREdgeValueWidth
        rNodeInwardEdges = rEdges
          where
            (maxREdgeValueWidth, rEdges) =
              sequenceA $ renderInwardEdge maxREdgeValueWidth' <$>
                inwardEdges
            maxREdgeValueWidth' = option 0 getMax maxREdgeValueWidth
        rNodeDef = List.foldr1 horizTop [
          " ",
          if enableIdentifiersResolution
            then rNodeValue
            else List.foldr1 horizTop [rNodeId, ": ", rNodeValue ],
          " " ]
        rBlockHeight =
          listLength rNodeInwardEdges `max`
          listLength rNodeOutwardEdges `max`
          collageHeight rNodeDef
        (rInwardTopPad, rInwardBottomPad) =
          integralDistribExcess rBlockHeight (listLength rNodeInwardEdges)
        (rOutwardTopPad, rOutwardBottomPad) =
          integralDistribExcess rBlockHeight (listLength rNodeOutwardEdges)
        (rNodeDefTopPad, rNodeDefBottomPad) =
          integralDistribExcess rBlockHeight (collageHeight rNodeDef)
        rBlock = List.foldr1 horizTop [
          List.foldr1 vertRight $
            List.concat [
              ["┌"],
              listReplicate rInwardTopPad rEmptyEdge,
              rNodeInwardEdges,
              listReplicate rInwardBottomPad rEmptyEdge,
              ["└"] ],
          List.foldr1 vertLeft [
            fromString $
              listReplicate (collageWidth rNodeDef) '─',
            substrate (LRTB 0 0 rNodeDefTopPad rNodeDefBottomPad)
                      (\e -> inj (ActiveZone nodeId e)) $
              rNodeDef,
            fromString $
              listReplicate (collageWidth rNodeDef) '─' ],
          List.foldr1 vertLeft $
            List.concat [
              ["┐"],
              listReplicate rOutwardTopPad rEmptyEdge,
              rNodeOutwardEdges,
              listReplicate rOutwardBottomPad rEmptyEdge,
              ["┘"] ] ]
      in
        rBlock

renderValue ::
  Value ->
  Collage Rendered
renderValue = \case
  ValueInteger n ->
    Slay.string (defAttr `withForeColor` blue) $ show @Integer n
  ValueChar c ->
    Slay.string (defAttr `withForeColor` yellow) $ show @Char c
  ValueList vs ->
    let
      img = case nonEmpty vs of
        Nothing -> "[]"
        Just vs' -> List.foldr1 horizTop [
          "[",
          List.foldr1 horizTop $
            NonEmpty.intersperse "; " (renderValue <$> vs'),
          "]" ]
      strVal = do
        for vs $ \case
          ValueChar c -> Just c
          _           -> Nothing
    in
      case strVal of
        Just cs | not (List.null cs) ->
          Slay.string (defAttr `withForeColor` red) $ show @String cs
        _ -> img

renderIdentifier :: Identifier -> Collage Rendered
renderIdentifier =
  Slay.string (defAttr `withStyle` bold) .
    nameToString . identifierToName

newtype FindNodeId = FindNodeId (Offset -> Maybe NodeId)

instance Semigroup FindNodeId where
  FindNodeId f1 <> FindNodeId f2 =
    FindNodeId $ \point -> f1 point A.<|> f2 point

instance Monoid FindNodeId where
  mempty = FindNodeId (const Nothing)

findInActiveZone :: Offset -> ActiveZone -> FindNodeId
findInActiveZone offset az =
  FindNodeId $ \point ->
    if insideBox (offset, activeZoneExtents az) point
    then Just (activeZoneNodeId az)
    else Nothing

type VtyImages = DList (Positioned Vty.Image)

renderedToAccum :: Positioned Rendered -> (FindNodeId, VtyImages)
renderedToAccum (At o r) =
  case r of
    RenderedPrim p -> (mempty, DList.singleton (At o (primImage p)))
    RenderedActiveZone az -> (findInActiveZone o az, mempty)

data NodeInfo = NodeInfo NodeId Node (PerDirection [Edge])

nodesToposort :: Nodes -> Edges -> [NodeInfo]
nodesToposort nodes edges = toposort $ do
  (nodeId, node) <- Map.toList (nodes ^. _Nodes)
  let
    nodeEdges = edgesNodeEdges nodeId edges
    nodeInfo = NodeInfo nodeId node nodeEdges
    targetNodeIds = view edgeTarget <$> view atOutward nodeEdges
  return (nodeInfo, nodeId, targetNodeIds)
