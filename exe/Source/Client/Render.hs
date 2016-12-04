module Source.Client.Render
  ( renderModel
  , EnableIdentifiersResolution(..)
  ) where

import Control.Lens
import Data.Traversable
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Graphics.Vty as Vty
import Data.Semigroup
import Data.EnumMap.Lazy as EnumMapL
import Data.String

import Source.Model
import Source.Value
import Source.Identifier
import Source.Util
import Source.Layout
import Source.Client.Render.Layout

newtype ActiveZone = ActiveZone NodeId

data Rendered =
  RenderedEmpty |
  RenderedString Vty.Attr String |
  RenderedActiveZone ActiveZone

renderedToEither :: Rendered -> Either ActiveZone Vty.Image
renderedToEither = \case
  RenderedEmpty -> Right emptyImage
  RenderedString attr str -> Right (string attr str)
  RenderedActiveZone activeZone -> Left activeZone

renderedBox :: Rendered -> Box Int Int
renderedBox = \case
  RenderedEmpty -> imageBox emptyImage
  RenderedString attr str -> imageBox (string attr str)
  RenderedActiveZone _ -> error "Active zone has no inherent size"

instance IsString Rendered where
  fromString = RenderedString defAttr

renderValue ::
  (Num x, Num y, Ord x, Ord y) =>
  Value ->
  Layout x y Rendered
renderValue = \case
  ValueInteger n ->
    layoutString (defAttr `withForeColor` blue) $ show @Integer n
  ValueChar c ->
    layoutString (defAttr `withForeColor` yellow) $ show @Char c
  ValueList vs ->
    let
      img = case nonEmpty vs of
        Nothing -> "[]"
        Just vs' -> List.foldr1 layoutHorizontalTop [
          "[",
          List.foldr1 layoutHorizontalTop $
            NonEmpty.intersperse "; " (renderValue <$> vs'),
          "]" ]
      strVal = do
        for vs $ \case
          ValueChar c -> Just c
          _           -> Nothing
    in
      case strVal of
        Just cs | not (List.null cs) ->
          layoutString (defAttr `withForeColor` red) $ show @String cs
        _ -> img

renderIdentifier :: Identifier -> Layout x y Rendered
renderIdentifier =
  layoutString (defAttr `withStyle` bold) .
    nameToString . identifierToName

distribExcess :: Integral n => n -> n -> (n, n)
distribExcess desired actual = (left, right)
  where
    excess = max 0 (desired - actual)
    left   = excess `quot` 2
    right  = excess - left

newtype EnableIdentifiersResolution = EnableIdentifiersResolution Bool

data NodeInfo = NodeInfo NodeId Node (PerDirection [Edge])

nodesToposort :: Nodes -> Edges -> [NodeInfo]
nodesToposort nodes edges = toposort $ do
  (nodeId, node) <- EnumMapL.toList (nodes ^. _Nodes)
  let
    nodeEdges = edgesNodeEdges nodeId edges
    nodeInfo = NodeInfo nodeId node nodeEdges
    targetNodeIds = view edgeTarget <$> view atOutward nodeEdges
  return (nodeInfo, nodeId, targetNodeIds)

renderModel
  :: EnableIdentifiersResolution
  -> Maybe Vty.Event
  -> Nodes
  -> Edges
  -> Cursors
  -> (Vty.Picture, (Int, Int) -> Maybe NodeId)
renderModel
    enableIdentifiersResolution
    mLastEvent
    nodes
    edges
    _cursors = (pic, ptrNodeId)
  where
    pic = renderImageElements imageElements
    ptrNodeId = pointerSelectNodeId activeZoneElements
    (activeZoneElements, imageElements) =
      elementsPartition .
      fmap (over elementObject renderedToEither) .
      collageElements .
      layoutCollage renderedBox $
      rView
    rView = layoutVerticalLeft rLastEvent rNodes
    rLastEvent =
      case mLastEvent of
        Nothing -> layoutSingleton RenderedEmpty
        Just lastEvent -> fromString (show lastEvent)
    rNodes = List.foldr layoutVerticalLeft (layoutSingleton RenderedEmpty) $
      renderNode enableIdentifiersResolution nodes <$>
        nodesToposort nodes edges

layoutActivate ::
  (Ord x, Ord y) =>
  ActiveZone ->
  Layout x y Rendered ->
  Layout x y Rendered
layoutActivate activeZone layout = Layout $ \getBox ->
  let
    box = collageBox collage
    collage = layoutCollage getBox layout
  in
    collageSuperimpose (collageElement box (RenderedActiveZone activeZone)) collage

pointerSelectNodeId ::
  (Ord x, Ord y) =>
  [Element x y ActiveZone] ->
  (x, y) ->
  Maybe NodeId
pointerSelectNodeId activeZones (x, y) =
  List.find inBounds activeZones <&> \el ->
    case el ^. elementObject of ActiveZone nodeId -> nodeId
  where
    inBounds el = boxInside (el ^. elementBox) (Point x y)

layoutString :: Attr -> String -> Layout x y Rendered
layoutString attr s = layoutSingleton (RenderedString attr s)

renderNode ::
  EnableIdentifiersResolution ->
  Nodes ->
  NodeInfo ->
  Layout Int Int Rendered
renderNode
    (EnableIdentifiersResolution enableIdentifiersResolution)
    nodes
    nodeInfo = rNode
  where
    NodeInfo nodeId node edges = nodeInfo
    PerDirection outwardEdges inwardEdges = edges
    activeZone = ActiveZone nodeId
    rNode = Layout $ \getBox ->
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
              layoutWidth getBox rEdgeValue +
              layoutWidth getBox rSourceNodeId
            (padLeftWidth, padRightWidth) =
              distribExcess maxREdgeValueWidth rEdgeValueWidth
            rEdgeValueWidth' = (Option . Just . Max) rEdgeValueWidth
            rEdge = List.foldr1 layoutHorizontalTop [
              rSourceNodeId,
              " ──",
              fromString (List.replicate padLeftWidth '─'),
              rEdgeValue,
              fromString (List.replicate padRightWidth '─'),
              "──┤" ]
          in
            (rEdgeValueWidth', rEdge)
        renderOutwardEdge maxREdgeValueWidth edge =
          let
            rTargetNodeId = renderNodeReference (edge ^. edgeTarget)
            rEdgeValue = renderValue $ edge ^. edgeValue
            rEdgeValueWidth = layoutWidth getBox rEdgeValue
            (padLeftWidth, padRightWidth) =
              distribExcess maxREdgeValueWidth rEdgeValueWidth
            rEdgeValueWidth' = (Option . Just . Max) rEdgeValueWidth
            rEdge = List.foldr1 layoutHorizontalTop [
              "├──",
              fromString (List.replicate padLeftWidth '─'),
              rEdgeValue,
              fromString (List.replicate padRightWidth '─'),
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
        rNodeDef = List.foldr1 layoutHorizontalTop [
          " ",
          if enableIdentifiersResolution
            then rNodeValue
            else List.foldr1 layoutHorizontalTop [rNodeId, ": ", rNodeValue ],
          " " ]
        rBlockHeight =
          List.length rNodeInwardEdges `max`
          List.length rNodeOutwardEdges `max`
          layoutHeight getBox rNodeDef
        (rInwardTopPad, rInwardBottomPad) =
          distribExcess rBlockHeight (List.length rNodeInwardEdges)
        (rOutwardTopPad, rOutwardBottomPad) =
          distribExcess rBlockHeight (List.length rNodeOutwardEdges)
        (rNodeDefTopPad, rNodeDefBottomPad) =
          distribExcess rBlockHeight (layoutHeight getBox rNodeDef)
        rBlock = List.foldr1 layoutHorizontalTop [
          List.foldr1 layoutVerticalRight $
            List.concat [
              ["┌"],
              List.replicate rInwardTopPad rEmptyEdge,
              rNodeInwardEdges,
              List.replicate rInwardBottomPad rEmptyEdge,
              ["└"] ],
          List.foldr1 layoutVerticalLeft [
            fromString $
              List.replicate (layoutWidth getBox rNodeDef) '─',
            layoutActivate activeZone $
            layoutPadTop rNodeDefTopPad $
            layoutPadBottom rNodeDefBottomPad $
              rNodeDef,
            fromString $
              List.replicate (layoutWidth getBox rNodeDef) '─' ],
          List.foldr1 layoutVerticalLeft $
            List.concat [
              ["┐"],
              List.replicate rOutwardTopPad rEmptyEdge,
              rNodeOutwardEdges,
              List.replicate rOutwardBottomPad rEmptyEdge,
              ["┘"] ] ]
      in
        layoutCollage getBox rBlock


{-
newtype XPos = XPos Int
  deriving (Eq, Ord, Show)

newtype YPos = YPos Int
  deriving (Eq, Ord, Show)

data Position = Position
  { _positionX :: XPos
  , _positionY :: YPos
  } deriving (Eq, Ord, Show)

newtype LineNumber = LineNumber Int

type PointerNodesMap = Position -> Maybe NodeId

type CursorNodesMap = (LineNumber, YPos) -> Maybe NodeId

type PositionNodesMap = NodeId -> (XPos, YPos, Maybe NodeId, Maybe NodeId)
-}
