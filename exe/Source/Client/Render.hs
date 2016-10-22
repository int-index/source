module Source.Client.Render
  ( renderModel
  , EnableIdentifiersResolution(..)
  ) where

import Control.Lens
import Data.Traversable
import Data.List as List
import Graphics.Vty as Vty
import Data.Semigroup
import Data.EnumMap.Lazy as EnumMapL

import Source.Model
import Source.Value
import Source.Identifier
import Source.Util
import Source.Layout
import Source.Client.Render.Layout

renderValue ::
  (Num x, Num y, Ord x, Ord y) =>
  Value ->
  Layout x y (Either a Vty.Image)
renderValue = \case
  ValueInteger n ->
    layoutString (defAttr `withForeColor` blue) $ show @Integer n
  ValueChar c ->
    layoutString (defAttr `withForeColor` yellow) $ show @Char c
  ValueList vs ->
    let
      img = List.foldr1 layoutHorizontalTop [
        layoutChar defAttr '[',
        List.foldr1 layoutHorizontalTop $
          intersperse (layoutString defAttr "; ") (renderValue <$> vs),
        layoutChar defAttr ']' ]
      strVal = do
        for vs $ \case
          ValueChar c -> Just c
          _           -> Nothing
    in
      case strVal of
        Just cs | not (List.null cs) ->
          layoutString (defAttr `withForeColor` red) $ show @String cs
        _ -> img

renderIdentifier :: Identifier -> Layout x y (Either a Vty.Image)
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
      collageElements .
      layoutCollage (either (error "ActiveZone box") imageBox) $
      rView
    rView = layoutVerticalLeft rLastEvent rNodes
    rLastEvent =
      case mLastEvent of
        Nothing -> Right <$> layoutEmptyImage
        Just lastEvent -> layoutString defAttr (show lastEvent)
    rNodes = List.foldr layoutVerticalLeft (Right <$> layoutEmptyImage) $
      renderNode enableIdentifiersResolution nodes <$>
        nodesToposort nodes edges

newtype ActiveZone = ActiveZone NodeId

layoutActivate ::
  (Ord x, Ord y) =>
  ActiveZone ->
  Layout x y (Either ActiveZone a) ->
  Layout x y (Either ActiveZone a)
layoutActivate activeZone layout = Layout $ \getBox ->
  let
    box = collageBox collage
    collage = layoutCollage getBox layout
  in
    collageSuperimpose (collageElement box (Left activeZone)) collage

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

layoutChar :: Attr -> Char -> Layout x y (Either a Image)
layoutChar attr c = Right <$> layoutSingleton (char attr c)

layoutString :: Attr -> String -> Layout x y (Either a Image)
layoutString attr s = Right <$> layoutSingleton (string attr s)

renderNode ::
  EnableIdentifiersResolution ->
  Nodes ->
  NodeInfo ->
  Layout Int Int (Either ActiveZone Image)
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
              layoutString defAttr " ──",
              layoutString defAttr (List.replicate padLeftWidth '─'),
              rEdgeValue,
              layoutString defAttr (List.replicate padRightWidth '─'),
              layoutString defAttr "──┤" ]
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
              layoutString defAttr "├──",
              layoutString defAttr (List.replicate padLeftWidth '─'),
              rEdgeValue,
              layoutString defAttr (List.replicate padRightWidth '─'),
              layoutString defAttr "── ", rTargetNodeId ]
          in
            (rEdgeValueWidth', rEdge)
        rEmptyEdge = layoutChar defAttr '│'
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
        rNodeDef = List.foldr1 layoutHorizontalTop
          [ layoutChar defAttr ' '
          , if enableIdentifiersResolution
              then rNodeValue
              else List.foldr1 layoutHorizontalTop [
                rNodeId,
                layoutString defAttr ": ",
                rNodeValue ]
          , layoutChar defAttr ' ' ]
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
              [layoutChar defAttr '┌'],
              List.replicate rInwardTopPad rEmptyEdge,
              rNodeInwardEdges,
              List.replicate rInwardBottomPad rEmptyEdge,
              [layoutChar defAttr '└'] ],
          List.foldr1 layoutVerticalLeft [
            layoutString defAttr $
              List.replicate (layoutWidth getBox rNodeDef) '─',
            layoutActivate activeZone $
            layoutPadTop rNodeDefTopPad $
            layoutPadBottom rNodeDefBottomPad $
              rNodeDef,
            layoutString defAttr $
              List.replicate (layoutWidth getBox rNodeDef) '─' ],
          List.foldr1 layoutVerticalLeft $
            List.concat [
              [layoutChar defAttr '┐'],
              List.replicate rOutwardTopPad rEmptyEdge,
              rNodeOutwardEdges,
              List.replicate rOutwardBottomPad rEmptyEdge,
              [layoutChar defAttr '┘'] ] ]
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
