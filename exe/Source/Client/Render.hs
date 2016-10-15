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

renderValue :: Value -> Vty.Image
renderValue = \case
  ValueInteger n ->
    string (defAttr `withForeColor` blue) $ show @Integer n
  ValueChar c ->
    string (defAttr `withForeColor` yellow) $ show @Char c
  ValueList vs ->
    let
      img = horizCat
        [ char defAttr '['
        , horizCat $ intersperse (string defAttr "; ") (renderValue <$> vs)
        , char defAttr ']' ]
      strVal = do
        for vs $ \case
          ValueChar c -> Just c
          _           -> Nothing
    in
      case strVal of
        Just cs | not (List.null cs) ->
          string (defAttr `withForeColor` red) $ show @String cs
        _ -> img

renderIdentifier :: Identifier -> Vty.Image
renderIdentifier =
  string (defAttr `withStyle` bold) .
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
  -> Vty.Picture
renderModel
    enableIdentifiersResolution
    mLastEvent
    nodes
    edges
    _cursors = pic
  where
    pic =
      renderImageElements .
      snd . elementsPartition .
      collageElements .
      layoutCollage (either (error "ActiveZone box") imageBox) $
      rView
    rView = layoutVertical rLastEvent rNodes
    rLastEvent = Right <$>
      case mLastEvent of
        Nothing -> layoutEmptyImage
        Just lastEvent -> layoutSingleton $
          string defAttr (show lastEvent)
    rNodes = List.foldr layoutVertical (Right <$> layoutEmptyImage) $
      renderNode enableIdentifiersResolution nodes <$>
        nodesToposort nodes edges

data ActiveZone = ActiveZone NodeId

layoutActivate
  :: (Ord x, Ord y)
  => ActiveZone
  -> Layout x y a
  -> Layout x y (Either ActiveZone a)
layoutActivate activeZone layout = Layout $ \getBox ->
  let
    box = collageBox collage
    collage = layoutCollage (getBox . Right) layout
  in
    collageSuperimpose
      (collageElement box (Left activeZone))
      (Right <$> collage)

renderNode ::
  EnableIdentifiersResolution ->
  Nodes ->
  NodeInfo ->
  Layout Int Int (Either ActiveZone Image)
renderNode
    (EnableIdentifiersResolution enableIdentifiersResolution)
    nodes
    nodeInfo = layoutActivate activeZone layout
  where
    NodeInfo nodeId node edges = nodeInfo
    PerDirection outwardEdges inwardEdges = edges
    activeZone = ActiveZone nodeId
    layout = layoutSingleton rNode
      -- TODO: limit the active zone to the node body
      -- by using more granular layouts
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
        rEdgeValueWidth = imageWidth rEdgeValue + imageWidth rSourceNodeId
        (padLeftWidth, padRightWidth) =
          distribExcess maxREdgeValueWidth rEdgeValueWidth
      in
        ( (Option . Just . Max) rEdgeValueWidth
        , horizCat
            [ rSourceNodeId
            , string defAttr " ──"
            , string defAttr (List.replicate padLeftWidth '─')
            , rEdgeValue
            , string defAttr (List.replicate padRightWidth '─')
            , string defAttr "──┤" ] )
    renderOutwardEdge maxREdgeValueWidth edge =
      let
        rTargetNodeId = renderNodeReference (edge ^. edgeTarget)
        rEdgeValue = renderValue $ edge ^. edgeValue
        rEdgeValueWidth = imageWidth rEdgeValue
        (padLeftWidth, padRightWidth) =
          distribExcess maxREdgeValueWidth rEdgeValueWidth
      in
        ( (Option . Just . Max) rEdgeValueWidth
        , horizCat
            [ string defAttr "├──"
            , string defAttr (List.replicate padLeftWidth '─')
            , rEdgeValue
            , string defAttr (List.replicate padRightWidth '─')
            , string defAttr "── "
            , rTargetNodeId ] )
    (rEmptyOutwardEdge, rNodeOutwardEdges) =
      let
        (maxREdgeValueWidth, rEdges) =
          sequenceA $ renderOutwardEdge maxREdgeValueWidth' <$>
            outwardEdges
        maxREdgeValueWidth' = option 0 getMax maxREdgeValueWidth
        rEmptyEdge = char defAttr '│'
      in
        (rEmptyEdge, rEdges)
    (rEmptyInwardEdge, rNodeInwardEdges) =
      let
        (maxREdgeValueWidth, rEdges) =
          sequenceA $ renderInwardEdge maxREdgeValueWidth' <$>
            inwardEdges
        maxREdgeValueWidth' = option 0 getMax maxREdgeValueWidth
        rEmptyEdge =
          translateX (maxREdgeValueWidth' + 5) $
            char defAttr '│'
      in
        (rEmptyEdge, rEdges )
    rNode =
      let
        rNodeDef = horizCat
          [ char defAttr ' '
          , if enableIdentifiersResolution
              then rNodeValue
              else horizCat
                [ rNodeId
                , string defAttr ": "
                , rNodeValue ]
          , char defAttr ' ' ]
        rBlock = vertCat
          [ pad (imageWidth rEmptyInwardEdge - 1) 0 0 0 rBlockRoof
          , rBlockBody
          , pad (imageWidth rEmptyInwardEdge - 1) 0 0 0 rBlockFloor ]
        rBlockHeight =
          List.length rNodeInwardEdges `max`
          List.length rNodeOutwardEdges `max`
          imageHeight rNodeDef
        (rInwardTopPad, rInwardBottomPad) =
          distribExcess rBlockHeight (List.length rNodeInwardEdges)
        (rOutwardTopPad, rOutwardBottomPad) =
          distribExcess rBlockHeight (List.length rNodeOutwardEdges)
        (rNodeDefTopPad, rNodeDefBottomPad) =
          distribExcess rBlockHeight (imageHeight rNodeDef)
        rBlockRoof = horizCat
          [ char defAttr '┌'
          , string defAttr $
              List.replicate (imageWidth rNodeDef) '─'
          , char defAttr '┐' ]
        rBlockFloor = horizCat
          [ char defAttr '└'
          , string defAttr $
              List.replicate (imageWidth rNodeDef) '─'
          , char defAttr '┘' ]
        rBlockBody = horizCat
          [ (vertCat . List.concat)
              [ List.replicate rInwardTopPad rEmptyInwardEdge
              , rNodeInwardEdges
              , List.replicate rInwardBottomPad rEmptyInwardEdge ]
          , (vertCat . List.concat)
              [ List.replicate rNodeDefTopPad (resize 0 1 emptyImage)
              , [rNodeDef]
              , List.replicate rNodeDefBottomPad (resize 0 1 emptyImage) ]
          , (vertCat . List.concat)
              [ List.replicate rOutwardTopPad rEmptyOutwardEdge
              , rNodeOutwardEdges
              , List.replicate rOutwardBottomPad rEmptyOutwardEdge ] ]
      in
        rBlock


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
