module Source.Model
  ( PerDirection(..)
  , atOutward
  , atInward
  -- * Identifiers
  , NodeId(..)
  , _NodeId
  -- * Nodes
  , Node(..)
  , nodeValue
  , Nodes(..)
  , _Nodes
  , nodesEmpty
  , nodesLookup
  , nodesInsert
  , nodesSet
  , nodesMember
  , nodesDelete
  , nodesValidEdge
  -- * Edges
  , Edge(..)
  , edgeSource
  , edgeTarget
  , edgeValue
  , edgeRelated
  , Edges(..)
  , edgesEmpty
  , edgesInsert
  , edgesPurge
  , edgesNodeEdges
  -- * Model
  , Model(..)
  , modelNodes
  , modelEdges
  , modelEmpty
  , modelCursorLookup
  , modelNodeDelete
  ) where

import Control.Applicative
import Control.Exception (assert)
import Control.Lens
import Data.Functor
import Data.Map as Map
import Data.Serialize as Cereal
import Data.Set as Set
import GHC.Generics as Generic

import Source.Identifier
import Source.Util ()
import Source.Value

data PerDirection a = PerDirection
  { _atOutward :: a
  , _atInward :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''PerDirection

instance Applicative PerDirection where
  pure a = PerDirection a a
  PerDirection f g <*> PerDirection a b =
    PerDirection (f a) (g b)

instance Monoid a => Monoid (PerDirection a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Serialize a => Serialize (PerDirection a)

newtype NodeId = NodeId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize)

makePrisms ''NodeId

data Node = Node
  { _nodeValue :: Value
  } deriving (Eq, Show, Generic)

makeLenses ''Node

instance Serialize Node

newtype Nodes = Nodes (Map NodeId Node)
  deriving (Eq, Show, Generic)

makePrisms ''Nodes

instance Serialize Nodes

nodesEmpty :: Nodes
nodesEmpty = _Nodes # Map.empty

nodesInsert :: NodeId -> Node -> Nodes -> Nodes
nodesInsert nodeId node = over _Nodes $
  \nodes -> assert (Map.notMember nodeId nodes) $
    Map.insert nodeId node nodes

nodesSet :: NodeId -> Node -> Nodes -> Nodes
nodesSet nodeId node = over _Nodes $
  \nodes -> assert (Map.member nodeId nodes) $
    Map.insert nodeId node nodes

nodesMember :: NodeId -> Nodes -> Bool
nodesMember nodeId nodes = Map.member nodeId (nodes ^. _Nodes)

nodesLookup :: NodeId -> Nodes -> Maybe Node
nodesLookup nodeId nodes = Map.lookup nodeId (nodes ^. _Nodes)

nodesDelete :: NodeId -> Nodes -> Nodes
nodesDelete nodeId = over _Nodes $
  \nodes -> assert (Map.member nodeId nodes) $
    Map.delete nodeId nodes

data Edge = Edge
  { _edgeSource :: NodeId
  , _edgeTarget :: NodeId
  , _edgeValue :: Value
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''Edge

instance Serialize Edge

edgeRelated :: NodeId -> Edge -> Bool
edgeRelated nodeId edge =
  edge ^. edgeSource == nodeId ||
  edge ^. edgeTarget == nodeId

newtype Edges = Edges (Set Edge)
  deriving (Eq, Show, Generic)

makePrisms ''Edges

instance Serialize Edges

edgesEmpty :: Edges
edgesEmpty = _Edges # Set.empty

edgesNodeEdges :: NodeId -> Edges -> PerDirection [Edge]
edgesNodeEdges nodeId edges =
  Set.toAscList <$> PerDirection outwardEdges inwardEdges
  where
    outwardEdges = Set.filter isOutwardEdge $ edges ^. _Edges
    inwardEdges  = Set.filter isInwardEdge  $ edges ^. _Edges
    isOutwardEdge edge = edge ^. edgeSource == nodeId
    isInwardEdge  edge = edge ^. edgeTarget == nodeId

edgesPurge :: (Edge -> Bool) -> Edges -> Edges
edgesPurge f = over _Edges $ Set.filter (not . f)

edgesInsert :: Edge -> Edges -> Edges
edgesInsert edge = over _Edges $ Set.insert edge

nodesValidEdge :: Edge -> Nodes -> Bool
nodesValidEdge edge nodes =
  nodesMember (edge ^. edgeSource) nodes &&
  nodesMember (edge ^. edgeTarget) nodes

data Model = Model
  { _modelNodes :: Nodes
  , _modelEdges :: Edges
  } deriving (Eq, Show, Generic)

makeLenses ''Model

instance Serialize Model

modelEmpty :: Model
modelEmpty = Model nodesEmpty edgesEmpty

modelCursorLookup :: NodeId -> Model -> Maybe (Map Value NodeId)
modelCursorLookup cursorId model =
  nodesLookup cursorId (model ^. modelNodes) $>
    let
      (view atOutward -> outwardEdges) =
        edgesNodeEdges cursorId (model ^. modelEdges)
      edgeToCursorPart edge = (edge ^. edgeValue, edge ^. edgeTarget)
    in
      Map.fromList . fmap edgeToCursorPart $ outwardEdges

modelNodeDelete :: NodeId -> Model -> Model
modelNodeDelete nodeId =
  over modelEdges (edgesPurge (edgeRelated nodeId)) .
  over modelNodes (nodesDelete nodeId)
