module Source.Model
  ( PerDirection(..)
  , atOutward
  , atInward
  -- Identifiers
  , NodeId(..)
  , _NodeId
  , CursorId(..)
  , _CursorId
  -- Nodes
  , Node(..)
  , nodeValue
  , Nodes(..)
  , _Nodes
  , nodesEmpty
  , nodesLookup
  , nodesInsert
  , nodesValidEdge
  -- Edges
  , Edge(..)
  , edgeSource
  , edgeTarget
  , edgeValue
  , Edges(..)
  , edgesEmpty
  , edgesInsert
  , edgesNodeEdges
  -- Cursors
  , Cursor(..)
  , _Cursor
  , Cursors(..)
  , _Cursors
  , cursorsEmpty
  , cursorsInsert
  , cursorsSet
  , cursorsMember
  , cursorsDelete
  , cursorsLookup
  -- Model
  , Model(..)
  , modelNodes
  , modelEdges
  , modelCursors
  , modelEmpty
  ) where

import Control.Lens
import Data.Serialize as Cereal
import Data.EnumMap.Lazy as EnumMapL
import Data.MultiSet as MultiSet
import Data.Map as Map
import GHC.Generics as Generic
import Control.Applicative
import Control.Exception (assert)

import Source.Identifier
import Source.Value
import Source.Util

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
  deriving (Eq, Ord, Enum, Bounded, Show, Serialize)

makePrisms ''NodeId

newtype CursorId = CursorId Identifier
  deriving (Eq, Ord, Enum, Bounded, Show, Serialize)

makePrisms ''CursorId

data Node = Node
  { _nodeValue :: Value
  } deriving (Eq, Show, Generic)

makeLenses ''Node

instance Serialize Node

newtype Nodes = Nodes (EnumMapL NodeId Node)
  deriving (Eq, Show, Generic)

makePrisms ''Nodes

instance Serialize Nodes

nodesEmpty :: Nodes
nodesEmpty = _Nodes # EnumMapL.empty

nodesInsert :: NodeId -> Node -> Nodes -> Nodes
nodesInsert nodeId node = over _Nodes $
  \nodes -> assert (EnumMapL.notMember nodeId nodes) $
    EnumMapL.insert nodeId node nodes

nodesMember :: NodeId -> Nodes -> Bool
nodesMember nodeId nodes = EnumMapL.member nodeId (nodes ^. _Nodes)

nodesLookup :: NodeId -> Nodes -> Maybe Node
nodesLookup nodeId nodes = EnumMapL.lookup nodeId (nodes ^. _Nodes)

data Edge = Edge
  { _edgeSource :: NodeId
  , _edgeTarget :: NodeId
  , _edgeValue :: Value
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''Edge

instance Serialize Edge

newtype Edges = Edges (MultiSet Edge)
  deriving (Eq, Show, Generic)

makePrisms ''Edges

instance Serialize Edges

edgesEmpty :: Edges
edgesEmpty = _Edges # MultiSet.empty

edgesNodeEdges :: NodeId -> Edges -> PerDirection [Edge]
edgesNodeEdges nodeId edges =
  MultiSet.toAscList <$> PerDirection outwardEdges inwardEdges
  where
    outwardEdges = MultiSet.filter isOutwardEdge $ edges ^. _Edges
    inwardEdges  = MultiSet.filter isInwardEdge  $ edges ^. _Edges
    isOutwardEdge edge = edge ^. edgeSource == nodeId
    isInwardEdge  edge = edge ^. edgeTarget == nodeId

edgesInsert :: Edge -> Edges -> Edges
edgesInsert edge = over _Edges $ MultiSet.insert edge

data Cursor = Cursor (Map Value NodeId)
  deriving (Eq, Show, Generic)

makePrisms ''Cursor

instance Serialize Cursor

newtype Cursors = Cursors (EnumMapL CursorId Cursor)
  deriving (Eq, Show, Generic)

makePrisms ''Cursors

instance Serialize Cursors

cursorsEmpty :: Cursors
cursorsEmpty = _Cursors # EnumMapL.empty

cursorsInsert :: CursorId -> Cursor -> Cursors -> Cursors
cursorsInsert cursorId cursor = over _Cursors $
  \cursors -> assert (EnumMapL.notMember cursorId cursors) $
    EnumMapL.insert cursorId cursor cursors

cursorsSet :: CursorId -> Cursor -> Cursors -> Cursors
cursorsSet cursorId cursor = over _Cursors $
  \cursors -> assert (EnumMapL.member cursorId cursors) $
    EnumMapL.insert cursorId cursor cursors

cursorsMember :: CursorId -> Cursors -> Bool
cursorsMember cursorId = EnumMapL.member cursorId . view _Cursors

cursorsDelete :: CursorId -> Cursors -> Cursors
cursorsDelete cursorId = over _Cursors $
  \cursors -> assert (EnumMapL.member cursorId cursors) $
    EnumMapL.delete cursorId cursors

cursorsLookup :: CursorId -> Cursors -> Maybe Cursor
cursorsLookup cursorId cursors = EnumMapL.lookup cursorId (cursors ^. _Cursors)

nodesValidEdge :: Edge -> Nodes -> Bool
nodesValidEdge edge nodes =
  nodesMember (edge ^. edgeSource) nodes &&
  nodesMember (edge ^. edgeTarget) nodes

data Model = Model
  { _modelNodes :: Nodes
  , _modelEdges :: Edges
  , _modelCursors :: Cursors
  } deriving (Eq, Show, Generic)

makeLenses ''Model

instance Serialize Model

modelEmpty :: Model
modelEmpty = Model nodesEmpty edgesEmpty cursorsEmpty
