module Source.Server.State
  ( ServerState
  , serverStateNodes
  , serverStateEdges
  , serverStateCursors
  , serverStateFreeIdentifier
  , serverStateClients
  , serverStateEmpty
  , serverStateModel
  , serverStateNewIdentifier
  , serverStateRegisterClient
  , serverStateUnregisterClient
  , serverStateCreateNode
  , serverStateInsertEdge
  , serverStateAssignCursor
  , serverStateSample
  , module Source.Server.State.Client
  ) where

import Control.Lens
import Control.Monad.State
import Control.Exception
import Data.Foldable

import Source.Value
import Source.Identifier
import Source.Model
import Source.Server.State.Client

data ServerState = ServerState
  { _serverStateNodes :: Nodes
  , _serverStateEdges :: Edges
  , _serverStateCursors :: Cursors
  , _serverStateFreeIdentifier :: Identifier
  , _serverStateClients :: Clients
  } deriving ()

makeLenses ''ServerState

serverStateEmpty :: ServerState
serverStateEmpty = ServerState
  { _serverStateNodes = nodesEmpty
  , _serverStateEdges = edgesEmpty
  , _serverStateCursors = cursorsEmpty
  , _serverStateFreeIdentifier = minBound
  , _serverStateClients = clientsEmpty }

serverStateModel :: Lens' ServerState (Nodes, Edges, Cursors)
serverStateModel = lens getter setter
  where
    getter serverState =
      ( _serverStateNodes serverState
      , _serverStateEdges serverState
      , _serverStateCursors serverState )
    setter serverState (nodes, edges, cursors) =
      serverState
        { _serverStateNodes = nodes
        , _serverStateEdges = edges
        , _serverStateCursors = cursors }

-- | Return the current free identifier and update the stored one.
serverStateNewIdentifier :: State ServerState Identifier
serverStateNewIdentifier = serverStateFreeIdentifier <<%= succ

serverStateRegisterClient :: Client -> State ServerState ClientId
serverStateRegisterClient client =
  zoom serverStateClients $
    clientsRegister client

serverStateUnregisterClient :: ClientId -> State ServerState ()
serverStateUnregisterClient clientId = do
  client <- uses serverStateClients $ clientsGet clientId
  for_ (client ^. clientCursorId) $ \cursorId ->
    serverStateCursors %= cursorsDelete cursorId
  zoom serverStateClients $
    clientsUnregister clientId

serverStateCreateNode :: Value -> State ServerState NodeId
serverStateCreateNode value = do
  nodeId <- NodeId <$> serverStateNewIdentifier
  serverStateNodes %= nodesInsert nodeId (Node value)
  return nodeId

serverStateInsertEdge :: Edge -> State ServerState ()
serverStateInsertEdge edge = do
  isValidEdge <- uses serverStateNodes (nodesValidEdge edge)
  when isValidEdge $
    serverStateEdges %= edgesInsert edge

serverStateAssignCursor :: ClientId -> State ServerState CursorId
serverStateAssignCursor clientId = do
  client <- uses serverStateClients $ clientsGet clientId
  case client ^. clientCursorId of
    Just cursorId -> do
      isValidCursor <-
        uses serverStateCursors $
          cursorsMember cursorId
      assert isValidCursor $
        return cursorId
    Nothing -> do
      cursorId <- CursorId <$> serverStateNewIdentifier
      serverStateCursors %= cursorsInsert cursorId CursorNone
      zoom serverStateClients $
        clientsAssignCursor clientId cursorId
      return cursorId

serverStateSample :: State ServerState ()
serverStateSample = do
  nodeId1 <- serverStateCreateNode $
    toValue ((1, "Hello") :: (Int, String))
  nodeId2 <- serverStateCreateNode $
    toValue (("World", 2) :: (String, Int))
  serverStateInsertEdge $
    Edge nodeId1 nodeId1 $
      toValue ("self" :: String)
  serverStateInsertEdge $
    Edge nodeId2 nodeId2 $
      toValue ("self-a" :: String)
  serverStateInsertEdge $
    Edge nodeId2 nodeId2 $
      toValue ("self-b" :: String)
  serverStateInsertEdge $
    Edge nodeId2 nodeId1 $
      toValue ("sentence" :: String)
