module Source.Server.State
  ( ServerState
  , serverStateModel
  , serverStateFreeIdentifier
  , serverStateClients
  , serverStateEmpty
  , serverStateNewIdentifier
  , serverStateRegisterClient
  , serverStateUnregisterClient
  , serverStateCreateNode
  , serverStateInsertEdge
  , serverStateAssignCursor
  , serverStateEdit
  , serverStateSample
  , module Source.Server.State.Client
  ) where

import Control.Lens
import Control.Monad.State
import Control.Exception
import Data.Foldable
import Data.Monoid
import Data.Map as Map

import Source.Value
import Source.Identifier
import Source.Model
import Source.Edit
import Source.Server.State.Client

data ServerState = ServerState
  { _serverStateModel :: Model
  , _serverStateFreeIdentifier :: Identifier
  , _serverStateClients :: Clients
  } deriving ()

makeLenses ''ServerState

serverStateEmpty :: ServerState
serverStateEmpty = ServerState
  { _serverStateModel = modelEmpty
  , _serverStateFreeIdentifier = identifierZero
  , _serverStateClients = clientsEmpty }

-- | Return the current free identifier and update the stored one.
serverStateNewIdentifier :: State ServerState Identifier
serverStateNewIdentifier = serverStateFreeIdentifier <<%= identifierSucc

serverStateRegisterClient :: Client -> State ServerState ClientId
serverStateRegisterClient client =
  zoom serverStateClients $
    clientsRegister client

serverStateUnregisterClient :: ClientId -> State ServerState ()
serverStateUnregisterClient clientId = do
  client <- uses serverStateClients $ clientsGet clientId
  for_ (client ^. clientCursorId) $ \cursorId ->
    serverStateModel %= modelNodeDelete cursorId
  zoom serverStateClients $
    clientsUnregister clientId

serverStateCreateNode :: Value -> State ServerState NodeId
serverStateCreateNode value = do
  nodeId <- NodeId <$> serverStateNewIdentifier
  serverStateModel . modelNodes %= nodesInsert nodeId (Node value)
  return nodeId

serverStateInsertEdge :: Edge -> State ServerState ()
serverStateInsertEdge edge = do
  isValidEdge <- uses (serverStateModel . modelNodes) (nodesValidEdge edge)
  when isValidEdge $
    serverStateModel . modelEdges %= edgesInsert edge

serverStateAssignCursor :: ClientId -> State ServerState NodeId
serverStateAssignCursor clientId = do
  client <- uses serverStateClients $ clientsGet clientId
  case client ^. clientCursorId of
    Just cursorId -> do
      isValidCursor <-
        uses (serverStateModel . modelNodes) $
          nodesMember cursorId
      assert isValidCursor $
        return cursorId
    Nothing -> do
      cursorId <- NodeId <$> serverStateNewIdentifier
      let cursorNode = Node { _nodeValue = toValue () }
      serverStateModel . modelNodes %=
        nodesInsert cursorId cursorNode
      zoom serverStateClients $
        clientsAssignCursor clientId cursorId
      return cursorId

serverStateEdit :: ClientId -> EditAction -> State ServerState ()
serverStateEdit clientId editAction = do
  client <- uses serverStateClients $ clientsGet clientId
  case editAction of
    EditActionCursorSet cursor -> do
      for_ @Maybe (client ^. clientCursorId) $ \cursorId -> do
        let
          removeOldEdges = edgesPurge (\edge -> edge ^. edgeSource == cursorId)
          newEdges       = do
            (value, target) <- Map.toList cursor
            return Edge
              { _edgeSource = cursorId
              , _edgeTarget = target
              , _edgeValue  = value }
          insertNewEdges = appEndo $
            foldMap (Endo . edgesInsert) newEdges
        serverStateModel . modelEdges %= insertNewEdges . removeOldEdges
          -- cursorsSet cursorId (_Cursor # cursor)
    EditActionCreateNode value -> do
      _nodeId <- serverStateCreateNode value
      return ()
    EditActionInsertEdge edge ->
      serverStateInsertEdge edge

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
