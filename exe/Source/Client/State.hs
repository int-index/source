module Source.Client.State
  ( ClientState(..)
  , clientStateLastEvent
  , clientStateNodes
  , clientStateEdges
  , clientStateCursors
  , clientStateCursorId
  , clientStateEmpty
  ) where

import Control.Lens
import Graphics.Vty as Vty (Event)

import Source.Model

data ClientState = ClientState
  { _clientStateLastEvent :: Maybe Vty.Event
  , _clientStateNodes :: Nodes
  , _clientStateEdges :: Edges
  , _clientStateCursors :: Cursors
  , _clientStateCursorId :: Maybe CursorId
  } deriving ()

makeLenses ''ClientState

clientStateEmpty :: ClientState
clientStateEmpty = ClientState
  { _clientStateLastEvent = Nothing
  , _clientStateNodes = nodesEmpty
  , _clientStateEdges = edgesEmpty
  , _clientStateCursors = cursorsEmpty
  , _clientStateCursorId = Nothing }
