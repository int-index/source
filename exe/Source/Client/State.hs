module Source.Client.State
  ( ClientState(..)
  , clientStateLastEvent
  , clientStateModel
  , clientStateCursorId
  , clientStatePtrNodeId
  , clientStateEmpty
  ) where

import Control.Lens
import Graphics.Vty as Vty (Event)

import Slay.Core
import Source.Model

data ClientState = ClientState
  { _clientStateLastEvent :: Maybe Vty.Event
  , _clientStateModel :: Model
  , _clientStateCursorId :: Maybe NodeId
  , _clientStatePtrNodeId :: Offset -> Maybe NodeId
  } deriving ()

makeLenses ''ClientState

clientStateEmpty :: ClientState
clientStateEmpty = ClientState
  { _clientStateLastEvent = Nothing
  , _clientStateModel = modelEmpty
  , _clientStateCursorId = Nothing
  , _clientStatePtrNodeId = const Nothing }
