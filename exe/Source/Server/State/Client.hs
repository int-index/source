module Source.Server.State.Client
  ( Client(..)
  , clientHandle
  , clientHostName
  , clientPortNumber
  , clientMessageChan
  , clientCursorId
  , ClientId
  , Clients(..)
  , clientsFreeClientId
  , clientsClients
  , clientsEmpty
  , clientsRegister
  , clientsUnregister
  , clientsAssignCursor
  , ClientNotRegistered(..)
  , clientsGet
  , forClients
  ) where

import Network (HostName, PortNumber)
import System.IO (Handle)
import Control.Concurrent (Chan)
import Control.Lens
import Control.Monad.State
import Control.Exception
import Data.Coerce
import Data.Map.Strict as MapS
import Data.Foldable
import Data.Maybe
import Control.Exception (assert)

import Source.Protocol (ServerMessage)
import Source.Model
import Source.Util

-- Information about a connected client.
data Client = Client
  { _clientHandle :: Handle
  , _clientHostName :: HostName
  , _clientPortNumber :: PortNumber
  , _clientMessageChan :: Chan ServerMessage
  , _clientCursorId :: Maybe NodeId
  } deriving ()

makeLenses ''Client

-- Client identifiers are not reused.
newtype ClientId = ClientId Int
  deriving (Enum, Eq, Ord)

instance Bounded ClientId where
  minBound = ClientId 0
  maxBound = ClientId maxBound

instance Show ClientId where
  showsPrec = coerce (showsPrec @Int)

data Clients = Clients
  { _clientsFreeClientId :: ClientId
  , _clientsClients :: MapS.Map ClientId Client
  } deriving ()

makeLenses ''Clients

clientsEmpty :: Clients
clientsEmpty = Clients minBound MapS.empty

-- | Return the current free identifier and update the stored one.
clientsNewClientId :: State Clients ClientId
clientsNewClientId = clientsFreeClientId <<%= succ

clientsRegister :: Client -> State Clients ClientId
clientsRegister client = do
  clientId <- clientsNewClientId
  isAvailableClientId <-
    uses clientsClients $
      MapS.notMember clientId
  assert isAvailableClientId $ do
    clientsClients %= MapS.insert clientId client
    return clientId

clientsUnregister :: ClientId -> State Clients ()
clientsUnregister clientId = do
  isRegisteredClientId <-
    uses clientsClients $
      MapS.member clientId
  assert isRegisteredClientId $ do
    clientsClients %= MapS.delete clientId

clientsAssignCursor :: ClientId -> NodeId -> State Clients ()
clientsAssignCursor clientId cursorId = do
  isRegisteredClientId <-
    uses clientsClients $
      MapS.member clientId
  assert isRegisteredClientId $ do
    clientsClients %= MapS.alter
      (_Just . clientCursorId .~ Just cursorId)
      clientId

data ClientNotRegistered = ClientNotRegistered ClientId
  deriving (Show)

instance Exception ClientNotRegistered

clientsGet :: ClientId -> Clients -> Client
clientsGet clientId clients =
  fromMaybe (throw $ ClientNotRegistered clientId) mClient
  where
    mClient = MapS.lookup clientId $
      clients ^. clientsClients

forClients
  :: Applicative f
  => Clients
  -> (ClientId -> Client -> f ())
  -> f ()
forClients clients f =
  traverse_ (uncurry f) $
    MapS.toAscList (clients ^. clientsClients)
