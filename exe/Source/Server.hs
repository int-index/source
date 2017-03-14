module Source.Server
  ( runServer
  ) where

import Data.IORef
import Data.Monoid
import System.IO as Sys
import Network
import Control.Monad
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Lens
import Text.Show.Pretty

import Source.Protocol
import Source.Server.State
import Source.Util

portNumber :: PortNumber
portNumber = 50113

showClient :: ClientId -> Client -> String
showClient clientId client =
  "#" <> show clientId <> "@" <>
    (client ^. clientHostName) <> ":" <> show (client ^. clientPortNumber)

handleConnections
  :: Socket
  -> IORef ServerState
  -> IO ()
handleConnections sock serverStateRef = forever $ do
  -- Wait for a client to connect.
  (handle, host, port) <- accept sock
  messageChan <- newChan
  let
    client = Client
      { _clientHandle = handle
      , _clientHostName = host
      , _clientPortNumber = port
      , _clientMessageChan = messageChan
      , _clientCursorId = Nothing }
  -- Add the client to the registry.
  clientId <- atomicRunStateIORef' serverStateRef $
    serverStateRegisterClient client
  -- Handle the connected client in a separate thread.
  let handleClient' = handleClient serverStateRef clientId client
  _threadId <-
    handleClient' `forkFinally` \_ -> do
      -- Close the handle after the client is handled (sucessfully or
      -- with an exception).
      hClose handle
      -- Remove the client from the registry.
      atomicRunStateIORef' serverStateRef $
        serverStateUnregisterClient clientId
      Sys.putStrLn $
        "Done with client " <> showClient clientId client
  -- Now we can proceed to wait for other clients.
  return ()

sendMessages :: ClientId -> Client -> IO ()
sendMessages clientId client = forever $ do
  let
    messageChan = client ^. clientMessageChan
    handle = client ^. clientHandle
  message <- readChan messageChan
  Sys.putStrLn $
    "To client " <> showClient clientId client <> ": " <>
      ppShow message
  hPutMessage handle (message :: ServerMessage)

getClient :: IORef ServerState -> ClientId -> IO Client
getClient serverStateRef clientId = do
  clients <- view serverStateClients <$>
    readIORef serverStateRef
  evaluate (clientsGet clientId clients)

receiveMessages
  :: IORef ServerState
  -> ClientId
  -> IO ()
receiveMessages serverStateRef clientId = forever $ do
  client <- getClient serverStateRef clientId
  let handle = client ^. clientHandle
  message :: ClientMessage <- hGetMessage handle
  Sys.putStrLn $
    "From client " <> showClient clientId client <> ": " <>
      ppShow message
  case message of
    MessageModelGet -> do
      let messageChan = client ^. clientMessageChan
      model <- view serverStateModel <$> readIORef serverStateRef
      writeChan messageChan $
        MessageModelPut model
    MessageCursorRequest -> do
      let messageChan = client ^. clientMessageChan
      cursorId <- atomicRunStateIORef' serverStateRef $
        serverStateAssignCursor clientId
      writeChan messageChan $
        MessageCursorAssign cursorId
    MessageModelEdit editAction -> do
      atomicRunStateIORef' serverStateRef $
        serverStateEdit clientId editAction
      model <- view serverStateModel <$> readIORef serverStateRef
      clients' <- view serverStateClients <$>
        readIORef serverStateRef
      forClients clients' $ \_clientId client' -> do
        let messageChan = client' ^. clientMessageChan
        writeChan messageChan $
          MessageModelPut model

handleClient
  :: IORef ServerState
  -> ClientId
  -> Client
  -> IO ()
handleClient serverStateRef clientId client = do
  Sys.putStrLn $ "Handle client " <> showClient clientId client
  initHandle (client ^. clientHandle)
  sendMessagesThreadId <- forkIO $ sendMessages clientId client
  receiveMessages serverStateRef clientId
  killThread sendMessagesThreadId

runServer :: IO ()
runServer = do
  sock <- listenOn (PortNumber portNumber)
  Sys.putStrLn $ "Listening on port " <> show portNumber
  serverStateRef <- newIORef serverStateEmpty
  atomicRunStateIORef' serverStateRef serverStateSample
  handleConnections sock serverStateRef
