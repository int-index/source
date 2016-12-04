module Source.Client
  ( runClient
  ) where

import Data.IORef
import System.IO
import System.Exit
import Network
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Control.Exception (bracket, finally)
import Graphics.Vty as Vty

import Source.Model
import Source.Value
import Source.Edit
import Source.Protocol
import Source.Client.State
import Source.Client.Render
import Source.Util

clientStatePicture :: State ClientState Vty.Picture
clientStatePicture = do
  clientState <- get
  let
    (picture, ptrNodeId) = renderModel
      (EnableIdentifiersResolution True)
      (clientState ^. clientStateLastEvent)
      (clientState ^. clientStateNodes)
      (clientState ^. clientStateEdges)
      (clientState ^. clientStateCursors)
  clientStatePtrNodeId .= ptrNodeId
  return picture

receiveMessages :: Vty -> IORef ClientState -> Handle -> IO ()
receiveMessages vty clientStateRef handle = forever $ do
  message <- hGetMessage handle
  case message of
    MessageModelPut nodes edges cursors ->
      atomicRunStateIORef' clientStateRef $ do
        clientStateNodes .= nodes
        clientStateEdges .= edges
        clientStateCursors .= cursors
    MessageCursorAssign cursorId ->
      atomicRunStateIORef' clientStateRef $ do
        clientStateCursorId .= Just cursorId
  picture <- atomicRunStateIORef' clientStateRef clientStatePicture
  Vty.update vty picture

sendMessages :: Vty -> IORef ClientState -> Handle -> IO ()
sendMessages vty clientStateRef handle = do
  hPutMessage handle MessageCursorRequest
  hPutMessage handle MessageModelGet
  forever $ do
    e <- nextEvent vty
    clientState <-
      atomicRunStateIORef' clientStateRef $ do
        clientStateLastEvent .= Just e
        get
    case e of
      EvKey (KChar 'q') [] -> exitSuccess
      EvKey (KChar 'Q') [] -> exitSuccess
      EvKey (KChar 'c') [MCtrl] -> exitSuccess
      EvKey (KChar 'd') [MCtrl] -> exitSuccess
      EvKey (KChar 'n') [] -> do
        hPutMessage handle . MessageModelEdit $
          EditActionCreateNode (toValue ())
      EvMouseDown x y BLeft [] -> do
        let
          mNodeId = (clientState ^. clientStatePtrNodeId) (x, y)
          newCursor = maybe CursorNone CursorSingle mNodeId
        hPutMessage handle . MessageModelEdit $
          EditActionCursorSet newCursor
      _ -> return ()
    picture <- atomicRunStateIORef' clientStateRef clientStatePicture
    Vty.update vty picture

withVty :: (Vty -> IO a) -> IO a
withVty = bracket createVty Vty.shutdown
  where
    createVty =
      Vty.standardIOConfig >>= \config ->
        mkVty config { mouseMode = Just True }

runClient :: IO ()
runClient = withVty $ \vty -> do
  clientStateRef <- newIORef clientStateEmpty
  handle <- connectTo "localhost" (PortNumber 50113)
  initHandle handle
  receiveMessagesThreadId <-
    forkIO $ receiveMessages vty clientStateRef handle
  sendMessages vty clientStateRef handle
    `finally` do
      killThread receiveMessagesThreadId
