module Source.Client
  ( runClient
  ) where

import Data.IORef
import System.IO
import System.Exit
import Network
import Control.Monad
import Control.Concurrent
import Control.Lens
import Control.Exception (bracket, finally)
import Graphics.Vty as Vty

import Source.Protocol
import Source.Client.State
import Source.Client.Render
import Source.Util

receiveMessages :: Chan Picture -> IORef ClientState -> Handle -> IO ()
receiveMessages pictureChan clientStateRef handle = forever $ do
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
  pic <- readIORef clientStateRef <&> \clientState ->
    renderModel
      (EnableIdentifiersResolution True)
      (clientState ^. clientStateLastEvent)
      (clientState ^. clientStateNodes)
      (clientState ^. clientStateEdges)
      (clientState ^. clientStateCursors)
  writeChan pictureChan pic

sendMessages :: IO Event -> IORef ClientState -> Handle -> IO ()
sendMessages nextEvent' clientStateRef handle = do
  hPutMessage handle MessageCursorRequest
  hPutMessage handle MessageModelGet
  forever $ do
    e <- nextEvent'
    atomicRunStateIORef' clientStateRef $
      clientStateLastEvent .= Just e
    case e of
      EvKey (KChar 'q') [] -> exitSuccess
      EvKey (KChar 'Q') [] -> exitSuccess
      EvKey (KChar 'c') [MCtrl] -> exitSuccess
      EvKey (KChar 'd') [MCtrl] -> exitSuccess
      _ -> hPutMessage handle MessageModelGet

updateVtyPicture :: Vty -> Chan Vty.Picture -> IO ()
updateVtyPicture vty pictureChan = forever $ do
  picture <- readChan pictureChan
  Vty.update vty picture

withVty :: (Vty -> IO a) -> IO a
withVty = bracket createVty Vty.shutdown
  where
    createVty =
      Vty.standardIOConfig >>= \config ->
        mkVty config { mouseMode = Just True }

runClient :: IO ()
runClient = withVty $ \vty -> do
  pictureChan <- newChan
  updateVtyPictureThreadId <-
    forkIO $ updateVtyPicture vty pictureChan
  clientStateRef <- newIORef clientStateEmpty
  handle <- connectTo "localhost" (PortNumber 50113)
  initHandle handle
  receiveMessagesThreadId <-
    forkIO $ receiveMessages pictureChan clientStateRef handle
  sendMessages (nextEvent vty) clientStateRef handle
    `finally` do
      killThread receiveMessagesThreadId
      killThread updateVtyPictureThreadId
