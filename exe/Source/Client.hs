module Source.Client
  ( runClient
  ) where

import Data.IORef
import Data.Map as Map
import Data.Foldable
import Data.Maybe
import System.IO
import System.Exit
import Network
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Control.Exception (bracket, finally)
import Graphics.Vty as Vty

import Source.Model as Source
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
      (clientState ^. clientStateModel)
  clientStatePtrNodeId .= ptrNodeId
  return picture

receiveMessages :: Vty -> IORef ClientState -> Handle -> IO ()
receiveMessages vty clientStateRef handle = forever $ do
  message <- hGetMessage handle
  case message of
    MessageModelPut model ->
      atomicRunStateIORef' clientStateRef $ do
        clientStateModel .= model
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
    e <- Vty.nextEvent vty
    clientState <-
      atomicRunStateIORef' clientStateRef $ do
        clientStateLastEvent .= Just e
        get
    case e of
      EvKey (KChar 'q') [] -> exitSuccess
      EvKey (KChar 'Q') [] -> exitSuccess
      EvKey (KChar 'c') [MCtrl] -> exitSuccess
      EvKey (KChar 'd') [MCtrl] -> exitSuccess
      EvMouseDown _ _ BMiddle [] -> do
        hPutMessage handle . MessageModelEdit $
          EditActionCreateNode (toValue ())
      EvMouseDown x y BLeft [] -> do
        let mNewCursor = updateCursor (x, y) UpdateCursorTo clientState
        for_ @Maybe mNewCursor $ \newCursor ->
          hPutMessage handle . MessageModelEdit $
            EditActionCursorSet newCursor
      EvMouseDown x y BRight [] -> do
        let mNewCursor = updateCursor (x, y) UpdateCursorFrom clientState
        for_ @Maybe mNewCursor $ \newCursor ->
          hPutMessage handle . MessageModelEdit $
            EditActionCursorSet newCursor
      _ -> return ()
    picture <- atomicRunStateIORef' clientStateRef clientStatePicture
    Vty.update vty picture

data UpdateCursor = UpdateCursorTo | UpdateCursorFrom

updateCursor :: (Int, Int) -> UpdateCursor -> ClientState -> Maybe (Map Value NodeId)
updateCursor (x, y) updCur clientState = do
  clientState ^. clientStateCursorId <&> \cursorId ->
    let
      mOldCursor = modelCursorLookup cursorId (clientState ^. clientStateModel)
      defaultCursor = Map.empty
      baseCursor = fromMaybe defaultCursor mOldCursor
      mNodeId = (clientState ^. clientStatePtrNodeId) (x, y)
      cursorPart = case updCur of
        UpdateCursorTo -> "to"
        UpdateCursorFrom -> "from"
      newCursor = baseCursor & at cursorPart .~ mNodeId
    in
      newCursor

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
