module Source.Protocol
  ( ServerMessage(..)
  , ClientMessage(..)
  , module Source.Protocol.Handle
  ) where

import Data.Serialize as Cereal
import GHC.Generics (Generic)

import Source.Protocol.Handle
import Source.Edit
import Source.Model

data ServerMessage
  = MessageModelPut Nodes Edges Cursors
  | MessageCursorAssign CursorId
  deriving (Show, Generic)

instance Serialize ServerMessage

data ClientMessage
  = MessageModelGet
  | MessageCursorRequest
  | MessageModelEdit EditAction
  deriving (Show, Generic)

instance Serialize ClientMessage
