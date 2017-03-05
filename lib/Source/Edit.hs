module Source.Edit
  ( EditAction(..)
  ) where

import Data.Serialize as Cereal
import GHC.Generics (Generic)
import Data.Map as Map

import Source.Value
import Source.Model

-- TODO: Now I need a language for graph manipulations.

data EditAction =
  EditActionCursorSet (Map Value NodeId) |
  EditActionCreateNode Value |
  EditActionInsertEdge Edge
  deriving (Show, Generic)

instance Serialize EditAction
