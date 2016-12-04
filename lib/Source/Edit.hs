module Source.Edit
  ( EditAction(..)
  ) where

import Data.Serialize as Cereal
import GHC.Generics (Generic)

import Source.Value
import Source.Model

data EditAction =
  EditActionCursorSet Cursor |
  EditActionCreateNode Value |
  EditActionInsertEdge Edge
  deriving (Show, Generic)

instance Serialize EditAction
