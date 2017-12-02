{-

When editing a structure, we can navigate it (focus on its parts) and perform
certain actions (depending on the part in focus). There are parts
of the structure that need an editor of their own:

* When editing module/import structure of a project, one may want to focus
  on a particular module and edit its AST.
* When editing a particular module (an AST), one may want to focus on a
  variable name or a string literal and edit it as text.
* When editing a string literal (text), one may want to open character panel.

We formalize this idea as a subspace. A subspace contains its data structure,
editing-related metadata (undo hints, cursors, etc), and references to other
subspaces.

-}

module Source.Subspace where

import Data.Serialize as Cereal

import Source.Identifier

newtype SubspaceId = SubspaceId Identifier
  deriving (Eq, Ord, Enum, Show, Serialize)
