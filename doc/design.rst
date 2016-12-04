=============================================
 Source — a general purpose structure editor
=============================================

An Introduction
---------------

Describe the structure of your data and get an editor for free.

Source is a client-server application. The server stores *the model* —
a directed graph of values that represents the data structure being
edited. A client connects to the server and can request the current
version of the model. However, the client isn't allowed to modify the
model locally; instead, the client sends modification requests to the
server and receives an updated version of the model. This is to ensure
that all edits are valid and multilpe clients cannot introduce
conflicting edits.

Clients can be either users or bots, although the server intentionally
makes no distinction. The possibility for multiple users to edit the
same structure gives rise to collaborative editing, and bots can
automate routine parts of the editing process. For example, special
support for a programming language can be implemented as a bot that
calls the compiler under the hood.


The Model
---------

The model stored within the server is a directed graph of values. A
value is either an integer, a character, or a list of values. The
graph consists of mutable nodes and immutable edges. Cycles,
(self-)loops, and multiple edges are all allowed.

Nodes are used to represent parts of the data structure, and edges to
define the relations between them. Each node is identified by a
non-negative integer assigned by the server when the node was
created. Identifiers are never reused during an editing session to
ease reasoning, and the clients cannot assume those identifiers to be
sequential.

Edges are triples, each containing the identifier of the source node,
the identifier of the target node, and a value that defines the
relation. Edges have no identifiers and form a multiset.
