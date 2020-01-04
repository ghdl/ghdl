.. _INT:AST:

AST
###

Introduction
************

The AST is the main data structure of the front-end and is created by the parser.

AST stands for Abstract Syntax Tree.

This is a tree because it is a graph with nodes and links between nodes.  As the graph
is acyclic and each node but the root has only one parent (the link that point to it).
In the front-end there is only one root which represent the set of libraries.

The tree is a syntax tree because it follows the grammar of the VHDL language: there
is for example a node per operation (like `or`, `and` or `+`), a node per declaration,
a node per statement, a node per design unit (like entity or architecture).  The front-end needs to represent the source file using the grammar because most of the
VHDL rules are defined according to the grammar.

Finally, the tree is abstract because it is an abstraction of the source file.  Comments and layout aren't kept in the syntax tree.  Furthermore, if you rename a
declaration or change the value of a literal, the tree will have exactely the same
shape.

But we can also say that the tree is neither abstract, nor syntaxic and nor a tree.

It is not abstract because it contains all the information from the source file
(except comments) are available in the AST, inclusing the location.  So the source
file can be reprinted (the name unparsed is also used) from the AST.  If a mechanism
is also added to deal with comments, the source file can even be pretty-printed from
the AST.

It is not purely syntaxic because the semantic analysis pass decorate the tree
with semantic information.  For example the type of each expression and sub-expression
is computed.  This is necessary to detect some semantic error like assigning an array
to an integer.

Finally, it is not anymore a tree because new links are added during semantic
analysis.  Simple names are linked to their declaration.

The AST in GHDL
***************

The GHDL AST is described in file :file:`vhdl-nodes.ads`.

An interesting particularity about the AST is the presence of a
meta-model.  The meta-model is not formally described (so, there is no
meta-meta-model), but it is very simple: there are 3 kinds of vertices:

* variable list of nodes (`List`).  These are like vectors as the
  length can be changed.

* Fixed lists of nodes (`Flist`).  The length of a fixed list is defined at creation.

* Nodes.  A node has a kind (`Iir_Kind` which is also defined in the file), and fields.
  The kind is set at creation and cannot be changed, while fields can be.

The meta-model describes the type of the fields: most of them are
either a node reference, a boolean flag or a enumerated type (like
`Iir_Staticness`).  But there are various node references.  A node can either owns
another node, which means this is the main reference to the node; or a node can
reference another node without owning it.

Why a meta-model ?
******************

Having a meta-model allows to build algorithm that deals with any
node.  The dumper (in file :file:`vhdl-disp_tree.ad[sb]`) is used to
dump a node and possibly its sub-nodes.  This is very useful while
debugging GHDL.  It is written using the meta-model, so it knows how to display
a boolean and the various other enumerated types, and how to display a list.  To
display a node, it just gets the kind of the type, prints the kind name and queries
all the fields of the node.  There is nothing particular to a specific kind, so you
don't need to modify the dumper if you add a node.

The dumper won't be a strong enough reason by itself to have a meta-model.  But
the pass to create instances is a good one.  When a vhdl-2008 package is instantiated,
at least the package declaration is created in the AST (this is needed because there
are possibly new types).  And creating an instance using the meta-model is much
simpler (and much more generic) that creating the instance using directly the nodes.
The code to create instances is in files :file:`vhdl-sem_inst.ad[sb]`.

The meta-model also structures the tree.  We know that each node is owned only by one node, and that each node is owned (except the top-level one).  So it is possible to
free a sub-tree.  It is also possible to check that the tree is well-formed.

Dealing with ownership
**********************

TBC: two fields, Is_Ref, Second_XXX; Rust & Scripts.

Node Type
*********

TBC: 32-bit, extensions.
