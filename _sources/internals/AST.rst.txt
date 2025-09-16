.. _INT:AST:

AST
###

Introduction
************

The AST is the main data structure of the front-end and is created by the parser.

AST stands for Abstract Syntax Tree.

This is a tree because it is a graph with nodes and links between nodes.  As the graph
is acyclic and each node but the root has only one parent (the link that points to it).
In the front-end there is only one root which represents the set of libraries.

The tree is a syntax tree because it follows the grammar of the VHDL language: there
is, for example, a node per operation (like `or`, `and` or `+`), a node per declaration,
a node per statement, and a node per design unit (like entity or architecture).  The front-end needs to represent the source file using the grammar because most of the
VHDL rules are defined according to the grammar.

Finally, the tree is abstract because it is an abstraction of the source file.  Comments and layout aren't kept in the syntax tree.  Furthermore, if you rename a
declaration or change the value of a literal, the tree will have exactly the same
shape.

But we can also say that the tree is neither abstract, nor syntaxic and nor a tree.

It is not abstract because it contains all the information from the source file
(except comments) are available in the AST, inclusing the location.  So the source
file can be reprinted (the name unparsed is also used) from the AST.  If a mechanism
is also added to deal with comments, the source file can even be pretty-printed from
the AST.

It is not purely syntactic because the semantic analysis pass decorates the tree
with semantic information.  For example, the type of each expression and sub-expression
is computed.  This is necessary to detect some semantic errors like assigning an array
to an integer.

Finally, it is not a tree anymore because new links are added during semantic
analysis.  Simple names are linked to their declaration.

The AST in GHDL
***************

The GHDL AST is described in file :file:`vhdl-nodes.ads`.

An interesting particularity about the AST is the presence of a
meta-model.

The meta-model is not formally described.  What the
meta-meta-model would be is very simple: there are elements and attributes.  An
element is composed of attributes, and an attribute is either a value
(a flag, an integer, an enumeration) or a link to an element.

(When someone wants to be clever, they often speak about meta-model in
order to confuse you.  Don't let them impress you.  The trick is to
answer them with any sentence containing 'meta-meta-model').

In the GHDL meta-model, there are only 3 elements:

* variable list of nodes (`List`).  These are like vectors as the
  length can be changed.

* Fixed lists of nodes (`Flist`).  The length of a fixed list is defined at creation.

* Nodes.  A node has a kind (`Iir_Kind` which is also defined in the file), and fields.
  The kind is set at creation and cannot be changed, while fields can be.

Or without using the word "meta-model", the AST is composed of nodes and
lists.

The meta-model describes the types of the attributes: most of them are
either a node reference, a boolean flag or a enumerated type (like
`Iir_Staticness`).  But there are also links: a reference to another
node or to a list.

The accessors for the node are generated automatically by the python
script :file:`src/scripts/pnodes.py`.

Why a meta-model ?
******************

All ASTs could have a meta-model, because the definition of elements
and attributes is very generic.  But there is a detail: the definition
of an element is static.  So for each node, the list of attributes and
their types is static and each list is a list of the same element type.
So there is no bag, nor dynamic typing.  This is per the definition of
the meta-meta-model.

But in GHDL there is an API at the meta-model level in file
:file:`vhdl-nodes_meta.ads`.  There is the list of all attribute types
in enumeration `Types_Enum`.  There is the list of all possible
attributes in enumeration `Fields_Enum`.  For a particular kind of
node, you can get the list of fields with `Get_Field` and for every
type, there is API to get or set any field of any node.

Having a meta-model API allows to build algorithm that deals with any
node.  The dumper (in file :file:`vhdl-disp_tree.ad[sb]`) is used to
dump a node and possibly its sub-nodes.  This is very useful while
debugging GHDL.  It is written using the meta-model, so it knows how to display
a boolean and the various other enumerated types, and how to display a list.  To
display a node, it just gets the kind of the type, prints the kind name and queries
all the fields of the node.  There is nothing particular to a specific kind, so you
don't need to modify the dumper if you add a node.

The dumper wouldn't be a strong enough reason by itself to have a meta-model.  But
the pass to create instances is a good one.  When a vhdl-2008 package is instantiated,
at least the package declaration is created in the AST (this is needed because there
are possibly new types).  And creating an instance using the meta-model is much
simpler (and much more generic) that creating the instance using the nodes directly.
The code to create instances is in files :file:`vhdl-sem_inst.ad[sb]`.

The meta-model API is mostly automatically generated by the python
script.

Dealing with ownership
**********************

The meta-model also structures the tree, because there is a notion of
ownership: every element (but the root) has only one parent that owns
it, and there are no cycle in the ownership.  So the tree is really a
tree.

That simplifies algorithms because it is easier to walk a tree than a
graph.  It is also easier to free a sub-tree than a sub-graph.

Getting a real tree from the parser might look obvious, but it is
not.  Consider the following VHDL declaration:

.. code-block:: vhdl

   variable v1, v2 : std_logic_vector (1 downto 0) := "00";

Both variables ``v1`` and ``v2`` share the same type and the same
initial value.  The GHDL AST uses two different strategies:

* For the type, there are two fields in the node:
  ``subtype_indication`` and ``type``.  The ``subtype_indication`` is
  owned and set only on the first variable to the output of the
  parser.  The ``type`` field is a reference and set on all variables
  to the result of analysis of ``subtype_indication``.

* For the initial value, there is only one field ``default_value``
  that is set on all variables.  But the ownership is controlled by a
  flag in the node (an attribute) named ``is_ref``.  It is set to
  false on the first variable and true for the others.

The notion of ownership is highlighted by the Rust language, and
indeed this is an important notion.  The implementation of the Rust
AST has to be investigated.

Node Type
*********

TBC: 32-bit, extensions.
