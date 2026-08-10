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

A node is a 32-bit index into a table, not a pointer, so ``Iir`` is an integer type and
``Null_Iir`` is ``0``.  Using an index rather than an address keeps a node small, makes the whole
tree relocatable, and lets the meta-model address any field of any node generically.

Every node reserves a fixed set of physical slots, and the *format* says how many.  There are two
formats:

* ``Short`` - **28 slots**: ``Field0`` .. ``Field5``, ``Flag1`` .. ``Flag18``, ``State1`` and
  ``State2``, plus the node's kind and its source location.
* ``Medium`` - **37 slots**: the same, with ``Field6`` .. ``Field12``, ``State3`` and ``State4``
  added.  A ``Medium`` node occupies two node slots in the table, which is where the extra fields
  come from.

``Short`` is the common case; ``Medium`` is used by the kinds that need more fields.  There is no
third format, so a kind needing more than a ``Medium`` node holds stores the surplus in a second
node, which is why some accessors read a field of a node other than the one they were given.

The slots are untyped.  What gives them meaning is the kind: for
:ref:`Iir_Kind_Signal_Declaration <INT:AST:Signal_Declaration>`, ``Field5`` is the subtype
indication, while for another kind the same slot holds something else entirely.  Two accessors can
also share one slot when they are mutually exclusive, which the source marks with ``Alias``.

Chains, lists and flists
************************

Nodes are linked in three different ways, and the difference matters when walking the tree.

**Chains** are singly linked lists threaded through the nodes themselves.  Every node that can be an
element of a chain has a ``Chain`` field holding the next element, and the node that owns the chain
points at the first element.  A declarative part, a statement part, a port clause - all of these are
chains.  Since the link lives in the element, a node can be in at most one chain, which is what makes
the chain an ownership relation:

.. code-block:: python

   from pyGHDL.libghdl.vhdl import nodes

   declaration = nodes.Get_Declaration_Chain(architecture)
   while declaration != nodes.Null_Iir:
       ...
       declaration = nodes.Get_Chain(declaration)

:mod:`pyGHDL.libghdl.utils` wraps that loop as :func:`~pyGHDL.libghdl.utils.chain_iter`, so the same
walk reads:

.. code-block:: python

   from pyGHDL.libghdl import utils

   for declaration in utils.chain_iter(nodes.Get_Declaration_Chain(architecture)):
       ...

**Lists** (``Iir_List``) are separate, growable vectors of node references.  They are used where a
node has to appear in a collection it does not belong to - a sensitivity list, for instance, names
signals declared elsewhere.  Iterate them with :func:`~pyGHDL.libghdl.utils.list_iter`.

**Flists** (``Iir_Flist``) are the same thing with a length fixed at creation, used where the
number of elements is known once and for all, such as the index constraints of an array subtype.
Iterate them with :func:`~pyGHDL.libghdl.utils.flist_iter`.

The reference below marks which of the three a field is, together with whether the link is owned or a
reference.

Asking the meta-model
*********************

Because the fields of a kind are known statically, an algorithm can be written against the
meta-model instead of against a particular kind.  :file:`vhdl-nodes_meta.ads`, generated by the same
script, exposes it, and :mod:`pyGHDL.libghdl.vhdl.nodes_meta` mirrors it in Python.

The most useful entry point is the ``Has_*`` predicate.  Every field has one, and it answers whether
a *kind* has that field at all:

.. code-block:: python

   from pyGHDL.libghdl.vhdl import nodes, nodes_meta

   kind = nodes.Get_Kind(node)
   if nodes_meta.Has_Label(kind):
       label = nodes.Get_Label(node)

This is how :func:`pyGHDL.dom._Utils.GetLabelOfNode` decides whether a statement carries its label in
``Label`` or, for a generate statement body, in ``Alternative_Label``: it asks the meta-model rather
than enumerating the kinds that have each field.

``Get_Fields`` returns the fields of a kind, and ``Get_Field_Type`` the type of one, which is enough
to walk an arbitrary node without knowing anything about it.  That is how the tree dumper in
:file:`vhdl-disp_tree.adb` works, and why adding a node kind does not require touching it.

Node reference
**************

The pages below are generated from :file:`src/vhdl/vhdl-nodes.ads` when the documentation is built,
by the same parser that generates the Ada accessors and the :mod:`pyGHDL.libghdl` bindings.  They
therefore cannot describe a node that does not exist, or miss a field that was added.

The order follows the ``Iir_Kind`` enumeration, grouped by the ``Iir_Kinds_*`` subtype ranges the
source declares.

.. toctree::
   :maxdepth: 1
   :glob:

   ast/*
