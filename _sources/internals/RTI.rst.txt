
Run Time Information (RTI)
##########################

.. WARNING::
   This section was written while trying to understand how the RTI is
   organized. It almost certainly contains errors, and it likely
   won't be updated with the codebase, so don't belive any of the
   information here. Nevertheless, it may be helpful for a developer new
   to GHDL trying to understand the organization of the RTI.

Useful Definitions
==================

RTI

 Run Time Information. The information that is used when simulating the design.

RTIN

 Run Time Information Node. The design is organized into a directed
 graph where the architectures, signals and statements are represented
 as nodes on the graph. This graph can be cyclic since an architecture
 may be instantiated many times, and could be recursively
 instantiated.

Context

 The context of a node is the position in the elaborated design. For
 example a architecture might be instantiated 4 times in a design, but
 will have a single RTI node. The data related to that node but
 specfic to particular instantiation has an address in memory. A
 context object (`Rti_Context`) bundles an RTI node with an address for
 it's instantiation data.  If this RTI node references another RTI node we
 can find the relevant instantiation data for that node by considering the
 context.

RTII

 Run Time Instance Information. This is a record that groups an RTI
 node with any other data necessary to specify a particular
 instantiation. The RTII objects create a tree that represents the
 elaborated design. Currently they are only implemented for RTIN nodes
 of signals, ports, generics, constants and their subcomponents.


RTI Nodes / RTII Nodes
======================

All RTI node records have a `Ghdl_Rtin_Common` record as their first
element. This record contains the following elements:

Kind \: Ghdl_Rtik

 This specified what kind of node it is. For example a `process` and
 an `entity` node are both represented by `Ghdl_Rtin_Block` records
 but they are distinguished by having a different Kind.

Depth \: Ghdl_Rti_Depth

 The depth indicates the relationship between the RTI Node and the RTI
 Context. Child nodes often just use the same Context as their parent,
 and the depth indicates how far up in the hierarchy you go to find
 where the Context is based.

Mode \: Ghdl_Rti_U8

 ??

Max_Depth \: Ghdl_Rti_Depth

 ??

It's also useful to look at some of the other elements that commonly
appear in the different RTI Node records.

Loc \: Ghdl_Rti_Loc

 This is an address offset. It tells us where the instantiation data
 for this node is located relative to the data of it's parent.

Linecol \: Ghdl_Index_Type

 Refers back to a location in the source code.

Parent \: Ghdl_Rti_Access

 Points to the parent. This isn't necessarily the parent in the RTI
 tree though. For example the `Parent` of an architecture RTI node
 points at the entity node, however the parent in the tree is the
 instance RTI.

This document will now review that main classes of RTI Nodes.

Architecture RTI (Ghdl_Rtin_Block)
-----------------------------------

The architecture acts as a simple container for it's children. Create
the child tree nodes by looping through `Ghdl_Rti_Block.Children` and
keeping the context unchanged.

The information about the generics and ports access the entity RTI
nodes through `Ghdl_Rti_Block.Parent` using the same context.

The instantiation data of an architecture contains a single item, a
pointer to the RTI node. This is necessary because it is necessary to
store which of the possible architectures of this entity was
instantiated.

Entity RTI (Ghdl_Rtin_Block)
----------------------------

The RTI of an entity is a `Ghdl_Rti_Block` record (the same as the
architecture) and uses the same context as the architecture. It is
accessed via the architecture's `Parent` element. The generics and
ports can be accessed as the children of the entity.

Other Blocks (Package/Process) (Ghdl_Rtin_Block)
------------------------------------------------

The block just loops over it's children.

if_generate / case_generate (Ghdl_Rtin_Block)
---------------------------------------------

If-Generate and Case-Generate statements are represented with
`Ghdl_Rtin_Block` records with Kind `Ghdl_Rtik_If_Generate` and
`Ghdl_Rtik_Case_Generate`.

Their children are all of Kind `Ghdl_Rtik_Body`, and represent the
different possible blocks that could be selected.

The instantiation data of a if_generate or case_generate RTI contains two items:
1) A pointer to the context of the selected generate body (instance_pointer).
2) The index of the selected child (block_id)

The child node is then created from the RTI node
`Ghdl_Rtik_Body.Children(block_id)` combined with the instantiation data given by
`instance_pointer`.

for_generate (Ghdl_Rtin_Generate)
---------------------------------

For-Generate statements are represented with `Ghdl_Rtin_Generate`
records with Kind `Ghdl_Rtik_For_Generate`.

Their RTI-node structure is different from the `Ghdl_Rtin_Block`
record in that rather than having `Nbr_Child` and `Children` elements,
it has:

Child \: Ghdl_Rti_Access

 A pointer to the generate body node that is their only child.

Size \: Ghdl_Index_Type

 The amount of memory requrired for the context of their child.

The Child element is a generate body. There is only a single RTI-node
structure which Child points to, however a different context is used
each time we go around the for-generate loop.

The context of a for_generate RTI contains a single item: An address
which points at the contexts for it's children.

Each time we go around the for generate loop we increment the address
of the context by `Size` so we looking at the correct context for that
instantiation of the contexts of the loop.

One complexity of the for-generate is finding the number of times that
we go around the loop. The first element in the child generate body is
an iterator. That iterator has a type and we can get the bounds of
that type by passing it the local context. The type of the iterator
for the for-generate loop is implicitly created and placed directly
before the for_generate block, so using the local context will work.
There might be a bug if the for-generate loop uses a type that wasn't
defined implicitly.

instance (Ghdl_Rtin_Instance)
-----------------------------

An instantiation of an entity is represented by a `Ghdl_Rtin_Instance`
node with Kind `Ghdl_Rtik_Instance`.

The context contains a single item, which is a pointer to the context
of the architecture. The architecture context also contains a single
item, which is a pointer to the architecture RTI Node.

Port (Ghdl_Rtin_Object)
-----------------------

Array Kinds
-----------

Ghdl_Rtik_Type_Array
 A VHDL array where the range is not specified.

Ghdl_Rtik_Subtype_Array
 A VHDL array where the range is specified.
 A Type_Array together with the bounds.

Object_To_Base_Bound
--------------------

This function takes an object type and an object's static context
location and returns the complex context location and the bounds.

When the object is static the bounds is null (because the bounds
are held in the type definition) and the complex context is the
same as the static context.

When the object is complex the bounds is null, and the static
context location contains a pointer to the complex context
location.

When the object is unbound the static context contains a `Ghdl_Uc_Array`
record. The contains `Bounds` which points to the bounds, and `Base`
which points to the complex context location.

Array_Type (Ghdl_Rtin_Type_Array)
---------------------------------
Contains Common and Name fields followed by:

Element \: Ghdl_Rti_Access
 The type of the elements in the array.

Nbr_Dim \: Ghdl_Index_Type
 The number of dimensions in the array.
 Multidimensional arrays are not stored as arrays of arrays,
 but rather directly as multidimensional arrays.

Indexes \: Ghdl_Rti_Arr_Acc
 ??? This is an array of the indices for each dimension, but I don't
 know what kind of object they are represented by yet.

Functions acting on types don't seem to use context in the same way.
The functions are often pass the RTI object, a context (of a object
higher in the hierarcy, and a pointer to a local context (often called
layout)).

The context of an Array Type has a defined structure which is `Ghdl_Uc_Array`.
This contains a `Base` and a `Bounds` field.

Base \: Address
 Points to the complex context of the object.
Bounds \: Address
 Points to the bounds of the array.


Array Subtype (Ghdl_Rtin_Subtype_Array)
---------------------------------------
Array subtypes are represented by the `Ghdl_Rtin_Subtype_Composite`
RTI node.
The node contains the `Common` and `Name` fields, followed by

Basetype \: Ghdl_Rti_Access
 A pointer to the RTI array type which it is a subtype of.

Layout \: Ghdl_Rti_Loc
 A pointer to the context of the subtype relative to the parent context.
 The layout contains:
 a value size, a signal sizes, and the bounds.

Port / Signal / Generic / Constant / Variable (Ghdl_Rtin_Object)
----------------------------------------------------------------

The context of an object is found by taking offsetting the Context by
the `Loc` field on the object. The implementation often uses the same
Context for a group of hierarhical signals, so that the determination
of the location of the context of objects in the hierarchy must be
found using a function such as `Loc_To_Addr`.

The `Obj_Type` field of an object points at the type of the object.

A signal definition can also include placing bounds on a unbounded
type.

The tree of an object can be created by pairing the hierarchy of types
with the hierarchy of contexts.

If the type is a scalar type then the value of the object is found at:
  If the object is a port or signal then the only item in the context
   is a pointer to the signal object.  The first item in the signal object
   is a pointer to the value.
  If the object is a constant, generic or variable then the context
   contains a pointer to the value itself.

If the type is an unbound array:
  We must be at the top level of a hierarchical object.
  The context contains a pointer to the first element context,
  and a pointer to the bounds.

If the type is a static array:
  The context is the same as the context of the first element.
  The bounds are given in the layout of the type (composite).

If the type is a complex array:
  The context contains a pointer to the context of the first element.
  Because the size of the context cannot be determined at compile time
  this layer of indirection is necessary.


Record Kinds
------------
Ghdl_Rtik_Type_Record

 A standard VHDL record.

Ghdl_Rtik_Type_Unbounded_Record

 A vhdl record containing an unbounded array (directory or indirectly).

Ghdl_Rtik_Subtype_Record

 A subtype of an unbounded record where all arrays are not bounded.

Ghdl_Rtik_Subtype_Unbounded_Record

 A subtype of an unbounded record where some but not all of the previously
 unbound arrays have been bound.

Record Type (Ghdl_Rtin_Type_Record)
-----------------------------------
Can have Kind of `Ghdl_Rtik_Type_Record` or `Ghdl_Rtik_Type_Unbounded_Record`.
The record elements after `Common` and `Name` are:

Nbrel \: Ghdl_Index_Type

 Number elements in the record.

Elements \: Ghdl_Rti_Arr_Acc;

 The RTI nodes of the element defintions.

Layout \: Ghdl_Rti_Loc

 The layout is the relative address that the layout/bounds information
 of the elements will be relative to.

Record Type (Ghdl_Rtin_Type_Record)
---------------------------------------------
For an unbounded record the Layout is not used, but rather a `Bounds` must be
given.

Element Type (Ghdl_Rtin_Element)
--------------------------------
The record elements after `Common` and `Name` are:

Eltype \: Ghdl_Rti_Access
 The RTI node representing the type of the element.

Val_Off \: Ghdl_Index_Type
 For static element the offset is in the record.
 For complex element the offset is in the type layout or object layout.
 This is the offset for the value for generics or constants.

Sig_Off \: Ghdl_Index_Type
 This is the offset for the value wrapper in signals or ports.

Layout_Off \: Ghdl_Index_Type;
 For unbounded records: element layout offset in the layout.
 The layout is stores all the bounds for the various elements
 when the unbounded record is given bounds.

Examples
--------

.. code-block:: vhdl

   library ieee ;
   use ieee.std_logic_1164.all;

   package mypkg is

     type mytype is record
       a: std_logic;
       b: std_logic;
     end record;

   end package;

   library ieee ;
   use ieee.std_logic_1164.all;
   use work.mypkg.all;

   entity myentity is
     port(
       x: in mytype
       );
   end myentity;

   architecture arch of myentity is
   begin
   end arch;


What will be the structure of the RTI for the port `myentity.x`?

The architecture has a context.
Address of the architecture is A

The entity has the same context.
Address of the entity is A.

The child on the entity is the port.
Address of the port is A + 16.

A port is a record 'x'
Address of the record value is A + 16.

The record contains 'a' a std_logic vector.
Address is A + 16.

The record contains 'b' a std_logic_vector.
Address is A + 24

.. code-block:: vhdl

   library ieee ;
   use ieee.std_logic_1164.all;

   package mypkg is

     type mytype is record
       a: std_logic_vector(1 downto 0);
       b: std_logic_vector(1 downto 0);
     end record;

   end package;

   library ieee ;
   use ieee.std_logic_1164.all;
   use work.mypkg.all;

   entity myentity is
     port(
       x: in mytype
       );
   end myentity;

   architecture arch of myentity is
   begin
   end arch;

.. code-block::

   - Architecture (A)
     - Entity (A)
       - port x (A+16)
         - x.a (A+16)
         - x.a(?) (A+16)
         - x.a(?) (A+24)
         - x.b (A+32)
         - x.b(?) (A+40)
         - x.b(?) (A+48)

.. code-block:: vhdl

   library ieee ;
   use ieee.std_logic_1164.all;

   entity myentity is
     generic (
       WIDTH: natural := 2
       );
     port(
       x: in std_logic_vector(WIDTH-1 downto 0)
       );
   end myentity;

   architecture arch of myentity is
   begin
   end arch;

.. code-block::

   - Architecture (A)
     - Entity (A)
       - generic WIDTH (A+16)
       - port x (A+48) content of address (A+48) is B
         - type information
           analyze a type with context (address=A, rti=entity)
           layout is located at A+20
           so bounds is located at A+28
         - x subtype array (B)
           - x(?) (B)
           - x(?) (B+8)
