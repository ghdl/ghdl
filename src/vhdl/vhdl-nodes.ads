--  Tree node definitions.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
with Ada.Unchecked_Deallocation;
with Types; use Types;
with Vhdl.Tokens; use Vhdl.Tokens;
with Vhdl.Nodes_Priv;
with Vhdl.Lists;
with Vhdl.Flists;
with PSL.Types; use PSL.Types;

package Vhdl.Nodes is
   --  This package defines the semantic tree and functions to handle it.
   --  The tree is roughly based on IIR (Internal Intermediate Representation),
   --  [AIRE/CE Advanced Intermediate Representation with Extensibility,
   --   Common Environment.  http://www.vhdl.org/aire/index.html [DEAD LINK] ]
   --  but oriented object features are not used, and often, functions
   --  or fields have changed.

   --  Note: this tree is also used during syntactic analysis, but with
   --  a little bit different meanings for the fields.
   --  The parser (parse package) build the tree.
   --  The semantic pass (sem, sem_expr, sem_names, ...) transforms it into a
   --  semantic tree.

   --  Documentation:
   --  Only the semantic aspect is to be fully documented.
   --  The syntactic aspect is only used between parse and sem.

   --  Each node of the tree is a record of type iir, based on the private (so
   --  hidden) type nodes.node_type.
   --
   --  Each node in the tree should be referenced only once (as this is a
   --  tree).  There are some exceptions to this rule for space optimization
   --  purpose:
   --    - the interface list of implicit subprograms are shared among the
   --      implicit subprograms.
   --
   --  As the tree represents an AST it is in fact a graph: for there are links
   --  from names to the declaration.  However these links are marked
   --  explicitly as Ref.  A Ref doesn't own the node.
   --
   --  The distinction between owner and reference is very important as it
   --  allows to use this meta-model for processing: displaying the tree
   --  (without creating infinite loops), copying the tree for instantiation...
   --
   --  There is a little bit of overhead due to this choice:
   --    - some fields looks duplicated: for example an object declaration has
   --      both a type field and a subtype indication field, array subtypes
   --      have both an index_subtype_list and an index_constraint_list.
   --    - Maybe_Ref trick: the Is_Ref flag tells whether the Maybe_Ref are
   --      owner or ref.
   --    - Maybe_Forward_Ref: the Is_Forward_Ref tells whether the field is
   --      ref or forward_ref

   --  The root of a semantic tree is a library_declaration.
   --  All the library_declarations are kept in a private list, held by
   --  package libraries.
   --  Example of a tree:
   --   library_declaration
   --   +-- design_file
   --       +-- design_unit
   --       |   +-- entity_declaration
   --       +-- design_unit
   --           +-- architecture_body
   --  ...

   --  Since the tree can represent all the libraries and their contents, it
   --  is not always loaded into memory.
   --  When a library is loaded, only library_declaration, design_file,
   --  design_unit and library_unit nodes are created.  When a design_unit is
   --  really loaded, the design_unit node is not replaced but modified (ie,
   --  access to this node are still valid).

   --  To add a new kind of node:
   --   the name should be of the form iir_kind_NAME
   --   add iir_kind_NAME in the definition of type iir_kind_type
   --   document the node below: grammar, methods.
   --   for each methods, add the name if the case statement in the body
   --     (this enables the methods)
   --   add an entry in disp_tree (debugging)
   --   handle this node in Errorout.Disp_Node

   --  Meta-grammar
   --  This file is processed by a tool to automatically generate the body, so
   --  it must follow a meta-grammar.
   --
   --  The low level representation is described in nodes.ads.
   --
   --  The literals for the nodes must be declared in this file like this:
   --   type Iir_Kind is
   --      (
   --       Iir_Kind_AAA,
   --   ...
   --       Iir_Kind_ZZZ
   --      );
   --  The tool doesn't check for uniqueness as this is done by the compiler.
   --
   --  It is possible to declare ranges of kinds like this:
   --   subtype Iir_Kinds_RANGE is Iir_Kind range
   --     Iir_Kind_FIRST ..
   --   --Iir_Kind_MID
   --     Iir_Kind_LAST;
   --  Literals Iir_Kind_MID are optional (FIXME: make them required ?), but
   --  if present all the values between FIRST and LAST must be present.
   --
   --  The methods appear after the comment: '   -- General methods.'
   --  They have the following format:
   --    --  Field: FIELD ATTR (CONV)
   --   function Get_NAME (PNAME : PTYPE) return RTYPE;
   --   procedure Set_NAME (PNAME : PTYPE; RNAME : RTYPE);
   --  'FIELD' indicate which field of the node is used to store the value.
   --  ATTR is optional and if present must be one of:
   --     Ref: the field is a reference to an existing node
   --     Chain: the field contains a chain of nodes
   --     Chain_Next: the field contains the next element of a chain (present
   --      only on one field: Set/Get_Chain).
   --  ' (CONV)' is present if the type of the value (indicated by RTYPE) is
   --  different from the type of the field.  CONV can be either 'uc' or 'pos'.
   --  'uc' indicates an unchecked conversion while 'pos' a pos/val conversion.
   --
   --  Nodes content is described between '   -- Start of Iir_Kind.' and
   --  '   -- End of Iir_Kind.' like this:
   --   -- Iir_Kind_NODE1 (FORMAT1)
   --   -- Iir_Kind_NODE2 (FORMAT2)
   --   --
   --   --   Get/Set_NAME1 (FIELD1)
   --   --
   --   --   Get/Set_NAME2 (FIELD2)
   --   --   Get/Set_NAME3 (Alias FIELD2)
   --   --
   --   -- Only for Iir_Kind_NODE1:
   --   --   Get/Set_NAME4 (FIELD3)
   --  Several nodes can be described at once; at least one must be described.
   --  Fields FIELD1, FIELD2, FIELD3 must be different, unless 'Alias ' is
   --  present.  The number of spaces is significant.  The 'Only for ' lines
   --  are optional and there may be several of them.

   -------------------------------------------------
   -- General methods (can be used on all nodes): --
   -------------------------------------------------

   --  Create a node of kind KIND.
   --    function Create_Iir (Kind: Iir_Kind) return Iir;
   --
   --  Deallocate a node.  Deallocate fields that where allocated by
   --  create_iir.
   --   procedure Free_Iir (Target: in out Iir);
   --
   --  Get the kind of the iir.
   --  See below for the (public) list of kinds.
   --   function Get_Kind (N : Iir) return Iir_Kind;

   --  Get the location of the node: ie the current position in the source
   --  file when the node was created.  This is a little bit fuzzy.
   --
   --   procedure Set_Location (Target : Iir; Location: Location_Type);
   --   function Get_Location (Target : Iir) return Location_Type;
   --
   --  Copy a location from a node to another one.
   --   procedure Location_Copy (Target: in out Iir; Src: in Iir);

   --  The next line marks the start of the node description.
   -- Start of Iir_Kind.

   --------------------------------------------------
   --  A set of methods are associated with a kind.  --
   --------------------------------------------------

   -- Iir_Kind_Design_File (Medium)
   --  LRM93 11
   --  design_file ::= design_unit { design_unit }
   --
   --  The library containing this design file.
   --   Get/Set_Library (Field0)
   --   Get/Set_Parent (Alias Field0)
   --
   --   Get/Set_File_Dependence_List (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  Time when the whole file has been analyzed.  This allows ordering
   --  analysis and detecting obsolete units across libraries.
   --   Get/Set_Analysis_Time_Stamp (Field3)
   --
   --   Get/Set_File_Checksum (Field4)
   --
   --  Get the chain of unit contained in the file.  This is a simply linked
   --  chain, but the tail is kept to speed-up appending operation.
   --   Get/Set_First_Design_Unit (Field5)
   --
   --   Get/Set_Last_Design_Unit (Field6)
   --
   --  Source file entry for this file.
   --   Get/Set_Design_File_Source (Field7)
   --
   --  Identifier for the design file file name and dirname.
   --   Get/Set_Design_File_Filename (Field12)
   --   Get/Set_Design_File_Directory (Field11)
   --
   --  Flag used during elaboration.  Set when the file was already seen.
   --   Get/Set_Elab_Flag (Flag3)

   -- Iir_Kind_Design_Unit (Medium)
   --  LRM93 11
   --  design_unit ::= context_clause library_unit
   --
   --  The design_file containing this design unit.
   --   Get/Set_Design_File (Field0)
   --   Get/Set_Parent (Alias Field0)
   --
   --  Get the chain of context clause.
   --   Get/Set_Context_Items (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  This is a symbolic date, only used as a order of analysis of design
   --  units.
   --   Get/Set_Date (Field4)
   --
   --  Get/Set the library unit, which can be an entity, an architecture,
   --  a package, a package body or a configuration.
   --   Get/Set_Library_Unit (Field7)
   --
   --  Collision chain for units.
   --   Get/Set_Hash_Chain (Field5)
   --
   --  Get the list of design units that must be analysed before this unit.
   --  See LRM93 11.4 for the rules defining the order of analysis.
   --   Get/Set_Dependence_List (Field8)
   --
   --  FIXME: this field can be put in the library_unit, since it is only used
   --  when the units have been analyzed.
   --   Get/Set_Analysis_Checks_List (Field9)
   --
   --  Set the line and the offset in the line, only for the library manager.
   --  This is valid until the file is really loaded in memory.  On loading,
   --  location will contain all this information.
   --   Get/Set_Design_Unit_Source_Pos (Field10)
   --
   --   Get/Set_Design_Unit_Source_Line (Field11)
   --
   --   Get/Set_Design_Unit_Source_Col (Field12)
   --
   --  Get/Set the date state, which indicates whether this design unit is in
   --  memory or not.
   --   Get/Set_Date_State (State1)
   --
   --  Flag used during elaboration.  Set when the file was already seen.
   --   Get/Set_Elab_Flag (Flag3)
   --
   --  Flags used during configuration
   --   Get/Set_Configuration_Mark_Flag (Flag4)
   --   Get/Set_Configuration_Done_Flag (Flag5)

   -- Iir_Kind_Library_Clause (Short)
   --
   --  LRM08 13.2 Design libraries
   --
   --  library_clause ::= LIBRARY logical_name_list ;
   --
   --  logical_name_list ::= logical_name { , logical_name }
   --
   --  logical_name ::= identifier
   --
   --  Note: a library_clause node is created for every logical_name.
   --  As a consequence, the scope of the library starts after the logical_name
   --  and not after the library_clause.  However, since an identifier
   --  can only be used as a logical_name, and since the second occurrence has
   --  no effect, this is correct.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Library_Declaration (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)

   ---------------
   --  Literals --
   ---------------

   -- Iir_Kind_String_Literal8 (Short)
   --
   --  Number of literals in the expanded string.
   --   Get/Set_String_Length (Field4)
   --
   --  Before analysis, this is the ASCII code of each character in the string.
   --  After analysis, this is the position of each literal.
   --   Get/Set_String8_Id (Field5)
   --
   --   Get/Set_Literal_Length (Field0)
   --
   --  Used for computed literals.  Literal_Origin contains the expression
   --  whose value was computed during analysis and replaces the expression.
   --   Get/Set_Literal_Origin (Field2)
   --
   --  Same as Type, but marked as property of that node.
   --   Get/Set_Literal_Subtype (Field3)
   --
   --   Get/Set_Type (Field1)
   --
   --  Base of the bit_string (corresponds to letters 'b', 'o', 'd' or 'x' in
   --  the base specifier).
   --   Get/Set_Bit_String_Base (Flag12,Flag13,Flag14)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --  True if the bit string is signed, (ie letter 's' is present in the base
   --  specifier).
   --   Get/Set_Has_Signed (Flag1)
   --
   --  True if the letter 'u' is present in the base specifier.
   --   Get/Set_Has_Sign (Flag2)
   --
   --  True if the integer specifying the length is present.
   --   Get/Set_Has_Length (Flag3)

   -- Iir_Kind_Integer_Literal (Short)
   --
   --  Get/Set the value of the integer.
   --   Get/Set_Value (Field4,Field5)
   --
   --   Get/Set_Literal_Length (Field0)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Floating_Point_Literal (Short)
   --
   --  The value of the literal.
   --   Get/Set_Fp_Value (Field4,Field5)
   --
   --   Get/Set_Literal_Length (Field0)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Null_Literal (Short)
   --  The null literal, which can be a disconnection or a null access.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Physical_Int_Literal (Short)
   -- Iir_Kind_Physical_Fp_Literal (Short)
   --
   -- Only for Iir_Kind_Physical_Int_Literal:
   --  The multiplicand.
   --   Get/Set_Value (Field4,Field5)
   --
   -- Only for Iir_Kind_Physical_Fp_Literal:
   --  The multiplicand.
   --   Get/Set_Fp_Value (Field4,Field5)
   --
   --  The name of the physical unit.
   --   Get/Set_Unit_Name (Field3)
   --
   --   Get/Set_Literal_Length (Field0)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Type (Field1)
   --
   --  Must be set to locally except for time literal, which is globally.
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Simple_Aggregate (Short)
   --  This node can only be generated by evaluation: it is an unidimensional
   --  positional aggregate.
   --
   --  Same as Type, but marked as property of that node.
   --   Get/Set_Literal_Subtype (Field3)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --  List of elements (Index 0 is for the leftest element).
   --   Get/Set_Simple_Aggregate_List (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Overflow_Literal (Short)
   --  This node can only be generated by evaluation to represent an error: out
   --  of range, division by zero...
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Unaffected_Waveform (Short)
   --  The 'unaffected' reserved word when it appears in the sources.
   --
   --  Unaffected replaces a waveform element, so it is considered to be part
   --  of a chain.  But it is always alone in the chain.
   --   Get/Set_Chain (Field2)

   -------------
   --  Tuples --
   -------------

   -- Iir_Kind_Association_Element_By_Expression (Short)
   -- Iir_Kind_Association_Element_By_Name (Short)
   -- Iir_Kind_Association_Element_Open (Short)
   -- Iir_Kind_Association_Element_By_Individual (Short)
   -- Iir_Kind_Association_Element_Package (Short)
   -- Iir_Kind_Association_Element_Type (Short)
   -- Iir_Kind_Association_Element_Subprogram (Short)
   -- Iir_Kind_Association_Element_Terminal (Short)
   --  These are used for association element of an association list with
   --  an interface (ie subprogram call, port map, generic map).
   --
   --   Get/Set_Formal (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   -- Only for Iir_Kind_Association_Element_By_Expression:
   -- Only for Iir_Kind_Association_Element_By_Name:
   -- Only for Iir_Kind_Association_Element_Package:
   -- Only for Iir_Kind_Association_Element_Type:
   -- Only for Iir_Kind_Association_Element_Subprogram:
   -- Only for Iir_Kind_Association_Element_Terminal:
   --   Get/Set_Actual (Field3)
   --
   -- Only for Iir_Kind_Association_Element_By_Individual:
   --   Get/Set_Individual_Association_Chain (Field4)
   --
   --  A function call or a type conversion for the actual.
   --  FIXME: should be a name ?
   -- Only for Iir_Kind_Association_Element_By_Expression:
   -- Only for Iir_Kind_Association_Element_By_Name:
   --   Get/Set_Actual_Conversion (Field4)
   --
   -- Only for Iir_Kind_Association_Element_Type:
   --   Get/Set_Subprogram_Association_Chain (Field4)
   --
   --  A function call or a type conversion for the formal.
   -- Only for Iir_Kind_Association_Element_By_Expression:
   -- Only for Iir_Kind_Association_Element_By_Name:
   --   Get/Set_Formal_Conversion (Field5)
   --
   --  Owner of Actual_Type if needed.
   -- Only for Iir_Kind_Association_Element_By_Individual:
   --   Get/Set_Actual_Type_Definition (Field3)
   --
   -- Only for Iir_Kind_Association_Element_By_Individual:
   -- Only for Iir_Kind_Association_Element_Type:
   --   Get/Set_Actual_Type (Field5)
   --
   --  Get/Set the whole association flag (true if the formal is associated in
   --  whole and not individually, see LRM93 4.3.2.2)
   --   Get/Set_Whole_Association_Flag (Flag1)
   --
   --   Get/Set_Collapse_Signal_Flag (Flag2)
   --
   -- Only for Iir_Kind_Association_Element_Open:
   --   Get/Set_Artificial_Flag (Flag3)
   --
   --   Get/Set_In_Formal_Flag (Flag4)
   --
   -- Only for Iir_Kind_Association_Element_By_Individual:
   --  Must be Locally unless there is an error on one choice.
   --   Get/Set_Choice_Staticness (State1)

   -- Iir_Kind_Waveform_Element (Short)
   --
   --   Get/Set_We_Value (Field1)
   --
   --   Get/Set_Time (Field3)
   --
   --   Get/Set_Chain (Field2)

   -- Iir_Kind_Conditional_Waveform (Short)
   --
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Waveform_Chain (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Conditional_Expression (Short)
   --  LRM08 10.5.3
   --  conditional_expressions ::=
   --      expression WHEN condition
   --    { ELSE expression WHEN condition }
   --    [ ELSE expression ]
   --
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Choice_By_Others (Short)
   -- Iir_Kind_Choice_By_None (Short)
   -- Iir_Kind_Choice_By_Range (Short)
   -- Iir_Kind_Choice_By_Name (Short)
   -- Iir_Kind_Choice_By_Expression (Short)
   --  (Iir_Kinds_Choice)
   --
   --  Used by:
   --  Iir_Kind_Aggregate
   --  Iir_Kind_Case_Statement
   --  Iir_Kind_Case_Generate_Statement
   --  Iir_Kind_Concurrent_Selected_Signal_Assignment
   --  Iir_Kind_Simultaneous_Case_Statement
   --
   --  The location of the first alternative is set on:
   --  'when' for case statement, selected assignment and case generate,
   --  '(' or ',' for aggregates.
   --  The location of the following alternatives is set on '|'.
   --
   --   Get/Set_Parent (Field0)
   --
   --  For a list of choices, only the first one is associated, the following
   --  associations have the same_alternative_flag set.
   --   Get/Set_Chain (Field2)
   --
   --  Should be a simple_name.
   -- Only for Iir_Kind_Choice_By_Name:
   --   Get/Set_Choice_Name (Field5)
   --
   -- Only for Iir_Kind_Choice_By_Expression:
   --   Get/Set_Choice_Expression (Field5)
   --
   -- Only for Iir_Kind_Choice_By_Range:
   --   Get/Set_Choice_Range (Field5)
   --
   --  Get/Set what is associated with the choice.  There are two different
   --  nodes, one for simple association and the other for chain association.
   --  They don't have the same properties (normal vs chain), so the right
   --  field must be selected according to the property to have working
   --  walkers. Both fields are never used at the same time.
   --
   --  For:
   --  * an expression for an aggregate
   --  * an individual association
   --  * a generate_statement_body chain for a case_generate_statement
   --   Get/Set_Associated_Expr (Field3)
   --   Get/Set_Associated_Block (Alias Field3)
   --
   --  For
   --  * a waveform_chain for a concurrent_selected_signal_assignment,
   --  * a sequential statement chain for a case_statement.
   --   Get/Set_Associated_Chain (Field4)
   --
   --  Set when share the same association as the previous one.
   --   Get/Set_Same_Alternative_Flag (Flag1)
   --
   --  For aggregates: if True, associated expression is for one element.
   --   Get/Set_Element_Type_Flag (Flag2)
   --
   -- Only for Iir_Kind_Choice_By_Range:
   -- Only for Iir_Kind_Choice_By_Expression:
   --   Get/Set_Choice_Staticness (State1)

   -- Iir_Kind_Entity_Aspect_Entity (Short)
   --
   --   Get/Set_Entity_Name (Field2)
   --
   --  A simple name for the architecture.  The named entity can be:
   --  * Null_Iir if the architecture is not known.
   --  * a design unit if the architecture is known but not loaded.
   --  * an architecture body if the architecture is loaded.
   --   Get/Set_Architecture (Field3)

   -- Iir_Kind_Entity_Aspect_Open (Short)

   -- Iir_Kind_Entity_Aspect_Configuration (Short)
   --
   --   Get/Set_Configuration_Name (Field1)

   -- Iir_Kind_Psl_Hierarchical_Name (Short)
   --
   --   Get/Set_Entity_Name (Field2)
   --
   --   Get/Set_Architecture (Field3)

   -- Iir_Kind_Block_Configuration (Short)
   --
   --  LRM08 3.4.2 Block configuration
   --  block_configuration ::=
   --    FOR block_specification
   --      { use_clause }
   --      { configuration_item }
   --    END FOR;
   --
   --  configuration_item ::=
   --      block_configuration
   --    | component_configuration
   --
   --  block_specification ::=
   --      /architecture_/name
   --    | /block_statement_/label
   --    | /generate_statement_/label [ ( generate_specification ) ]
   --
   --   Get/Set_Parent (Field0)
   --
   --  Note: for default block configurations of iterative generate statement,
   --  the block specification is an indexed_name, whose index_list is others.
   --  The name designates either a block statement or a generate statement
   --  body.
   --   Get/Set_Block_Specification (Field5)
   --
   --  Only use_clause are allowed here.
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Configuration_Item_Chain (Field3)
   --
   --  Single linked list of block configuration that apply to the same
   --  for scheme generate block.
   --   Get/Set_Prev_Block_Configuration (Field4)

   -- Iir_Kind_Binding_Indication (Medium)
   --
   --   Get/Set_Default_Entity_Aspect (Field1)
   --
   --  The entity aspect.
   --  It is a iir_kind_entity_aspect_entity, iir_kind_entity_aspect_open or
   --  iir_kind_entity_aspect_configuration.  This may be transformed into a
   --  declaration by semantic.
   --   Get/Set_Entity_Aspect (Field3)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Port_Map_Aspect_Chain (Field9)

   -- Iir_Kind_Component_Configuration (Short)
   -- Iir_Kind_Configuration_Specification (Short)
   --
   --  LRM08 7.3 Configuration specification
   --
   --  configuration_specification ::=
   --      simple_configuration_specification
   --    | compound_configuration_specification
   --
   --  simple_configuration_specification ::=
   --     FOR component_specification binding_indication ;
   --     [ END FOR ; ]
   --
   --  compound_configuration_specification ::=
   --     FOR component_specification binding_indication ;
   --        verification_unit_binding_indication ;
   --        { verification_unit_binding_indication ; }
   --     END FOR ;
   --
   --  component_specification ::=
   --     instantiation_list : component_name
   --
   --  instantiation_list ::=
   --      instantiation_label { , instantiation_label }
   --    | OTHERS
   --    | ALL
   --
   --  The location points to 'for'.
   --
   --  The declaration containing this type declaration.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Component_Name (Field5)
   --
   --  A list, list_others or list_all.
   --   Get/Set_Instantiation_List (Field1)
   --
   -- Only for Iir_Kind_Component_Configuration:
   --   Get/Set_Block_Configuration (Field4)
   --
   --   Get/Set_Binding_Indication (Field3)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Disconnection_Specification (Short)
   --
   --  LRM08 7.4 Disconnection specification
   --
   --  disconnection_specification ::=
   --    DISCONNECT guarded_signal_specification AFTER time_expression ;
   --
   --  guarded_signal_specification ::=
   --    guarded_signal_list : type_mark
   --
   --  signal_list ::=
   --       signal_name { , signal_name }
   --     | OTHERS
   --     | ALL
   --
   --  The declaration containing this type declaration.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Signal_List (Field3)
   --
   --   Get/Set_Type_Mark (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Step_Limit_Specification (Short)
   --
   --  AMS-LRM17 7.5 Step limit specification
   --
   --  step_limit_specification ::=
   --    LIMIT quantity_specification WITH real_expression ;
   --
   --  quantity_specification ::=
   --    quantity_list : type_mark
   --
   --  quantity_list ::=
   --       quantity_name { , quantity_name }
   --     | OTHERS
   --     | ALL
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Quantity_List (Field3)
   --
   --   Get/Set_Type_Mark (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Block_Header (Medium)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Port_Chain (Field7)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Port_Map_Aspect_Chain (Field9)

   -- Iir_Kind_Entity_Class (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Entity_Class (Field3)

   -- Iir_Kind_Attribute_Specification (Medium)
   --
   --  LRM08 7.2 Attribute specification
   --
   --  attribute_specification ::=
   --     ATTRIBUTE attribute_designator OF entity_specification
   --        IS expression ;
   --
   --  entity_specification ::= entity_name_list : entity_class
   --
   --  entity_name_list ::=
   --       entity_designator { , entity_designator }
   --     | OTHERS
   --     | ALL
   --
   --  entity_designator ::= entity_tag [ signature ]
   --
   --  entity_tag ::= simple_name | character_literal | operator_symbol
   --
   --  LRM08 8.6 Attribute names
   --
   --  attribute_designator ::= /attribute/_simple_name
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Entity_Class (Field3)
   --
   --   Get/Set_Entity_Name_List (Field8)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Attribute_Value_Spec_Chain (Field4)
   --
   --  Always a simple name.
   --   Get/Set_Attribute_Designator (Field6)
   --
   --   Get/Set_Attribute_Specification_Chain (Field7)
   --
   --   Get/Set_Static_Attribute_Flag (Flag2)

   -- Iir_Kind_Attribute_Value (Short)
   --  An attribute value is the element of the chain of attribute of an
   --  entity, marking the entity as decorated by the attribute.
   --  This node is built only by sem.
   --  In fact, the node is member of the chain of attribute of an entity, and
   --  of the chain of a parent node containing all the attributes value for
   --  a scope.
   --  This makes elaboration (and more precisely, expression evaluation)
   --  easier.
   --
   --  Chain of attribute_value for the attribute specification
   --   Get/Set_Spec_Chain (Field2)
   --
   --   Get/Set_Type (Field1)
   --
   --  Chain of all attribute_value for the node containing declarations
   --   Get/Set_Value_Chain (Field0)
   --
   --   Get/Set_Designated_Entity (Field3)
   --
   --   Get/Set_Attribute_Specification (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Psl_Expression (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Psl_Expression (Field3)

   -- Iir_Kind_Psl_Prev (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Count_Expression (Field2)
   --
   --   Get/Set_Clock_Expression (Field4)
   --
   --  Reference to the default_clock node.
   --   Get/Set_Default_Clock (Field3)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Psl_Stable (Short)
   -- Iir_Kind_Psl_Rose (Short)
   -- Iir_Kind_Psl_Fell (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Clock_Expression (Field4)
   --
   --  Reference to the default_clock node.
   --   Get/Set_Default_Clock (Field3)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Psl_Onehot (Short)
   -- Iir_Kind_Psl_Onehot0 (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Signature (Medium)
   --
   --  LRM08 4.5.3 Signatures
   --
   --  signature ::= '[' [ type_mark { , type_mark } ] [ RETURN type_mark ] ']'
   --
   --   Get/Set_Signature_Prefix (Field1)
   --
   --   Get/Set_Type_Marks_List (Field2)
   --
   --   Get/Set_Return_Type_Mark (Field8)

   -- Iir_Kind_Overload_List (Short)
   --
   --   Get/Set_Overload_List (Field1)

   -------------------
   --  Declarations --
   -------------------

   -- Iir_Kind_Foreign_Module (Medium)
   --
   --   Get/Set_Design_Unit (Field0)
   --   Get/Set_Parent (Alias Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Foreign_Node (Field1)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Port_Chain (Field7)

   -- Iir_Kind_Entity_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Port_Chain (Field7)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field4)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Bound_Vunit_Chain (Field8)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Has_Begin (Flag10)

   -- Iir_Kind_Architecture_Body (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --  Name of the entity declaration for the architecture.
   --   Get/Set_Entity_Name (Field2)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field4)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --  The default configuration created by canon.  This is a design unit.
   --   Get/Set_Default_Configuration_Declaration (Field6)
   --
   --   Get/Set_Bound_Vunit_Chain (Field8)
   --
   --   Get/Set_Foreign_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Configuration_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --  Name of the entity of a configuration.
   --   Get/Set_Entity_Name (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Block_Configuration (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Package_Header (Medium)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)

   -- Iir_Kind_Package_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Package_Header (Field6)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --  For nested packages
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --  The package body (not the unit).
   --   Get/Set_Package_Body (Field4)
   --
   --   Get/Set_Package_Origin (Field7)
   --
   --  If true, the package need a body.
   --   Get/Set_Need_Body (Flag1)
   --
   --  True for uninstantiated package that will be macro-expanded for
   --  simulation.  The macro-expansion is done by canon, so controlled by
   --  back-end.  The reason of macro-expansion is presence of interface
   --  type.
   --   Get/Set_Macro_Expanded_Flag (Flag2)
   --
   --  True if the package declaration has least one package instantiation
   --  declaration whose uninstantiated declaration needs both a body and
   --  macro-expansion.  In that case, the instantiation needs macro-expansion
   --  of their body.
   --   Get/Set_Need_Instance_Bodies (Flag3)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Package_Body (Short)
   --  Note: a body is not a declaration, that's the reason why there is no
   --  _declaration suffix in the name.
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --  For nested packages.
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --  The corresponding package declaration.
   --   Get/Set_Package (Field4)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Package_Instantiation_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  The name of the uninstantiated package as it appear in the sources.  May
   --  be Null_Iir.
   --   Get/Set_Uninstantiated_Package_Name (Field7)
   --
   --  The uninstantiated package declaration.
   --   Get/Set_Uninstantiated_Package_Decl (Field9)
   --
   --   Get/Set_Instance_Source_File (Field10)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --  For nested packages
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --  For macro-expanded packages: the body.
   --   Get/Set_Instance_Package_Body (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Context_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --  Get the chain of context clause.
   --   Get/Set_Context_Items (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Vunit_Declaration (Medium)
   -- Iir_Kind_Vmode_Declaration (Medium)
   -- Iir_Kind_Vprop_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Hierarchical_Name (Field1)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Vunit_Item_Chain (Field6)
   --
   --   Get/Set_Verification_Block_Configuration (Field4)
   --
   -- Only for Iir_Kind_Vunit_Declaration:
   --   Get/Set_Bound_Vunit_Chain (Field8)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Has_Begin (Flag10)

   -- Iir_Kind_Library_Declaration (Short)
   --
   --  Design files in the library.
   --   Get/Set_Design_File_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  This node is used to contain all a library.  Only internally used.
   --  Name (identifier) of the library.
   --   Get/Set_Identifier (Field3)
   --
   --  Most recent date in this library.
   --   Get/Set_Date (Field4)
   --
   --   Get/Set_Library_Directory (Field5)
   --
   --  Used to compute dependencies.
   --   Get/Set_Elab_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --  Set on vendor libraries to turn off warnings on unbounded instantiation.
   --  The vendor libraries are those providing components/entities for
   --  hard-macros.
   --   Get/Set_Vendor_Library_Flag (Flag1)

   -- Iir_Kind_Component_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Port_Chain (Field7)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Has_Is (Flag7)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   --  LRM08 6.6 Alias declarations
   --
   --  alias_declaration ::=
   --     ALIAS alias_designator [ : subtype_indication ] IS
   --        name [ signature ] ;
   --
   --  alias_designator ::= identifier | character_literal | operator_symbol
   --
   --  Object aliases and non-object aliases are represented by two different
   --  nodes, as their semantic is different.  The parser only creates object
   --  alias declaration nodes, but sem_decl replaces the node for non-object
   --  alias declarations.

   -- Iir_Kind_Object_Alias_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Name (Field4)
   --
   --  The subtype indication may not be present.
   --   Get/Set_Subtype_Indication (Field5)
   --
   --  The type can be deduced from the subtype indication, but this field is
   --  present for uniformity (and speed).
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_After_Drivers_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Non_Object_Alias_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Name (Field4)
   --
   --   Get/Set_Alias_Signature (Field5)
   --
   --  Set when the alias was implicitly created (by Sem) because of an
   --  explicit alias of a type.
   --   Get/Set_Implicit_Alias_Flag (Flag1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Anonymous_Type_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type_Definition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  Used for informative purpose only.
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Definition (Field4)
   --
   --  Set if the type declaration completes an incomplete type declaration
   --   Get/Set_Incomplete_Type_Declaration (Field5)

   -- Iir_Kind_Type_Declaration (Short)
   --
   --  LRM08 6.3 Type declarations
   --
   --  type_declaration ::=
   --       full_type_declaration
   --     | incomplete_type_declaration
   --
   --  full_type_declaration ::=
   --     TYPE identifier IS type_definition ;
   --
   --  type_definition ::=
   --       scalar_type_definition
   --     | composite_type_definition
   --     | access_type_definition
   --     | file_type_definition
   --     | protected_type_definition
   --
   --  LRM08 5.4.2 Incomplete type declarations
   --
   --  incomplete_type_declaration ::=
   --       TYPE identifier ;
   --
   --   Get/Set_Parent (Field0)
   --
   --  Definition of the type.
   --  Note: the type definition can be a real type (unconstrained array,
   --  enumeration, file, record, access) or a subtype (integer, floating
   --  point).
   --  The parser set this field to null_iir for an incomplete type
   --  declaration.  This field is set to an incomplete_type_definition node
   --  when analyzed.
   --   Get/Set_Type_Definition (Field1)
   --   Get/Set_Type (Alias Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  Set if the type declaration completes an incomplete type declaration
   --   Get/Set_Incomplete_Type_Declaration (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Subtype_Declaration (Short)
   --
   --  LRM08 6.3 Subtype declarations
   --
   --  subtype_declaration ::=
   --     SUBTYPE identifier IS subtype_indication ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  For integer and real types, the subtype_indication of the implicitly
   --  declared subtype for the type is the subtype definition.
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Nature_Declaration (Short)
   --
   --  AMS-LRM17 6.11 Nature and subnature declarations
   --  nature_declaration ::=
   --    NATURE identifier IS nature_definition ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Nature_Definition (Field1)
   --   Get/Set_Nature (Alias Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Subnature_Declaration (Short)
   --
   --  AMS-LRM17 6.11 Nature and subnature declarations
   --  subnature_declaration ::=
   --    SUBNATURE identifier IS subnature_indication ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subnature_Indication (Field5)
   --
   --   Get/Set_Nature (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Interface_Signal_Declaration (Short)
   -- Iir_Kind_Interface_Constant_Declaration (Short)
   -- Iir_Kind_Interface_Variable_Declaration (Short)
   -- Iir_Kind_Interface_File_Declaration (Short)
   -- Iir_Kind_Interface_Quantity_Declaration (Short)
   --
   --  Get/Set the parent of an interface declaration.
   --  The parent is an entity declaration, a subprogram specification, a
   --  component declaration, a loop statement, a block declaration or ??
   --  Useful to distinguish a port and an interface.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --  Must always be null_iir for iir_kind_interface_file_declaration.
   --   Get/Set_Default_Value (Field4)
   --
   --  The type can be deduced from the subtype indication, but this field is
   --  present for uniformity (and speed).
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Mode (Flag13,Flag14,Flag15)
   --
   -- Only for Iir_Kind_Interface_Signal_Declaration:
   --   Get/Set_Has_Disconnect_Flag (Flag1)
   --
   -- Only for Iir_Kind_Interface_Signal_Declaration:
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_After_Drivers_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   -- Only for Iir_Kind_Interface_Signal_Declaration:
   --   Get/Set_Guarded_Signal_Flag (Flag8)
   --
   -- Only for Iir_Kind_Interface_Signal_Declaration:
   --   Get/Set_Signal_Kind (Flag9)
   --
   --   Get/Set_Has_Mode (Flag10)
   --
   --   Get/Set_Has_Class (Flag11)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   -- Only for Iir_Kind_Interface_Signal_Declaration:
   -- Only for Iir_Kind_Interface_Constant_Declaration:
   --   Get/Set_Open_Flag (Flag7)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Interface_Terminal_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subnature_Indication (Field5)
   --
   --   Get/Set_Nature (Field1)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Has_Mode (Flag10)
   --
   --   Get/Set_Has_Class (Flag11)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Interface_Type_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Interface_Type_Subprograms (Field4)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Open_Flag (Flag7)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Interface_Package_Declaration (Medium)
   --
   --  LRM08 6.5.5 Interface package declarations
   --
   --  interface_package_declaration ::=
   --     PACKAGE identifier IS NEW /uninstantiated_package/_name
   --        interface_package_generic_map_aspect
   --
   --  interface_package_generic_map_aspect ::=
   --       generic_map_aspect
   --     | GENERIC MAP ( <> )                  --  Represented by Null_Iir
   --     | GENERIC MAP ( DEFAULT )             --  Not yet implemented
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Uninstantiated_Package_Name (Field7)
   --
   --   Get/Set_Uninstantiated_Package_Decl (Field9)
   --
   --   Get/Set_Instance_Source_File (Field10)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Open_Flag (Flag7)

   -- Iir_Kind_Function_Declaration (Medium)
   -- Iir_Kind_Procedure_Declaration (Medium)
   --
   --  LRM08 4.2 Subprogram declarations
   --
   --  subprogram_declaration ::= subprogram_specification ;
   --
   --  subprogram_specification ::=
   --     procedure_specification | function_specification
   --
   --  procedure_specification ::=
   --     PROCEDURE designator
   --        subprogram_header
   --        [ [ PARAMETER ] ( formal_parameter_list ) ]
   --
   --  function_specification ::=
   --     [ PURE | IMPURE ] FUNCTION designator
   --        subprogram_header
   --        [ [ PARAMETER ] ( formal_parameter_list ) ] RETURN type_mark
   --
   --  designator ::= identifier | operator_symbol
   --
   --  operator_symbol ::= string_literal
   --
   --  Note: the subprogram specification of a body is kept, but should be
   --  ignored if there is a subprogram declaration.  The function
   --  Is_Second_Subprogram_Specification returns True on such specification.
   --
   --  The declaration containing this subprogram declaration.
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Return_Type (Field1)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Type (Alias Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  For string, the identifier is the corresponding reserved word.
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subprogram_Hash (Field4)
   --
   --   Get/Set_Interface_Declaration_Chain (Field5)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --  --Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Implicit_Definition (Field7)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Return_Type_Mark (Field8)
   --
   --   Get/Set_Subprogram_Body (Field9)
   --
   --   Get/Set_Subprogram_Depth (Field10)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Return_Identifier (Field11)
   --
   --   Get/Set_Overload_Number (Field12)
   --
   --   Get/Set_Seen_Flag (Flag1)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Pure_Flag (Flag2)
   --
   -- Only for Iir_Kind_Procedure_Declaration:
   --   Get/Set_Passive_Flag (Flag2)
   --
   --   Get/Set_Foreign_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Resolution_Function_Flag (Flag13)
   --
   -- Only for Iir_Kind_Function_Declaration:
   --   Get/Set_Has_Pure (Flag8)
   --
   --  True is the specification is immediately followed by a body.
   --   Get/Set_Has_Body (Flag9)
   --
   --   Get/Set_Has_Parameter (Flag10)
   --
   -- Only for Iir_Kind_Procedure_Declaration:
   --   Get/Set_Suspend_Flag (Flag11)
   --
   --  For an explicit subprogram: true if the declaration is an homograph of
   --  an implicit operation of a type.
   --   Get/Set_Hide_Implicit_Flag (Flag12)
   --
   --   Get/Set_Wait_State (State1)
   --
   -- Only for Iir_Kind_Procedure_Declaration:
   --   Get/Set_Purity_State (State2)
   --
   --   Get/Set_All_Sensitized_State (State3)

   -- Iir_Kind_Function_Body (Medium)
   -- Iir_Kind_Procedure_Body (Medium)
   --
   --  LRM08 4.3 Subprogram bodies
   --
   --  subprogram_body ::=
   --     subprogram_specification IS
   --        subprogram_declarative_part
   --     BEGIN
   --        subprogram_statement_part
   --     END [ subprogram_kind ] [ designator ] ;
   --
   --  subprogram_kind ::= PROCEDURE | FUNCTION
   --
   --   Get/Set_Parent (Field0)
   --
   --  The parse stage always puts a declaration before a body.
   --  Sem will remove the declaration if there is a forward declaration.
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Impure_Depth (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Sequential_Statement_Chain (Field4)
   --
   --   Get/Set_Subprogram_Specification (Field6)
   --
   --   Get/Set_Callees_List (Field7)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   -- Only for Iir_Kind_Procedure_Body:
   --   Get/Set_Suspend_Flag (Flag11)

   -- Iir_Kind_Function_Instantiation_Declaration (Medium)
   -- Iir_Kind_Procedure_Instantiation_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_Function_Instantiation_Declaration:
   --   Get/Set_Return_Type (Field1)
   --
   -- Only for Iir_Kind_Function_Instantiation_Declaration:
   --   Get/Set_Type (Alias Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subprogram_Hash (Field4)
   --
   --   Get/Set_Interface_Declaration_Chain (Field5)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --  A signature or a simple name.
   --   Get/Set_Uninstantiated_Subprogram_Name (Field7)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Instance_Source_File (Field10)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Interface_Function_Declaration (Medium)
   -- Iir_Kind_Interface_Procedure_Declaration (Medium)
   --
   --  LRM08 6.5.4 Interface subprogram declarations
   --
   --  interface_subprogram_declaration ::=
   --     interface_subprogram_specification
   --       [ IS interface_subprogram_default ]
   --
   --  interface_subprogram_specification ::=
   --     interface_procedure_specification | interface_function_specification
   --
   --  interface_procedure_specification ::=
   --     PROCEDURE designator
   --        [ [ PARAMETER ] ( formal_parameter_list ) ]
   --
   --  interface_function_specification ::=
   --     [ PURE | IMPURE ] FUNCTION designator
   --        [ [ PARAMETER ] ( formal_parameter_list ) ] RETURN type_mark
   --
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_Interface_Function_Declaration:
   --   Get/Set_Return_Type (Field1)
   --
   -- Only for Iir_Kind_Interface_Function_Declaration:
   --   Get/Set_Type (Alias Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  For string, the identifier is the corresponding reserved word.
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subprogram_Hash (Field4)
   --
   --   Get/Set_Interface_Declaration_Chain (Field5)
   --
   --   Get/Set_Return_Type_Mark (Field8)
   --
   --   Get/Set_Subprogram_Depth (Field10)
   --
   --   Get/Set_Seen_Flag (Flag1)
   --
   -- Only for Iir_Kind_Interface_Function_Declaration:
   --   Get/Set_Pure_Flag (Flag2)
   --
   --   Get/Set_Foreign_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   -- Only for Iir_Kind_Interface_Function_Declaration:
   --   Get/Set_Resolution_Function_Flag (Flag13)
   --
   -- Only for Iir_Kind_Interface_Function_Declaration:
   --   Get/Set_Has_Pure (Flag8)
   --
   --   Get/Set_Has_Parameter (Flag10)
   --
   --   Get/Set_All_Sensitized_State (State3)
   --
   --   Get/Set_Open_Flag (Flag7)

   -- Iir_Kind_Signal_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Default_Value (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --  For a non-resolved signal: null_iir if the signal has no driver, or
   --  a process/concurrent_statement for which the signal should have a
   --  driver.  This is used to catch at analyse time unresolved signals with
   --  several drivers.
   --  -- Get/Set_Signal_Driver (Field7)
   --
   --   Get/Set_Has_Disconnect_Flag (Flag1)
   --
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_After_Drivers_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Guarded_Signal_Flag (Flag8)
   --
   --   Get/Set_Signal_Kind (Flag9)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Guard_Signal_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Guard_Expression (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Guard_Sensitivity_List (Field4)
   --
   --   Get/Set_Block_Statement (Field5)
   --
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Guarded_Signal_Flag (Flag8)
   --
   --   Get/Set_Signal_Kind (Flag9)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Signal_Attribute_Declaration (Short)
   --
   --  Chain of implicit signals created from signal attribute.  This is just
   --  an helper so that translation can create these implicit signals at the
   --  same time as user signal declarations.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --  Chain of signals
   --   Get/Set_Signal_Attribute_Chain (Field3)

   -- Iir_Kind_Suspend_State_Declaration (Short)
   --
   --  Implicit state variable to handle suspension.  Added after semantic
   --  analysis.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Suspend_State_Chain (Field4)

   -- Iir_Kind_Constant_Declaration (Medium)
   -- Iir_Kind_Iterator_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  For iterator, this is the reconstructed subtype indication.
   --   Get/Set_Subtype_Indication (Field5)
   --
   -- Only for Iir_Kind_Iterator_Declaration:
   --   Get/Set_Discrete_Range (Field4)
   --
   -- Only for Iir_Kind_Constant_Declaration:
   --  Default value of a deferred constant points to the full constant
   --  declaration.
   --   Get/Set_Default_Value (Field4)
   --
   --  Note that the type may be extracted from the default_value if the
   --  subtype indication is unconstrained.
   --   Get/Set_Type (Field1)
   --
   -- Only for Iir_Kind_Constant_Declaration:
   --  Summary:
   --  |  constant C1 : integer;        -- Deferred declaration (in a package)
   --  |  constant C2 : integer := 4;   -- Declaration
   --  |  constant C1 : integer := 3;   -- Full declaration (in a body)
   --  | NAME   Deferred_declaration  Deferred_declaration_flag
   --  |  C1      Null_iir or C1' (*)     True
   --  |  C2      Null_Iir                False
   --  |  C1'     C1                      False
   --  |(*): Deferred_declaration is Null_Iir as long as the full declaration
   --  |   has not been analyzed.
   --   Get/Set_Deferred_Declaration (Field6)
   --
   -- Only for Iir_Kind_Constant_Declaration:
   --   Get/Set_Deferred_Declaration_Flag (Flag1)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Variable_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Default_Value (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --  True if the variable is a shared variable.
   --   Get/Set_Shared_Flag (Flag2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_File_Declaration (Medium)
   --
   --  LRM08 6.4.2.5 File declarations
   --
   --  file_declaration ::=
   --     FILE identifier_list : subtype_indication [ file_open_information ] ;
   --
   --  file_open_information ::=
   --     [ OPEN file_open_kind_expression ] IS file_logical_name
   --
   --  file_logical_name ::= string_expression
   --
   --  LRM87
   --
   --  file_declaration ::=
   --     FILE identifier : subtype_indication IS [ mode ] file_logical_name ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_File_Logical_Name (Field6)
   --
   --  This is not used in vhdl 87.
   --   Get/Set_File_Open_Kind (Field7)
   --
   --  This is used only in vhdl 87.
   --   Get/Set_Mode (Flag13,Flag14,Flag15)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Has_Mode (Flag10)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Element_Declaration (Short)
   --
   --  LRM08 5.3.3 Record types
   --
   --  element_declaration ::=
   --     identifier_list : element_subtype_definition ;
   --
   --  identifier_list ::= identifier { , identifier }
   --
   --  element_subtype_definition ::= subtype_indication
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --  Return the position of the element in the record, starting from 0 for
   --  the first record element, increasing by one for each successive element.
   --   Get/Set_Element_Position (Field4)
   --
   --  The type can be deduced from the subtype indication, but this field is
   --  present for uniformity (and speed).
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Record_Element_Constraint (Short)
   --
   --  Record subtype definition which defines this constraint.
   --   Get/Set_Parent (Field0)
   --
   --  For Owned_Elements_Chain, so that the node has an owner.
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  Return the position of the element in the record, starting from 0 for
   --  the first record element, increasing by one for each successive element.
   --   Get/Set_Element_Position (Field4)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Attribute_Declaration (Short)
   --
   --  LRM08 6.7 Attribute declarations
   --
   --  attribute_declaration ::=
   --     ATTRIBUTE identifier : type_mark ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Type_Mark (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Group_Template_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  List of entity class entry.
   --  To handle `<>', the last element of the list can be an entity_class of
   --  kind tok_box.
   --   Get/Set_Entity_Class_Entry_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Group_Declaration (Short)
   --
   --  The declaration containing this type declaration.
   --   Get/Set_Parent (Field0)
   --
   --  List of constituents.
   --   Get/Set_Group_Constituent_List (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Group_Template_Name (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Psl_Endpoint_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --  Always boolean.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Psl_Declaration (Field6)
   --
   --   Get/Set_PSL_Clock (Field7)
   --
   --   Get/Set_PSL_NFA (Field8)
   --
   --  Number of states in the NFA.
   --   Get/Set_PSL_Nbr_States (Field9)
   --
   --   Get/Set_PSL_Clock_Sensitivity (Field10)
   --
   --  True if at least one of the NFA edge has the EOS flag.
   --   Get/Set_PSL_EOS_Flag (Flag1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Psl_Declaration (Medium)
   --  A psl sequence or property declaration.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Psl_Declaration (Field6)
   --
   --  Valid only for property declaration.
   --   Get/Set_PSL_Clock (Field7)
   --
   --  Valid only for property declaration without parameters.
   --   Get/Set_PSL_NFA (Field8)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Terminal_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subnature_Indication (Field5)
   --
   --   Get/Set_Nature (Field1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Free_Quantity_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Default_Value (Field4)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Spectrum_Quantity_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Magnitude_Expression (Field6)
   --
   --   Get/Set_Phase_Expression (Field7)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Noise_Quantity_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   --   Get/Set_Power_Expression (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Across_Quantity_Declaration (Medium)
   -- Iir_Kind_Through_Quantity_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Default_Value (Field4)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --  Set only for the first declaration.
   --   Get/Set_Plus_Terminal_Name (Field8)
   --
   --  Set only for the first declaration.
   --   Get/Set_Minus_Terminal_Name (Field9)
   --
   --  Same as Plus_Terminal_Name when defined.
   --   Get/Set_Plus_Terminal (Field10)
   --
   --  Same as Minus_Terminal_Name when defined.
   --   Get/Set_Minus_Terminal (Field11)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Use_Clause (Short)
   --
   --  LRM08 12.4 Use clauses
   --
   --  use_clause ::=
   --     USE selected_name { , selected_name } ;
   --
   --  Location is on 'USE'.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Selected_Name (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Use_Clause_Chain (Field3)

   -- Iir_Kind_PSL_Inherit_Spec (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Name (Field4)
   --
   --   Get/Set_Inherit_Spec_Chain (Field3)
   --
   --   Get/Set_Chain (Field2)

   -- Iir_Kind_Context_Reference (Short)
   --
   --  LRM08 13.4 Context clauses
   --
   --  context_reference ::=
   --     CONTEXT selected_name { , selected_name }
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Selected_Name (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Context_Reference_Chain (Field3)


   -----------------------
   --  type definitions --
   -----------------------

   --  For Iir_Kinds_Type_And_Subtype_Definition:
   --
   --  Type_Declarator:
   --  Points to the type declaration or subtype declaration that has created
   --  this definition. For some types, such as integer and floating point
   --  types, both type and subtype points to the declaration.
   --  However, there are cases where a type definition doesn't point to
   --  a declarator: anonymous subtype created by index constraints, or
   --  anonymous subtype created by an object declaration.
   --  Note: a type definition cannot be anonymous.
   --   Get/Set_Type_Declarator (Field3)
   --
   --  The parent type.
   --  This is the type or subtype which was used to build a subtype.  This
   --  creates a path to the base type.  Only for subtypes.
   --   Get/Set_Parent_Type (Field4)
   --
   --  The staticness of a type, according to LRM93 7.4.1.
   --  Note: These types definition are always locally static:
   --  enumeration, integer, floating.
   --  However, their subtype are not necessary locally static.
   --   Get/Set_Type_Staticness (State1)
   --
   --  The resolved flag of a subtype, according to LRM93 2.4
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --  The signal_type flag of a type definition.
   --  It is true when the type can be used for a signal.
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)

   -- Iir_Kind_Enumeration_Type_Definition (Short)
   --
   --  Return the list of literals.  This list is created when the node is
   --  created.
   --   Get/Set_Enumeration_Literal_List (Field2)
   --
   --  Get the range of the type (This is just an ascending range from the
   --  first literal to the last declared literal).
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Only_Characters_Flag (Flag4)
   --
   --   Get/Set_Is_Character_Type (Flag5)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --  Note: only 8 or 32.
   --   Get/Set_Scalar_Size (Flag6,Flag7)

   -- Iir_Kind_Enumeration_Literal (Short)
   --
   --  Nota: two literals of the same type are equal iff their value is the
   --  same; in other words, there may be several literals with the same
   --  value.
   --
   --  The parent of an enumeration_literal is the same parent as the type
   --  declaration.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --   Get/Set_Return_Type (Alias Field1)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subprogram_Hash (Field4)
   --
   --  The value of an enumeration literal is the position.
   --   Get/Set_Enum_Pos (Field5)
   --
   --   Get/Set_Seen_Flag (Flag1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --  Never set to true, but possible when used as a prefix of an expanded
   --  name in a overloaded subprogram.
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Physical_Type_Definition (Short)
   --
   --  The range_constraint from the type declaration.
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Unit_Chain (Field2)
   --   Get/Set_Primary_Unit (Alias Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Scalar_Size (Flag6,Flag7)

   -- Iir_Kind_Unit_Declaration (Short)
   --
   --  LRM08 5.2.4 Physical types
   --
   --  primary_unit_declaration ::= identifier ;
   --
   --  secondary_unit_declaration ::= identifier = physical_literal ;
   --
   --  physical_literal ::= [ abstract_literal ] /unit/_name
   --
   --  The parent of a physical unit is the same parent as the type
   --  declaration.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  The Physical_Literal is the expression that defines the value of a
   --  unit.  It is evaluated during analysis and thus expressed as a multiple
   --  of the primary unit.  That's true even for the primary unit whose value
   --  is thus 1.
   --   Get/Set_Physical_Literal (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --  Used for time literals, to compute minimal resolution.
   --   Get/Set_Use_Flag (Flag6)

   --  LRM08 5.2 Scalar types
   --
   --  range_constraint ::= RANGE range
   --
   --  range ::=
   --       range_attribute_name
   --     | simple_expression direction simple_expression
   --
   --  direction ::= to | downto

   -- Iir_Kind_Integer_Type_Definition (Short)
   -- Iir_Kind_Floating_Type_Definition (Short)
   --
   --  The range_constraint from the type declaration.
   --   Get/Set_Range_Constraint (Field1)
   --
   --  The type declarator that has created this type.
   --   Get/Set_Type_Declarator (Field3)
   --
   --  Type staticness is always locally.
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --  Note: only 32 or 64.
   --   Get/Set_Scalar_Size (Flag6,Flag7)

   -- Iir_Kind_Array_Type_Definition (Medium)
   --
   --  LRM08 5.3.2 Array types / LRM93 3.2.1
   --
   --  unbounded_array_definition ::=
   --     ARRAY ( index_subtype_definition { , index_subtype_definition } )
   --       OF element_subtype_indication
   --
   --  index_subtype_definition ::= type_mark RANGE <>
   --
   --  This is a list of type marks.
   --   Get/Set_Index_Subtype_Definition_List (Field6)
   --
   --   Get/Set_Element_Subtype_Indication (Field2)
   --
   --  Same as the index_subtype_definition_list.
   --   Get/Set_Index_Subtype_List (Field9)
   --
   --   Get/Set_Element_Subtype (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --  Always false.
   --   Get/Set_Index_Constraint_Flag (Flag4)

   -- Iir_Kind_Record_Type_Definition (Short)
   --
   --  LRM08 5.3.3 Record types / LRM93 3.2.2 Record types
   --
   --  record_type_definition ::=
   --     RECORD
   --        element_declaration
   --        { element_declaration }
   --     END RECORD [ /record_type/_simple_name ]
   --
   --   Get/Set_Elements_Declaration_List (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --  Always false for record type: elements are owned by this node.
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Access_Type_Definition (Short)
   --
   --  LRM08 5.4 Access types
   --
   --  access_type_definition ::= ACCESS subtype_indication
   --
   --  The subtype_indication as it appears.  Can designate an
   --  incomplete_type_definition.
   --   Get/Set_Designated_Subtype_Indication (Field5)
   --
   --  The resolved designated type.
   --   Get/Set_Designated_Type (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --  Next access type that also referenced the same incomplete type when
   --  defined.
   --   Get/Set_Incomplete_Type_Ref_Chain (Field0)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_File_Type_Definition (Short)
   --
   --   Get/Set_File_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --  True if this is the std.textio.text file type, which may require special
   --  handling.
   --   Get/Set_Text_File_Flag (Flag4)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Incomplete_Type_Definition (Short)
   --  Type definition for an incomplete type.  This is created during the
   --  analysis of the incomplete type declaration.
   --
   --  Chain of access_type_definition that designated this type.  This is
   --  simply a forward_ref as the access type is declared after the
   --  incomplete type.
   --   Get/Set_Incomplete_Type_Ref_Chain (Field0)
   --
   --  Set to the incomplete type declaration.
   --   Get/Set_Type_Declarator (Field3)
   --
   --  Set to the complete type definition when completed.
   --   Get/Set_Complete_Type_Definition (Field5)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)

   -- Iir_Kind_Interface_Type_Definition (Short)
   --  Type definition for an interface type.
   --
   --  Set to interface type declaration.
   --   Get/Set_Type_Declarator (Field3)
   --
   --  Set only during analysis of association: type associated with this
   --  interface, so that references to this interface can use the actual
   --  type.
   --   Get/Set_Associated_Type (Field5)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)

   -- Iir_Kind_Protected_Type_Declaration (Short)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Protected_Type_Body (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Protected_Type_Body (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Protected_Type_Declaration (Field4)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Wildcard_Type_Definition (Short)
   --  A wildcard type doesn't correspond to a type defined by VHDL.  It
   --  is used only during analysis to temporary set the type of an entity
   --  when the type is not precisely known but restricted to some class of
   --  types.  Eg: the type of an aggregate is not known before being
   --  determined by the context, but can only be an array or a record.
   --  Wildcard types are statically created by std_package and the set of
   --  restrictions depends on the node.  See std_package.ads
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Foreign_Vector_Type_Definition (Medium)
   --
   --  A one dimensional array representing a vector defined in a foreign
   --  language.
   --
   --  FIXME: add constraint state, add length
   --
   --   Get/Set_Type_Declarator (Field3)

   --------------------------
   --  subtype definitions --
   --------------------------

   --  LRM08 6.3 Subtype declarations
   --
   --  subtype_indication ::=
   --    [ resolution_indication ] type_mark [ constraint ]
   --
   --  There is no unique representation for a subtype indication.  If there is
   --  only a type_mark, then a subtype indication is represented by a name
   --  (a simple name or an expanded name); otherwise it is represented by one
   --  of the subtype definition node.
   --
   --  resolution_indication ::=
   --     resolution_function_name | ( element_resolution )
   --
   --  element_resolution ::= array_element_resolution | record_resolution
   --
   --  If there is no constraint but a resolution function name, the subtype
   --  indication is represented by a subtype_definition (which will be
   --  replaced by the correct subtype definition).  If there is an array
   --  element resolution the subtype indication is represented by an array
   --  subtype definition, and if there is a record resolution, it is
   --  represented by a record subtype definition.
   --
   --  constraint ::=
   --     range_constraint
   --   | index_constraint
   --   | array_constraint
   --   | record_constraint
   --
   --  There is no node for constraint, it is directly represented by one of
   --  the rhs.
   --
   --  element_constraint ::=
   --     array_constraint
   --   | record_constraint
   --
   --  Likewise, there is no node for element_constraint.
   --
   --  index_constraint ::= ( discrete_range { , discrete_range } )
   --
   --  An index_constraint is represented by an array_subtype_definition.
   --
   --  discrete_range ::= /discrete/_subtype_indication | range
   --
   --  array_constraint ::=
   --     index_constraint [ array_element_constraint ]
   --   | ( OPEN ) [ array_element_constraint ]
   --
   --  An array_constraint is also represented by an array_subtype_definition.
   --
   --  array_element_constraint ::= element_constraint
   --
   --  There is no node for array_element_constraint.
   --
   --  record_constraint ::=
   --     ( record_element_constraint { , record_element_constraint } )
   --
   --  A record_constraint is represented by a record_subtype_definition.
   --
   --  record_element_constraint ::=
   --     record_element_simple_name element_constraint
   --
   --  Represented by Record_Element_Constraint.

   -- Iir_Kind_Enumeration_Subtype_Definition (Short)
   -- Iir_Kind_Integer_Subtype_Definition (Short)
   -- Iir_Kind_Physical_Subtype_Definition (Short)
   --
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Subtype_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Parent_Type (Field4)
   --
   --   Get/Set_Resolution_Indication (Field5)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Floating_Subtype_Definition (Medium)
   --
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Subtype_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Parent_Type (Field4)
   --
   --   Get/Set_Resolution_Indication (Field5)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Access_Subtype_Definition (Short)
   --
   --   Get/Set_Designated_Type (Field1)
   --
   --   Get/Set_Subtype_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Parent_Type (Field4)
   --
   --   Get/Set_Designated_Subtype_Indication (Field5)
   --
   --  Note: no resolution function for access subtype.
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)

   -- Iir_Kind_Array_Element_Resolution (Short)
   --
   --  LRM08 6.3 Subtype declarations
   --
   --  array_element_resolution ::= resolution_indication
   --
   --  The indication as it appears in the sources.
   --   Get/Set_Resolution_Indication (Field5)
   --
   --  The subtype definition of the element.  Owner of it.
   --   Get/Set_Element_Subtype_Indication (Field2)

   -- Iir_Kind_Record_Resolution (Short)
   --
   --  LRM08 6.3 Subtype declarations
   --
   --  record_resolution ::=
   --     record_element_resolution { , record_element_resolution }
   --
   --   Get/Set_Record_Element_Resolution_Chain (Field1)

   -- Iir_Kind_Record_Element_Resolution (Short)
   --
   --  LRM08 6.3 Subtype declarations
   --
   --  record_element_resolution ::=
   --     /record_element/_simple_name resolution_indication
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Resolution_Indication (Field5)

   -- Iir_Kind_Record_Subtype_Definition (Medium)
   --
   --  Chain of new elements constraint.  Needed only for internal consistency
   --  of the tree (ownership).
   --   Get/Set_Owned_Elements_Chain (Field6)
   --
   --  Chain of either element_declaration or record_element_constraint.
   --   Get/Set_Elements_Declaration_List (Field1)
   --
   --   Get/Set_Subtype_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Parent_Type (Field4)
   --
   --   Get/Set_Resolution_Indication (Field5)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --  Always true for record subtype: elements are owned through
   --  Owned_Elements_Chain
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Array_Subtype_Definition (Medium)
   --
   --   Get/Set_Subtype_Type_Mark (Field2)
   --
   --   Get/Set_Resolution_Indication (Field5)
   --
   --  The index_constraint list as it appears in the subtype indication (if
   --  present). This is a list of subtype indication.  Owned by this node.
   --   Get/Set_Index_Constraint_List (Field6)
   --
   --  The type of the index.  This is either the index_constraint list or the
   --  index subtypes of the type_mark.  Not owned by this node.
   --   Get/Set_Index_Subtype_List (Field9)
   --
   --  Set when the element is re-constrained.
   --  Note that the element subtype may be different from the parent also if
   --  it is resolved.  This is mostly for ownership.
   --   Get/Set_Array_Element_Constraint (Field8)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --   Get/Set_Element_Subtype (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Parent_Type (Field4)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --  True if the indexes are constrained (either by this definition or by
   --  a parent).
   --   Get/Set_Index_Constraint_Flag (Flag4)
   --
   --  If True, an array constraint is lexically present (at least for the
   --  index).  This can be an index constraint or 'open'.
   --   Get/Set_Has_Array_Constraint_Flag (Flag5)
   --
   --  If True, an element constraint is lexically present.  It can be 'open'.
   --   Get/Set_Has_Element_Constraint_Flag (Flag6)

   -- Iir_Kind_Range_Expression (Short)
   --
   --  There are two fields for both limits: those that own the node
   --  (Left_Limit_Expr and Right_Limit_Expr) and those that reference the node
   --  (Left_Limit and Right_Limit).  Always use the reference (they cannot be
   --  Null_Iir, while the owner nodes can be Null_Iir.  Set the owner nodes
   --  only for owning purpose.
   --   Get/Set_Left_Limit_Expr (Field2)
   --
   --   Get/Set_Right_Limit_Expr (Field3)
   --
   --   Get/Set_Range_Origin (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Left_Limit (Field4)
   --
   --   Get/Set_Right_Limit (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Direction (Flag1)

   -- Iir_Kind_Subtype_Definition (Medium)
   --  Such a node is only created by parse and transformed into the correct
   --  kind (enumeration_subtype, integer_subtype...) by sem.
   --
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Subtype_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Parent_Type (Field4)
   --
   --   Get/Set_Resolution_Indication (Field5)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --   Get/Set_Is_Ref (Flag12)

   -------------------------
   --  Nature definitions --
   -------------------------

   -- Iir_Kind_Scalar_Nature_Definition (Medium)
   --
   --   Get/Set_Reference (Field2)
   --
   --  The declarator that has created this nature type.
   --   Get/Set_Nature_Declarator (Field3)
   --
   --   Get/Set_Base_Nature (Field4)
   --
   --   Get/Set_Across_Type_Mark (Field9)
   --
   --   Get/Set_Through_Type_Mark (Field10)
   --
   --   Get/Set_Across_Type (Field11)
   --
   --   Get/Set_Through_Type (Field12)
   --
   --   Get/Set_Nature_Staticness (State1)

   -- Iir_Kind_Array_Nature_Definition (Medium)
   --
   --  AMS-LRM17 5.8.3.2 Array Natures
   --
   --  This is a list of type marks.
   --   Get/Set_Index_Subtype_Definition_List (Field6)
   --
   --   Get/Set_Element_Subnature_Indication (Field2)
   --
   --  Same as the index_subtype_definition_list.
   --   Get/Set_Index_Subtype_List (Field9)
   --
   --   Get/Set_Element_Subnature (Field1)
   --
   --   Get/Set_Nature_Declarator (Field3)
   --
   --   Get/Set_Base_Nature (Field4)
   --
   --   Get/Set_Simple_Nature (Field7)
   --
   --   Get/Set_Nature_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --  Always false.
   --   Get/Set_Index_Constraint_Flag (Flag4)
   --
   --   Get/Set_Across_Type_Definition (Field10)
   --
   --   Get/Set_Through_Type_Definition (Field5)
   --
   --   Get/Set_Across_Type (Field11)
   --
   --   Get/Set_Through_Type (Field12)

   -- Iir_Kind_Array_Subnature_Definition (Medium)
   --
   --   Get/Set_Subnature_Nature_Mark (Field2)
   --
   --  The index_constraint list as it appears in the subtype indication (if
   --  present). This is a list of subtype indication.
   --   Get/Set_Index_Constraint_List (Field6)
   --
   --  The type of the index.  This is either the index_constraint list or the
   --  index subtypes of the type_mark.
   --   Get/Set_Index_Subtype_List (Field9)
   --
   --   Get/Set_Array_Element_Constraint (Field8)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --   Get/Set_Element_Subnature (Field1)
   --
   --   Get/Set_Nature_Declarator (Field3)
   --
   --   Get/Set_Base_Nature (Field4)
   --
   --   Get/Set_Nature_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --   Get/Set_Index_Constraint_Flag (Flag4)
   --
   --   Get/Set_Across_Type_Definition (Field10)
   --
   --   Get/Set_Through_Type_Definition (Field5)
   --
   --   Get/Set_Across_Type (Field11)
   --
   --   Get/Set_Through_Type (Field12)

   -- Iir_Kind_Record_Nature_Definition (Medium)
   --
   --  AMS-LRM17 5.8.3.3 Record natures
   --  record_nature_definition ::=
   --     RECORD
   --        nature_element_declaration
   --        { nature_element_declaration }
   --     END RECORD [ /record_nature/_simple_name ]
   --
   --   Get/Set_Elements_Declaration_List (Field1)
   --
   --   Get/Set_Nature_Declarator (Field3)
   --
   --   Get/Set_Base_Nature (Field4)
   --
   --   Get/Set_Across_Type_Definition (Field10)
   --
   --   Get/Set_Through_Type_Definition (Field5)
   --
   --   Get/Set_Across_Type (Field11)
   --
   --   Get/Set_Through_Type (Field12)
   --
   --   Get/Set_Simple_Nature (Field7)
   --
   --   Get/Set_Nature_Staticness (State1)
   --
   --   Get/Set_Constraint_State (State2)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --  Always false for record type: elements are owned by this node.
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Nature_Element_Declaration (Short)
   --
   --  AMS-LRM17 5.8.3.3 Record natures
   --
   --  nature_element_declaration ::=
   --     identifier_list : element_subnature_definition ;
   --
   --  element_subnature_definition ::= subnature_indication
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subnature_Indication (Field5)
   --
   --   Get/Set_Element_Position (Field4)
   --
   --   Get/Set_Nature (Field1)
   --
   --   Get/Set_Has_Identifier_List (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)


   ----------------------------
   --  concurrent statements --
   ----------------------------

   -- Iir_Kind_Concurrent_Conditional_Signal_Assignment (Medium)
   -- Iir_Kind_Concurrent_Selected_Signal_Assignment (Medium)
   -- Iir_Kind_Concurrent_Simple_Signal_Assignment (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Target (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Reject_Time_Expression (Field4)
   --
   -- Only for Iir_Kind_Concurrent_Simple_Signal_Assignment:
   --   Get/Set_Waveform_Chain (Field5)
   --
   -- Only for Iir_Kind_Concurrent_Selected_Signal_Assignment:
   --   Get/Set_Expression (Field5)
   --
   -- Only for Iir_Kind_Concurrent_Conditional_Signal_Assignment:
   --   Get/Set_Conditional_Waveform_Chain (Field5)
   --
   -- Only for Iir_Kind_Concurrent_Selected_Signal_Assignment:
   --   Get/Set_Selected_Waveform_Chain (Field7)
   --
   --  If the assignment is guarded, then get_guard must return the
   --  declaration of the signal guard, otherwise, null_iir.
   --  If the guard signal decl is not known, as a kludge and only to mark this
   --  assignment guarded, the guard can be this assignment.
   --   Get/Set_Guard (Field8)
   --
   --   Get/Set_Delay_Mechanism (Flag1)
   --
   --   Get/Set_Has_Delay_Mechanism (Flag2)
   --
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --  True if the target of the assignment is guarded
   --   Get/Set_Guarded_Target_State (State1)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Sensitized_Process_Statement (Medium)
   -- Iir_Kind_Process_Statement (Medium)
   --
   --  Location is on the label, or 'postponed' or 'process'.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --  The concurrent statement at the origin of that process.  This is
   --  Null_Iir for a user process.
   --   Get/Set_Process_Origin (Field8)
   --
   --   Get/Set_Sequential_Statement_Chain (Field4)
   --
   --  Sensitivity list.  Must be after the statements, because signal aliases
   --  may be referenced (in case of implicit process).
   -- Only for Iir_Kind_Sensitized_Process_Statement:
   --   Get/Set_Sensitivity_List (Field6)
   --
   --   Get/Set_Callees_List (Field7)
   --
   --   Get/Set_Wait_State (State1)
   --
   --   Get/Set_Seen_Flag (Flag1)
   --
   --   Get/Set_Passive_Flag (Flag2)
   --
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Has_Label (Flag6)
   --
   --   Get/Set_Has_Is (Flag7)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_End_Has_Postponed (Flag10)
   --
   -- Only for Iir_Kind_Process_Statement:
   --   Get/Set_Suspend_Flag (Flag11)
   --
   -- Only for Iir_Kind_Sensitized_Process_Statement:
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Concurrent_Assertion_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Assertion_Condition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Severity_Expression (Field4)
   --
   --   Get/Set_Report_Expression (Field5)
   --
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Psl_Default_Clock (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Psl_Boolean (Field1)
   --
   --   Get/Set_Chain (Field2)

   -- Iir_Kind_Psl_Assert_Directive (Medium)
   -- Iir_Kind_Psl_Assume_Directive (Medium)
   -- Iir_Kind_Psl_Cover_Directive (Medium)
   -- Iir_Kind_Psl_Restrict_Directive (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_Psl_Assert_Directive:
   -- Only for Iir_Kind_Psl_Assume_Directive:
   --   Get/Set_Psl_Property (Field1)
   --
   -- Only for Iir_Kind_Psl_Cover_Directive:
   -- Only for Iir_Kind_Psl_Restrict_Directive:
   --   Get/Set_Psl_Sequence (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   -- Only for Iir_Kind_Psl_Assert_Directive:
   --   Get/Set_Severity_Expression (Field4)
   --
   -- Only for Iir_Kind_Psl_Assert_Directive:
   -- Only for Iir_Kind_Psl_Cover_Directive:
   --   Get/Set_Report_Expression (Field5)
   --
   --  The following fields are set by canon.
   --   Get/Set_PSL_Clock (Field7)
   --
   --   Get/Set_PSL_NFA (Field8)
   --
   --  Number of states in the NFA.
   --   Get/Set_PSL_Nbr_States (Field9)
   --
   --   Get/Set_PSL_Clock_Sensitivity (Field10)
   --
   --  True if at least one of the NFA edge has the EOS flag.
   --   Get/Set_PSL_EOS_Flag (Flag1)
   --
   --  True if there is an outer abort is present (but not in the NFA)
   -- Only for Iir_Kind_Psl_Assert_Directive:
   -- Only for Iir_Kind_Psl_Assume_Directive:
   --   Get/Set_PSL_Abort_Flag (Flag2)
   --
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Component_Instantiation_Statement (Medium)
   --
   --  LRM08 11.7 Component instantiation statements
   --
   --  component_instantiation_statement ::=
   --     instantiation_label :
   --        instantiated_unit
   --           [ generic_map_aspect ]
   --           [ port_map_aspect ] ;
   --
   --  instantiated_unit ::=
   --       [ COMPONENT ] component_name
   --     | ENTITY entity_name [ ( architecture_identifier ) ]
   --     | CONFIGURATION configuration_name
   --
   --   Get/Set_Parent (Field0)
   --
   --  Unit instantiated.  This is a name, an entity_aspect_entity or an
   --  entity_aspect_configuration.
   --   Get/Set_Instantiated_Unit (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Default_Binding_Indication (Field5)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Port_Map_Aspect_Chain (Field9)
   --
   --  Configuration:
   --  In case of a configuration specification, the node is put into
   --  default configuration.  In the absence of a specification, the
   --  default entity aspect, if any; if none, this field is null_iir.
   --   Get/Set_Configuration_Specification (Field7)
   --
   --  During Sem and elaboration, the configuration field can be filled by
   --  a component configuration declaration.
   --
   --  Configuration for this component.
   --  FIXME: must be get/set_binding_indication.
   --   Get/Set_Component_Configuration (Field6)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Has_Component (Flag5)

   -- Iir_Kind_Block_Statement (Medium)
   --  LRM08 11.2 Block statement
   --
   --  block_statement ::=
   --    block_label :
   --      BLOCK [ ( guard_condition ) ] [ IS ]
   --        block_header
   --        block_declarative_part
   --      BEGIN
   --        block_statement_part
   --      END BLOCK [ block_label ] ;
   --
   --   Get/Set_Parent (Field0)
   --
   --  get/set_guard_decl is used for semantic analysis, in order to add
   --  a signal declaration.
   --   Get/Set_Guard_Decl (Field8)
   --
   --   Get/Set_Block_Header (Field7)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field4)
   --
   --   Get/Set_Block_Block_Configuration (Field6)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Has_Is (Flag7)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Generate_Statement_Body (Short)
   --  LRM08 11.8 Generate statements
   --
   --  generate_statement_body ::=
   --        [ block_declarative_part
   --     BEGIN ]
   --        { concurrent_statement }
   --     [ END [ alternative_label ] ; ]
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --  The block configuration for this statement body.
   --   Get/Set_Generate_Block_Configuration (Field2)
   --
   --   Get/Set_Alternative_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Has_Label (Flag6)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Has_Begin (Flag10)
   --
   --   Get/Set_Has_End (Flag11)

   -- Iir_Kind_For_Generate_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  The parameters specification is represented by an Iterator_Declaration.
   --   Get/Set_Parameter_Specification (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Generate_Statement_Body (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_If_Generate_Else_Clause (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  Null_Iir for the else clause.
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Generate_Statement_Body (Field4)
   --
   --   Get/Set_Generate_Else_Clause (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_If_Generate_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  Null_Iir for the else clause.
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Generate_Statement_Body (Field4)
   --
   --   Get/Set_Generate_Else_Clause (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Case_Generate_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  Chain is composed of Iir_Kind_Choice_By_XXX.
   --   Get/Set_Case_Statement_Alternative_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Simple_Simultaneous_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Simultaneous_Left (Field5)
   --
   --   Get/Set_Simultaneous_Right (Field6)
   --
   --   Get/Set_Tolerance (Field7)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Simultaneous_Null_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Simultaneous_Procedural_Statement (Short)
   --
   --  AMS-LRM17 11.13 Simultaneous procedural statement
   --  simultaneous_procedural_statement ::=
   --    [ procedural_label : ]
   --      PROCEDURAL [ IS ]
   --        procedural_declarative_part
   --      BEGIN
   --        procedural_statement_part
   --      END PROCEDURAL [ procedural_label ] ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Sequential_Statement_Chain (Field4)
   --
   --   Get/Set_Attribute_Value_Chain (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Has_Is (Flag7)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Simultaneous_If_Statement (Short)
   -- Iir_Kind_Simultaneous_Elsif (Short)
   --
   --  AMS-LRM17 11.11 Simultaneous if statement
   --  simultaneous_if_statement ::=
   --    [ /if/_label : ]
   --      IF condition USE
   --        simultaneous_statement_part
   --      { ELSIF condition USE
   --        simultaneous_statement_part }
   --      [ ELSE
   --        simultaneous_statement_part ]
   --      END USE [ /if/_label ];
   --
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_Simultaneous_If_Statement:
   --   Get/Set_Label (Field3)
   --
   -- Only for Iir_Kind_Simultaneous_If_Statement:
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Simultaneous_Statement_Chain (Field4)
   --
   --   Get/Set_Else_Clause (Field5)
   --
   -- Only for Iir_Kind_Simultaneous_If_Statement:
   --   Get/Set_Chain (Field2)
   --
   -- Only for Iir_Kind_Simultaneous_If_Statement:
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   -- Iir_Kind_Simultaneous_Case_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  Chain is composed of Iir_Kind_Choice_By_XXX.
   --   Get/Set_Case_Statement_Alternative_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_End_Has_Reserved_Id (Flag8)
   --
   --   Get/Set_End_Has_Identifier (Flag9)

   ----------------------------
   --  sequential statements --
   ----------------------------

   -- Iir_Kind_If_Statement (Short)
   -- Iir_Kind_Elsif (Short)
   --
   --  LRM08 10.8
   --  if_statement ::=
   --    [ /if/_label : ]
   --       IF condition THEN
   --          sequence_of_statements
   --       { ELSIF condition THEN
   --          sequence_of_statements }
   --       [ ELSE
   --          sequence_of_statements ]
   --       END IF [ /if/_label ] ;
   --
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Label (Field3)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Identifier (Alias Field3)
   --
   --  May be NULL only for an iir_kind_elsif node, and then means the else
   --  clause.
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Sequential_Statement_Chain (Field4)
   --
   --  Must be an Iir_kind_elsif node, or NULL for no more elsif clauses.
   --   Get/Set_Else_Clause (Field5)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Chain (Field2)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Suspend_Flag (Flag11)
   --
   --   Get/Set_Is_Ref (Flag12)

   --  LRM08 10.10 Loop statement / LRM93 8.9
   --
   --  loop_statement ::=
   --     [ loop_label : ]
   --        [ iteration_scheme ] LOOP
   --           sequence_of_statements
   --        END LOOP [ loop_label ] ;
   --
   --  iteration_scheme ::=
   --       WHILE condition
   --     | FOR loop_parameter_specification
   --
   --  parameter_specification ::=
   --     identifier IN discrete_range

   -- Iir_Kind_For_Loop_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  The parameters specification is represented by an Iterator_Declaration.
   --   Get/Set_Parameter_Specification (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Sequential_Statement_Chain (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Exit_Flag (Flag1)
   --
   --   Get/Set_Next_Flag (Flag2)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Suspend_Flag (Flag11)

   -- Iir_Kind_While_Loop_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Sequential_Statement_Chain (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Exit_Flag (Flag1)
   --
   --   Get/Set_Next_Flag (Flag2)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Suspend_Flag (Flag11)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Exit_Statement (Short)
   -- Iir_Kind_Next_Statement (Short)
   --
   --  LRM08 10.11 Next statement
   --
   --  next_statement ::=
   --     [ label : ] NEXT [ loop_label ] [ WHEN condition ] ;
   --
   --  LRM08 10.12 Exit statement
   --
   --  exit_statement ::=
   --     [ label : ] exit [ loop_label ] [ when condition ] ;
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Loop_Label (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Signal_Force_Assignment_Statement (Short)
   -- Iir_Kind_Signal_Release_Assignment_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Target (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   -- Only for Iir_Kind_Signal_Force_Assignment_Statement:
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Force_Mode (Flag1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --  True if the target of the assignment is guarded
   --   Get/Set_Guarded_Target_State (State1)
   --
   --   Get/Set_Is_Ref (Flag12)
   --
   --   Get/Set_Has_Force_Mode (Flag2)

   -- Iir_Kind_Simple_Signal_Assignment_Statement (Short)
   -- Iir_Kind_Conditional_Signal_Assignment_Statement (Short)
   -- Iir_Kind_Selected_Waveform_Assignment_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Target (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Reject_Time_Expression (Field4)
   --
   -- Only for Iir_Kind_Simple_Signal_Assignment_Statement:
   --  The waveform.
   --  If the waveform_chain is null_iir, then the signal assignment is a
   --  disconnection statement, ie TARGET <= null_iir after disconection_time,
   --  where disconnection_time is specified by a disconnection specification.
   --   Get/Set_Waveform_Chain (Field5)
   --
   -- Only for Iir_Kind_Conditional_Signal_Assignment_Statement:
   --   Get/Set_Conditional_Waveform_Chain (Field5)
   --
   -- Only for Iir_Kind_Selected_Waveform_Assignment_Statement:
   --   Get/Set_Expression (Field5)
   --
   -- Only for Iir_Kind_Selected_Waveform_Assignment_Statement:
   --   Get/Set_Selected_Waveform_Chain (Field7)
   --
   --   Get/Set_Delay_Mechanism (Flag1)
   --
   --   Get/Set_Has_Delay_Mechanism (Flag2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --  True if the target of the assignment is guarded
   --   Get/Set_Guarded_Target_State (State1)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Variable_Assignment_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Target (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Conditional_Variable_Assignment_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Target (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --  Chain of conditional_expressions.
   --   Get/Set_Conditional_Expression_Chain (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Assertion_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Assertion_Condition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Severity_Expression (Field4)
   --
   --   Get/Set_Report_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Report_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Severity_Expression (Field4)
   --
   --   Get/Set_Report_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Wait_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Timeout_Clause (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Condition_Clause (Field5)
   --
   --   Get/Set_Sensitivity_List (Field6)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Return_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --  Type of the return value of the function.  This is a copy of
   --  return_type.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Case_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Matching_Flag (Flag1)
   --
   --  Chain is composed of Iir_Kind_Choice_By_XXX.
   --   Get/Set_Case_Statement_Alternative_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_End_Has_Identifier (Flag9)
   --
   --   Get/Set_Suspend_Flag (Flag11)

   -- Iir_Kind_Procedure_Call_Statement (Short)
   -- Iir_Kind_Concurrent_Procedure_Call_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Procedure_Call (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   -- Only for Iir_Kind_Concurrent_Procedure_Call_Statement:
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Suspend_Flag (Flag11)

   -- Iir_Kind_Procedure_Call (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Parameter_Association_Chain (Field2)
   --
   --  Procedure declaration corresponding to the procedure to call.
   --   Get/Set_Implementation (Field3)
   --
   --   Get/Set_Method_Object (Field4)

   -- Iir_Kind_Null_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Break_Statement (Short)
   -- Iir_Kind_Concurrent_Break_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Condition (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   -- Only for Iir_Kind_Concurrent_Break_Statement:
   --   Get/Set_Sensitivity_List (Field6)
   --
   --   Get/Set_Break_Element (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Break_Element (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Selector_Quantity (Field3)
   --
   --   Get/Set_Break_Quantity (Field4)
   --
   --   Get/Set_Expression (Field5)

   -- Iir_Kind_Suspend_State_Statement (Short)
   --
   --  Implicit statement added to mark a suspend point.
   --
   --   Get/Set_Parent (Field0)
   --
   --  Next statement
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Suspend_State_Index (Field3)
   --
   --   Get/Set_Suspend_State_Chain (Field4)

   ----------------
   --  operators --
   ----------------

   -- Iir_Kinds_Monadic_Operator (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Operand (Field2)
   --   Get/Set_Left (Alias Field2)
   --
   --  Function declaration corresponding to the function to call.
   --   Get/Set_Implementation (Field3)
   --
   --  Expr_staticness is defined by LRM93 7.4
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kinds_Dyadic_Operator (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --  Left and Right operands.
   --   Get/Set_Left (Field2)
   --
   --  Function declaration corresponding to the function to call.
   --   Get/Set_Implementation (Field3)
   --
   --   Get/Set_Right (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Function_Call (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Parameter_Association_Chain (Field2)
   --
   --  Function declaration corresponding to the function to call.
   --   Get/Set_Implementation (Field3)
   --
   --   Get/Set_Method_Object (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Aggregate (Short)
   --
   --   Get/Set_Association_Choices_Chain (Field4)
   --
   --  Same as Type, but marked as property of that node.
   --   Get/Set_Literal_Subtype (Field3)
   --
   --  Exist for symmetry with other literals, but must never be set.  The
   --  content of the aggregate is modified during evaluation, not the
   --  aggregate itself.
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Aggregate_Info (Field5)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --  If true, the aggregate can be statically built.  This is an optimization
   --  and the conditions are defined in sem_expr.
   --   Get/Set_Aggregate_Expand_Flag (Flag1)

   -- Iir_Kind_Aggregate_Info (Short)
   --
   --  Get info for the next dimension.  NULL_IIR terminated.
   --   Get/Set_Sub_Aggregate_Info (Field1)
   --
   --  For array aggregate only:
   --  If TRUE, the choices are not locally static.
   --  This flag is only valid when the array aggregate is constrained, ie
   --  has no 'others' choice.
   --   Get/Set_Aggr_Dynamic_Flag (Flag3)
   --
   --  If TRUE, the aggregate is named, else it is positional.
   --   Get/Set_Aggr_Named_Flag (Flag4)
   --
   --  The following three fields are used to check bounds of an array
   --  aggregate.
   --  For named aggregate, low and high bounds are computed, for positional
   --  aggregate, the (minimum) number of elements is computed.
   --  Note there may be elements beyond the bounds, due to other choice.
   --  These fields may apply for the aggregate or for the aggregate and its
   --  brothers if the node is for a sub-aggregate.
   --
   --  The low and high index choice, if any.
   --   Get/Set_Aggr_Low_Limit (Field2)
   --
   --   Get/Set_Aggr_High_Limit (Field3)
   --
   --  The minimum number of elements, if any.  This is a minimax.
   --   Get/Set_Aggr_Min_Length (Field4)
   --
   --  True if the choice list has an 'others' choice.
   --   Get/Set_Aggr_Others_Flag (Flag2)

   -- Iir_Kind_Parenthesis_Expression (Short)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Qualified_Expression (Short)
   --
   --  LRM08 9.3.5 Qualified expressions
   --
   --  qualified_expression ::=
   --       type_mark ' ( expression )
   --     | type_mark ' aggregate
   --
   --   Get/Set_Type_Mark (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Type_Conversion (Short)
   --
   --  LRM08 9.3.6 Type conversions
   --
   --  type_conversion ::= type_mark ( expression )
   --
   --   Get/Set_Type (Field1)
   --
   --  If the type mark denotes an unconstrained array and the expression is
   --  locally static, the result should be locally static according to vhdl93
   --  (which is not clear on that point).  As a subtype is created, it is
   --  referenced by this field.
   --   Get/Set_Type_Conversion_Subtype (Field3)
   --
   --   Get/Set_Type_Mark (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Allocator_By_Expression (Short)
   -- Iir_Kind_Allocator_By_Subtype (Short)
   --
   --  LRM08 9.3.7 Allocators
   --
   --  allocator ::=
   --      NEW subtype_indication
   --    | NEW qualified_expression
   --
   --   Get/Set_Type (Field1)
   --
   -- Only for Iir_Kind_Allocator_By_Expression:
   --  Contains the expression for a by expression allocator.
   --   Get/Set_Expression (Field5)
   --
   -- Only for Iir_Kind_Allocator_By_Subtype:
   --  Contains the subtype indication for a by subtype allocator.
   --   Get/Set_Subtype_Indication (Field5)
   --
   -- Only for Iir_Kind_Allocator_By_Subtype:
   --  Same as subtype indication but set when the allocator defines a new
   --  subtype.  Used to track when an anonymous subtype is created.
   --   Get/Set_Allocator_Subtype (Field3)
   --
   --  To ease analysis: set to the designated type (either the type of the
   --  expression or the subtype)
   --   Get/Set_Allocator_Designated_Type (Field2)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Is_Ref (Flag12)

   ------------
   --  Names --
   ------------

   -- Iir_Kind_Simple_Name (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Character_Literal (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Operator_Symbol (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)

   -- Iir_Kind_Reference_Name (Short)
   --
   --  This doesn't correspond to a name in the sources.  This is an artificial
   --  name in the tree which is owned and reference another name.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --  The name from which the reference was created.  Can be Null_Iir if the
   --  reference was created directly from a declaration.
   --   Get/Set_Referenced_Name (Field2)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Selected_Name (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_External_Constant_Name (Short)
   -- Iir_Kind_External_Signal_Name (Short)
   -- Iir_Kind_External_Variable_Name (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_External_Pathname (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Subtype_Indication (Field5)
   --
   -- Only for Iir_Kind_External_Variable_Name:
   --   Get/Set_Shared_Flag (Flag2)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Is_Ref (Flag12)

   -- Iir_Kind_Selected_By_All_Name (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Indexed_Name (Short)
   --  Select the element designed with the INDEX_LIST from array PREFIX.
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Index_List (Field2)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Slice_Name (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Suffix (Field2)
   --
   --   Get/Set_Slice_Subtype (Field3)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Parenthesis_Name (Short)
   --  Created by the parser, and mutated into the correct iir node: it can be
   --  either a function call, an indexed array, a type conversion or a slice
   --  name.
   --
   --   Get/Set_Prefix (Field0)
   --
   --  Always returns null_iir.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Association_Chain (Field2)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)

   -- Iir_Kind_Selected_Element (Short)
   --  A record element selection.  This corresponds to a refined selected
   --  names.  The production doesn't exist in the VHDL grammar.
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --  The selected element.
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --  Always false.
   --   Get/Set_Is_Forward_Ref (Flag1)

   -- Iir_Kind_Implicit_Dereference (Short)
   -- Iir_Kind_Dereference (Short)
   --  An implicit access dereference.
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Package_Pathname (Short)
   --  This node represents only the library_logical_name. Package and object
   --  simple_names are represented by Selected_Name.
   --
   --   Get/Set_Pathname_Suffix (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)

   -- Iir_Kind_Absolute_Pathname (Short)
   --  Represents only the '.'.
   --
   --   Get/Set_Pathname_Suffix (Field2)

   -- Iir_Kind_Relative_Pathname (Short)
   --  Represents only one '^.'
   --
   --   Get/Set_Pathname_Suffix (Field2)

   -- Iir_Kind_Pathname_Element (Short)
   --
   --   Get/Set_Pathname_Suffix (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Pathname_Expression (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)

   -----------------
   --  Attributes --
   -----------------

   -- Iir_Kind_Attribute_Name (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Attribute_Signature (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Is_Forward_Ref (Flag1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Base_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)

   -- Iir_Kind_Across_Attribute (Short)
   -- Iir_Kind_Through_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Nature_Reference_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Nature (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Above_Attribute (Short)
   -- Iir_Kind_Dot_Attribute (Short)
   -- Iir_Kind_Integ_Attribute (Short)
   -- Iir_Kind_Quantity_Delayed_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Attr_Chain (Field2)
   --
   --  Head of the chain.  Used only to ease the reconstruction of the chain.
   --   Get/Set_Signal_Attribute_Declaration (Field3)
   --
   -- Only for Iir_Kind_Above_Attribute:
   -- Only for Iir_Kind_Quantity_Delayed_Attribute:
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Ramp_Attribute (Medium)
   -- Iir_Kind_Signal_Slew_Attribute (Medium)
   -- Iir_Kind_Quantity_Slew_Attribute (Medium)
   -- Iir_Kind_Zoh_Attribute (Medium)
   -- Iir_Kind_Ltf_Attribute (Medium)
   -- Iir_Kind_Ztf_Attribute (Medium)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Attr_Chain (Field2)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Parameter_2 (Field6)
   --
   -- Only for Iir_Kind_Ztf_Attribute:
   --   Get/Set_Parameter_3 (Field7)
   --
   -- Only for Iir_Kind_Ztf_Attribute:
   --   Get/Set_Parameter_4 (Field8)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Left_Type_Attribute (Short)
   -- Iir_Kind_Right_Type_Attribute (Short)
   -- Iir_Kind_High_Type_Attribute (Short)
   -- Iir_Kind_Low_Type_Attribute (Short)
   -- Iir_Kind_Ascending_Type_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Range_Array_Attribute (Short)
   -- Iir_Kind_Reverse_Range_Array_Attribute (Short)
   -- Iir_Kind_Left_Array_Attribute (Short)
   -- Iir_Kind_Right_Array_Attribute (Short)
   -- Iir_Kind_High_Array_Attribute (Short)
   -- Iir_Kind_Low_Array_Attribute (Short)
   -- Iir_Kind_Ascending_Array_Attribute (Short)
   -- Iir_Kind_Length_Array_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --  Set only when known to be constrained.
   --   Get/Set_Index_Subtype (Field2)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Subtype_Attribute (Short)
   -- Iir_Kind_Element_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Stable_Attribute (Short)
   -- Iir_Kind_Delayed_Attribute (Short)
   -- Iir_Kind_Quiet_Attribute (Short)
   -- Iir_Kind_Transaction_Attribute (Short)
   --  (Iir_Kinds_Signal_Attribute)
   --
   --   Get/Set_Prefix (Field0)
   --
   --  Not used by Iir_Kind_Transaction_Attribute
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --  Next attribute signal in the chain owned by the
   --  signal_attribute_declaration.  Usual Get/Set_Chain is not used here as
   --  the chain is composed only of forward references.
   --   Get/Set_Attr_Chain (Field2)
   --
   --  Head of the chain.  Used only to ease the reconstruction of the chain.
   --   Get/Set_Signal_Attribute_Declaration (Field3)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Event_Attribute (Short)
   -- Iir_Kind_Last_Event_Attribute (Short)
   -- Iir_Kind_Last_Value_Attribute (Short)
   -- Iir_Kind_Active_Attribute (Short)
   -- Iir_Kind_Last_Active_Attribute (Short)
   -- Iir_Kind_Driving_Attribute (Short)
   -- Iir_Kind_Driving_Value_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Pos_Attribute (Short)
   -- Iir_Kind_Val_Attribute (Short)
   -- Iir_Kind_Succ_Attribute (Short)
   -- Iir_Kind_Pred_Attribute (Short)
   -- Iir_Kind_Leftof_Attribute (Short)
   -- Iir_Kind_Rightof_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Image_Attribute (Short)
   -- Iir_Kind_Value_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Simple_Name_Attribute (Short)
   -- Iir_Kind_Instance_Name_Attribute (Short)
   -- Iir_Kind_Path_Name_Attribute (Short)
   --
   --   Get/Set_Prefix (Field0)
   --
   -- Only for Iir_Kind_Simple_Name_Attribute:
   --   Get/Set_Simple_Name_Identifier (Field3)
   --
   -- Only for Iir_Kind_Simple_Name_Attribute:
   --   Get/Set_Simple_Name_Subtype (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Behavior_Attribute (Short)
   -- Iir_Kind_Structure_Attribute (Short)
   --  FIXME: to describe (Short)

   -- Iir_Kind_Error (Short)
   --  Can be used instead of an expression or a type.
   --   Get/Set_Type (Field1)
   --   Get/Set_Nature (Alias Field1)
   --
   --   Get/Set_Error_Origin (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Type_Staticness (Alias State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)

   -- Iir_Kind_Unused (Short)

   -- End of Iir_Kind.


   type Iir_Kind is
     (
      Iir_Kind_Unused,
      Iir_Kind_Error,

      Iir_Kind_Design_File,

      Iir_Kind_Design_Unit,

      Iir_Kind_Library_Clause,
      Iir_Kind_Use_Clause,
      Iir_Kind_Context_Reference,

      Iir_Kind_PSL_Inherit_Spec,

   -- Literals.
      Iir_Kind_Integer_Literal,
      Iir_Kind_Floating_Point_Literal,
      Iir_Kind_Null_Literal,
      Iir_Kind_String_Literal8,
      Iir_Kind_Physical_Int_Literal,
      Iir_Kind_Physical_Fp_Literal,
      Iir_Kind_Simple_Aggregate,
      Iir_Kind_Overflow_Literal,

      Iir_Kind_Unaffected_Waveform,

   -- Tuple,
      Iir_Kind_Waveform_Element,
      Iir_Kind_Conditional_Waveform,
      Iir_Kind_Conditional_Expression,
      Iir_Kind_Association_Element_By_Expression,
      Iir_Kind_Association_Element_By_Name,
      Iir_Kind_Association_Element_By_Individual,
      Iir_Kind_Association_Element_Open,
      Iir_Kind_Association_Element_Package,
      Iir_Kind_Association_Element_Type,
      Iir_Kind_Association_Element_Subprogram,
      Iir_Kind_Association_Element_Terminal,
      Iir_Kind_Choice_By_Range,
      Iir_Kind_Choice_By_Expression,
      Iir_Kind_Choice_By_Others,
      Iir_Kind_Choice_By_None,
      Iir_Kind_Choice_By_Name,
      Iir_Kind_Entity_Aspect_Entity,
      Iir_Kind_Entity_Aspect_Configuration,
      Iir_Kind_Entity_Aspect_Open,
      Iir_Kind_Psl_Hierarchical_Name,
      Iir_Kind_Block_Configuration,
      Iir_Kind_Block_Header,
      Iir_Kind_Component_Configuration,
      Iir_Kind_Binding_Indication,
      Iir_Kind_Entity_Class,
      Iir_Kind_Attribute_Value,
      Iir_Kind_Signature,
      Iir_Kind_Aggregate_Info,
      Iir_Kind_Procedure_Call,
      Iir_Kind_Record_Element_Constraint,
      Iir_Kind_Array_Element_Resolution,
      Iir_Kind_Record_Resolution,
      Iir_Kind_Record_Element_Resolution,
      Iir_Kind_Break_Element,

      Iir_Kind_Attribute_Specification,
      Iir_Kind_Disconnection_Specification,
      Iir_Kind_Step_Limit_Specification,
      Iir_Kind_Configuration_Specification,

   -- Type definitions.
   -- iir_kinds_type_and_subtype_definition
   -- kinds: disc: discrete, st: subtype.
      Iir_Kind_Access_Type_Definition,
      Iir_Kind_Incomplete_Type_Definition,
      Iir_Kind_Interface_Type_Definition,
      Iir_Kind_File_Type_Definition,
      Iir_Kind_Protected_Type_Declaration,
      Iir_Kind_Record_Type_Definition,           -- composite
      Iir_Kind_Array_Type_Definition,            -- composite, array
      Iir_Kind_Array_Subtype_Definition,         -- composite, array, st
      Iir_Kind_Record_Subtype_Definition,        -- composite, st
      Iir_Kind_Access_Subtype_Definition,        -- st
      Iir_Kind_Physical_Subtype_Definition,      -- scalar, st, rng
      Iir_Kind_Floating_Subtype_Definition,      -- scalar, st, rng
      Iir_Kind_Integer_Subtype_Definition,       -- scalar, disc, st, rng
      Iir_Kind_Enumeration_Subtype_Definition,   -- scalar, disc, st, rng
      Iir_Kind_Enumeration_Type_Definition,      -- scalar, disc, rng
      Iir_Kind_Integer_Type_Definition,          -- scalar, disc
      Iir_Kind_Floating_Type_Definition,         -- scalar
      Iir_Kind_Physical_Type_Definition,         -- scalar
      Iir_Kind_Range_Expression,
      Iir_Kind_Protected_Type_Body,
      Iir_Kind_Wildcard_Type_Definition,
      Iir_Kind_Foreign_Vector_Type_Definition,
      Iir_Kind_Subtype_Definition,  -- temporary (must not appear after sem).

   -- Nature definition
      Iir_Kind_Scalar_Nature_Definition,
      Iir_Kind_Record_Nature_Definition,
      Iir_Kind_Array_Nature_Definition,
      Iir_Kind_Array_Subnature_Definition,

   -- Lists.
      Iir_Kind_Overload_List,  -- used internally by sem_expr.

   -- Declarations.
      Iir_Kind_Foreign_Module,
      Iir_Kind_Entity_Declaration,
      Iir_Kind_Configuration_Declaration,
      Iir_Kind_Context_Declaration,
      Iir_Kind_Package_Declaration,
      Iir_Kind_Package_Instantiation_Declaration,
      Iir_Kind_Vmode_Declaration,
      Iir_Kind_Vprop_Declaration,
      Iir_Kind_Vunit_Declaration,
      Iir_Kind_Package_Body,
      Iir_Kind_Architecture_Body,

      Iir_Kind_Type_Declaration,
      Iir_Kind_Anonymous_Type_Declaration,
      Iir_Kind_Subtype_Declaration,
      Iir_Kind_Nature_Declaration,
      Iir_Kind_Subnature_Declaration,
      Iir_Kind_Package_Header,
      Iir_Kind_Unit_Declaration,
      Iir_Kind_Library_Declaration,
      Iir_Kind_Component_Declaration,
      Iir_Kind_Attribute_Declaration,
      Iir_Kind_Group_Template_Declaration,
      Iir_Kind_Group_Declaration,
      Iir_Kind_Element_Declaration,
      Iir_Kind_Nature_Element_Declaration,
      Iir_Kind_Non_Object_Alias_Declaration,

      Iir_Kind_Psl_Declaration,
      Iir_Kind_Psl_Endpoint_Declaration,

      Iir_Kind_Enumeration_Literal,
      Iir_Kind_Function_Declaration,            --  Subprg, Func
      Iir_Kind_Procedure_Declaration,           --  Subprg, Proc
      Iir_Kind_Function_Body,
      Iir_Kind_Procedure_Body,
      Iir_Kind_Function_Instantiation_Declaration,
      Iir_Kind_Procedure_Instantiation_Declaration,

      Iir_Kind_Terminal_Declaration,

      Iir_Kind_Object_Alias_Declaration,       -- object
      Iir_Kind_Free_Quantity_Declaration,      -- object
      Iir_Kind_Spectrum_Quantity_Declaration,  -- object
      Iir_Kind_Noise_Quantity_Declaration,     -- object
      Iir_Kind_Across_Quantity_Declaration,    -- object
      Iir_Kind_Through_Quantity_Declaration,   -- object
      Iir_Kind_File_Declaration,               -- object
      Iir_Kind_Guard_Signal_Declaration,       -- object
      Iir_Kind_Signal_Declaration,             -- object
      Iir_Kind_Variable_Declaration,           -- object
      Iir_Kind_Constant_Declaration,           -- object
      Iir_Kind_Iterator_Declaration,           -- object
      Iir_Kind_Interface_Constant_Declaration, -- object, interface
      Iir_Kind_Interface_Variable_Declaration, -- object, interface
      Iir_Kind_Interface_Signal_Declaration,   -- object, interface
      Iir_Kind_Interface_File_Declaration,     -- object, interface
      Iir_Kind_Interface_Quantity_Declaration, -- object, interface
      Iir_Kind_Interface_Terminal_Declaration, --         interface
      Iir_Kind_Interface_Type_Declaration,     --         interface
      Iir_Kind_Interface_Package_Declaration,  --         interface
      Iir_Kind_Interface_Function_Declaration, --         interface
      Iir_Kind_Interface_Procedure_Declaration, --        interface

      Iir_Kind_Signal_Attribute_Declaration,
      Iir_Kind_Suspend_State_Declaration,

   -- Expressions.
      Iir_Kind_Identity_Operator,
      Iir_Kind_Negation_Operator,
      Iir_Kind_Absolute_Operator,
      Iir_Kind_Not_Operator,
      Iir_Kind_Implicit_Condition_Operator,
      Iir_Kind_Condition_Operator,
      Iir_Kind_Reduction_And_Operator,
      Iir_Kind_Reduction_Or_Operator,
      Iir_Kind_Reduction_Nand_Operator,
      Iir_Kind_Reduction_Nor_Operator,
      Iir_Kind_Reduction_Xor_Operator,
      Iir_Kind_Reduction_Xnor_Operator,
      Iir_Kind_And_Operator,
      Iir_Kind_Or_Operator,
      Iir_Kind_Nand_Operator,
      Iir_Kind_Nor_Operator,
      Iir_Kind_Xor_Operator,
      Iir_Kind_Xnor_Operator,
      Iir_Kind_Equality_Operator,
      Iir_Kind_Inequality_Operator,
      Iir_Kind_Less_Than_Operator,
      Iir_Kind_Less_Than_Or_Equal_Operator,
      Iir_Kind_Greater_Than_Operator,
      Iir_Kind_Greater_Than_Or_Equal_Operator,
      Iir_Kind_Match_Equality_Operator,
      Iir_Kind_Match_Inequality_Operator,
      Iir_Kind_Match_Less_Than_Operator,
      Iir_Kind_Match_Less_Than_Or_Equal_Operator,
      Iir_Kind_Match_Greater_Than_Operator,
      Iir_Kind_Match_Greater_Than_Or_Equal_Operator,
      Iir_Kind_Sll_Operator,
      Iir_Kind_Sla_Operator,
      Iir_Kind_Srl_Operator,
      Iir_Kind_Sra_Operator,
      Iir_Kind_Rol_Operator,
      Iir_Kind_Ror_Operator,
      Iir_Kind_Addition_Operator,
      Iir_Kind_Substraction_Operator,
      Iir_Kind_Concatenation_Operator,
      Iir_Kind_Multiplication_Operator,
      Iir_Kind_Division_Operator,
      Iir_Kind_Modulus_Operator,
      Iir_Kind_Remainder_Operator,
      Iir_Kind_Exponentiation_Operator,
      Iir_Kind_Function_Call,
      Iir_Kind_Aggregate,
      Iir_Kind_Parenthesis_Expression,
      Iir_Kind_Qualified_Expression,
      Iir_Kind_Type_Conversion,
      Iir_Kind_Allocator_By_Expression,
      Iir_Kind_Allocator_By_Subtype,
      Iir_Kind_Selected_Element,
      Iir_Kind_Dereference,
      Iir_Kind_Implicit_Dereference,
      Iir_Kind_Slice_Name,
      Iir_Kind_Indexed_Name,
      Iir_Kind_Psl_Prev,
      Iir_Kind_Psl_Stable,
      Iir_Kind_Psl_Rose,
      Iir_Kind_Psl_Fell,
      Iir_Kind_Psl_Onehot,
      Iir_Kind_Psl_Onehot0,
      Iir_Kind_Psl_Expression,

   -- Concurrent statements.
      Iir_Kind_Sensitized_Process_Statement,
      Iir_Kind_Process_Statement,
      Iir_Kind_Concurrent_Simple_Signal_Assignment,
      Iir_Kind_Concurrent_Conditional_Signal_Assignment,
      Iir_Kind_Concurrent_Selected_Signal_Assignment,
      Iir_Kind_Concurrent_Assertion_Statement,
      Iir_Kind_Concurrent_Procedure_Call_Statement,
      Iir_Kind_Concurrent_Break_Statement,
      Iir_Kind_Psl_Assert_Directive,
      Iir_Kind_Psl_Assume_Directive,
      Iir_Kind_Psl_Cover_Directive,
      Iir_Kind_Psl_Restrict_Directive,
      Iir_Kind_Block_Statement,
      Iir_Kind_If_Generate_Statement,
      Iir_Kind_Case_Generate_Statement,
      Iir_Kind_For_Generate_Statement,
      Iir_Kind_Component_Instantiation_Statement,

      Iir_Kind_Psl_Default_Clock,

      Iir_Kind_Generate_Statement_Body,
      Iir_Kind_If_Generate_Else_Clause,

   -- Simultaneous statements.
      Iir_Kind_Simple_Simultaneous_Statement,
      Iir_Kind_Simultaneous_Null_Statement,
      Iir_Kind_Simultaneous_Procedural_Statement,
      Iir_Kind_Simultaneous_Case_Statement,
      Iir_Kind_Simultaneous_If_Statement,
      Iir_Kind_Simultaneous_Elsif,

   -- Sequential statement
      Iir_Kind_Simple_Signal_Assignment_Statement,
      Iir_Kind_Conditional_Signal_Assignment_Statement,
      Iir_Kind_Selected_Waveform_Assignment_Statement,
      Iir_Kind_Signal_Force_Assignment_Statement,
      Iir_Kind_Signal_Release_Assignment_Statement,
      Iir_Kind_Null_Statement,
      Iir_Kind_Assertion_Statement,
      Iir_Kind_Report_Statement,
      Iir_Kind_Wait_Statement,
      Iir_Kind_Variable_Assignment_Statement,
      Iir_Kind_Conditional_Variable_Assignment_Statement,
      Iir_Kind_Return_Statement,
      Iir_Kind_For_Loop_Statement,
      Iir_Kind_While_Loop_Statement,
      Iir_Kind_Next_Statement,
      Iir_Kind_Exit_Statement,
      Iir_Kind_Case_Statement,
      Iir_Kind_Procedure_Call_Statement,
      Iir_Kind_Break_Statement,
      Iir_Kind_If_Statement,
      Iir_Kind_Suspend_State_Statement,
      Iir_Kind_Elsif,

   -- Names
      Iir_Kind_Character_Literal,              --  denoting_name
      Iir_Kind_Simple_Name,                    --  denoting_name
      Iir_Kind_Selected_Name,                  --  denoting_name
      Iir_Kind_Operator_Symbol,                --  denoting_name
      Iir_Kind_Reference_Name,                 --  denoting_name

      Iir_Kind_External_Constant_Name,
      Iir_Kind_External_Signal_Name,
      Iir_Kind_External_Variable_Name,

      Iir_Kind_Selected_By_All_Name,
      Iir_Kind_Parenthesis_Name,

      Iir_Kind_Package_Pathname,
      Iir_Kind_Absolute_Pathname,
      Iir_Kind_Relative_Pathname,
      Iir_Kind_Pathname_Element,

   -- Attributes
      Iir_Kind_Base_Attribute,
      Iir_Kind_Subtype_Attribute,
      Iir_Kind_Element_Attribute,
      Iir_Kind_Across_Attribute,
      Iir_Kind_Through_Attribute,
      Iir_Kind_Nature_Reference_Attribute,
      Iir_Kind_Left_Type_Attribute,            --  type_attribute
      Iir_Kind_Right_Type_Attribute,           --  type_attribute
      Iir_Kind_High_Type_Attribute,            --  type_attribute
      Iir_Kind_Low_Type_Attribute,             --  type_attribute
      Iir_Kind_Ascending_Type_Attribute,       --  type_attribute
      Iir_Kind_Image_Attribute,
      Iir_Kind_Value_Attribute,
      Iir_Kind_Pos_Attribute,                  --  scalar_type_attribute
      Iir_Kind_Val_Attribute,                  --  scalar_type_attribute
      Iir_Kind_Succ_Attribute,                 --  scalar_type_attribute
      Iir_Kind_Pred_Attribute,                 --  scalar_type_attribute
      Iir_Kind_Leftof_Attribute,               --  scalar_type_attribute
      Iir_Kind_Rightof_Attribute,              --  scalar_type_attribute
      Iir_Kind_Signal_Slew_Attribute,
      Iir_Kind_Quantity_Slew_Attribute,
      Iir_Kind_Ramp_Attribute,
      Iir_Kind_Zoh_Attribute,
      Iir_Kind_Ltf_Attribute,
      Iir_Kind_Ztf_Attribute,
      Iir_Kind_Dot_Attribute,
      Iir_Kind_Integ_Attribute,
      Iir_Kind_Above_Attribute,
      Iir_Kind_Quantity_Delayed_Attribute,
      Iir_Kind_Delayed_Attribute,              --  signal_attribute
      Iir_Kind_Stable_Attribute,               --  signal_attribute
      Iir_Kind_Quiet_Attribute,                --  signal_attribute
      Iir_Kind_Transaction_Attribute,          --  signal_attribute
      Iir_Kind_Event_Attribute,                --  signal_value_attribute
      Iir_Kind_Active_Attribute,               --  signal_value_attribute
      Iir_Kind_Last_Event_Attribute,           --  signal_value_attribute
      Iir_Kind_Last_Active_Attribute,          --  signal_value_attribute
      Iir_Kind_Last_Value_Attribute,           --  signal_value_attribute
      Iir_Kind_Driving_Attribute,              --  signal_value_attribute
      Iir_Kind_Driving_Value_Attribute,        --  signal_value_attribute
      Iir_Kind_Behavior_Attribute,
      Iir_Kind_Structure_Attribute,
      Iir_Kind_Simple_Name_Attribute,
      Iir_Kind_Instance_Name_Attribute,
      Iir_Kind_Path_Name_Attribute,
      Iir_Kind_Left_Array_Attribute,           --  array_attribute
      Iir_Kind_Right_Array_Attribute,          --  array_attribute
      Iir_Kind_High_Array_Attribute,           --  array_attribute
      Iir_Kind_Low_Array_Attribute,            --  array_attribute
      Iir_Kind_Length_Array_Attribute,         --  array_attribute
      Iir_Kind_Ascending_Array_Attribute,      --  array_attribute
      Iir_Kind_Range_Array_Attribute,          --  array_attribute
      Iir_Kind_Reverse_Range_Array_Attribute,  --  array_attribute

      Iir_Kind_Attribute_Name
     );

   --  Return TRUE iif K is K1 or K is K2.
   function Kind_In (K : Iir_Kind; K1, K2 : Iir_Kind) return Boolean;
   pragma Inline (Kind_In);

   type Iir_Signal_Kind is
     (
      Iir_Register_Kind,
      Iir_Bus_Kind
     );

   --  If the order of elements in IIR_MODE is modified, also modify the
   --  order in GRT (types and rtis).
   type Iir_Mode is
     (
      Iir_Unknown_Mode,
      Iir_Linkage_Mode,
      Iir_Buffer_Mode,
      Iir_Out_Mode,
      Iir_Inout_Mode,
      Iir_In_Mode
     );

   subtype Iir_In_Modes is Iir_Mode range Iir_Inout_Mode .. Iir_In_Mode;
   subtype Iir_Out_Modes is Iir_Mode range Iir_Out_Mode .. Iir_Inout_Mode;
   subtype Iir_Parameter_Modes is Iir_Mode range Iir_Out_Mode .. Iir_In_Mode;

   type Iir_Delay_Mechanism is
     (
      Iir_Inertial_Delay,
      Iir_Transport_Delay
     );

   type Iir_Force_Mode is
     (
      Iir_Force_In,
      Iir_Force_Out
     );

   --  LRM93 2.7 (conformance rules).
   --  To keep this simple, the layout is stored as a bit-string.
   --  Fields are:
   --  Get_Has_type: set if the interface is the last of a list.
   --  Get_Has_Mode: set if mode is explicit
   --  has_class: set if class (constant, signal, variable or file) is explicit
   --
   --  Example:
   --  procedure P (         A, B:       integer;
   --               constant C:    in    bit;
   --                        D:    inout bit;
   --               variable E:          bit;
   --                        F, G: in    bit;
   --               constant H, I:       bit;
   --               constant J, K: in    bit);
   --  A:
   --  B:                      has_type
   --  C, has_class, has_mode, has_type
   --  D:            has_mode, has_type
   --  E, has_class,           has_type
   --  F:            has_mode
   --  G:            has_mode, has_type
   --  H: has_class
   --  I: has_class,           has_type
   --  J: has_class, has_mode
   --  K: has_class, has_mode, has_type

   --  List of predefined operators and functions.
   type Iir_Predefined_Functions is
     (
      Iir_Predefined_Error,

      --  Predefined operators for BOOLEAN type

      --  LRM08 9.2.2 Logical Operators
      Iir_Predefined_Boolean_And,
      Iir_Predefined_Boolean_Or,
      Iir_Predefined_Boolean_Nand,
      Iir_Predefined_Boolean_Nor,
      Iir_Predefined_Boolean_Xor,
      Iir_Predefined_Boolean_Xnor,
      Iir_Predefined_Boolean_Not,

      --  LRM08 5.2.6 Predefined operations on scalar types.
      Iir_Predefined_Boolean_Rising_Edge,
      Iir_Predefined_Boolean_Falling_Edge,

      --  Predefined operators for any enumeration type.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Enum_Equality,
      Iir_Predefined_Enum_Inequality,
      Iir_Predefined_Enum_Less,
      Iir_Predefined_Enum_Less_Equal,
      Iir_Predefined_Enum_Greater,
      Iir_Predefined_Enum_Greater_Equal,

      --  Predefined operators for BIT type.

      --  LRM08 9.2.2 Logical Operators
      Iir_Predefined_Bit_And,
      Iir_Predefined_Bit_Or,
      Iir_Predefined_Bit_Nand,
      Iir_Predefined_Bit_Nor,
      Iir_Predefined_Bit_Xor,
      Iir_Predefined_Bit_Xnor,
      Iir_Predefined_Bit_Not,

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Bit_Match_Equality,
      Iir_Predefined_Bit_Match_Inequality,
      Iir_Predefined_Bit_Match_Less,
      Iir_Predefined_Bit_Match_Less_Equal,
      Iir_Predefined_Bit_Match_Greater,
      Iir_Predefined_Bit_Match_Greater_Equal,

      --  LRM08 9.2.9 Condition operator
      Iir_Predefined_Bit_Condition,

      --  Predefined operators for any integer type.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Integer_Equality,
      Iir_Predefined_Integer_Inequality,
      Iir_Predefined_Integer_Less,
      Iir_Predefined_Integer_Less_Equal,
      Iir_Predefined_Integer_Greater,
      Iir_Predefined_Integer_Greater_Equal,

      --  LRM08 9.2.6 Sign operators
      Iir_Predefined_Integer_Identity,
      Iir_Predefined_Integer_Negation,

      --  LRM08 9.2.8 Miscellaneous operators
      Iir_Predefined_Integer_Absolute,

      --  LRM08 9.2.5 Adding operators
      Iir_Predefined_Integer_Plus,
      Iir_Predefined_Integer_Minus,

      --  LRM08 9.2.7 Multiplying operators
      Iir_Predefined_Integer_Mul,
      Iir_Predefined_Integer_Div,
      Iir_Predefined_Integer_Mod,
      Iir_Predefined_Integer_Rem,

      --  LRM08 9.2.8 Miscellaneous operators
      Iir_Predefined_Integer_Exp,

      --  Predefined operators for any floating type.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Floating_Equality,
      Iir_Predefined_Floating_Inequality,
      Iir_Predefined_Floating_Less,
      Iir_Predefined_Floating_Less_Equal,
      Iir_Predefined_Floating_Greater,
      Iir_Predefined_Floating_Greater_Equal,

      --  LRM08 9.2.6 Sign operators
      Iir_Predefined_Floating_Identity,
      Iir_Predefined_Floating_Negation,

      --  LRM08 9.2.8 Miscellaneous operators
      Iir_Predefined_Floating_Absolute,

      --  LRM08 9.2.5 Adding operators
      Iir_Predefined_Floating_Plus,
      Iir_Predefined_Floating_Minus,

      --  LRM08 9.2.7 Multiplying operators
      Iir_Predefined_Floating_Mul,
      Iir_Predefined_Floating_Div,

      --  LRM08 9.2.8 Miscellaneous operators
      Iir_Predefined_Floating_Exp,

      --  Predefined operator for universal types.

      --  LRM08 9.2.7 Multiplying operators
      Iir_Predefined_Universal_R_I_Mul,
      Iir_Predefined_Universal_I_R_Mul,
      Iir_Predefined_Universal_R_I_Div,

      --  Predefined operators for physical types.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Physical_Equality,
      Iir_Predefined_Physical_Inequality,
      Iir_Predefined_Physical_Less,
      Iir_Predefined_Physical_Less_Equal,
      Iir_Predefined_Physical_Greater,
      Iir_Predefined_Physical_Greater_Equal,

      --  LRM08 9.2.6 Sign operators
      Iir_Predefined_Physical_Identity,
      Iir_Predefined_Physical_Negation,

      --  LRM08 9.2.8 Miscellaneous operators
      Iir_Predefined_Physical_Absolute,

      --  LRM08 9.2.5 Adding operators
      Iir_Predefined_Physical_Plus,
      Iir_Predefined_Physical_Minus,

      --  LRM08 9.2.7 Multiplying operators
      Iir_Predefined_Physical_Integer_Mul,
      Iir_Predefined_Physical_Real_Mul,
      Iir_Predefined_Integer_Physical_Mul,
      Iir_Predefined_Real_Physical_Mul,
      Iir_Predefined_Physical_Integer_Div,
      Iir_Predefined_Physical_Real_Div,
      Iir_Predefined_Physical_Physical_Div,
      Iir_Predefined_Physical_Mod,
      Iir_Predefined_Physical_Rem,

      --  Predefined operators for access.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Access_Equality,
      Iir_Predefined_Access_Inequality,

      --  Predefined operators for record.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Record_Equality,
      Iir_Predefined_Record_Inequality,

      --  Predefined operators for array.

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Array_Equality,
      Iir_Predefined_Array_Inequality,
      Iir_Predefined_Array_Less,
      Iir_Predefined_Array_Less_Equal,
      Iir_Predefined_Array_Greater,
      Iir_Predefined_Array_Greater_Equal,

      --  LRM08 9.2.5 Adding operators
      Iir_Predefined_Array_Array_Concat,
      Iir_Predefined_Array_Element_Concat,
      Iir_Predefined_Element_Array_Concat,
      Iir_Predefined_Element_Element_Concat,

      --  LRM08 5.3.2.4 Predefined operations on array types
      Iir_Predefined_Array_Minimum,
      Iir_Predefined_Array_Maximum,
      Iir_Predefined_Vector_Minimum,
      Iir_Predefined_Vector_Maximum,

      --  LRM08 9.2.4 Shift operators
      Iir_Predefined_Array_Sll,
      Iir_Predefined_Array_Srl,
      Iir_Predefined_Array_Sla,
      Iir_Predefined_Array_Sra,
      Iir_Predefined_Array_Rol,
      Iir_Predefined_Array_Ror,

      --  LRM08 9.2.2 Logical operators
      --  Predefined operators for one dimensional array.
      --  For bit and boolean type, the operations are the same.  To be
      --  neutral, we use TF (for True/False) instead of Bit, Boolean or
      --  Logic.
      Iir_Predefined_TF_Array_And,
      Iir_Predefined_TF_Array_Or,
      Iir_Predefined_TF_Array_Nand,
      Iir_Predefined_TF_Array_Nor,
      Iir_Predefined_TF_Array_Xor,
      Iir_Predefined_TF_Array_Xnor,
      Iir_Predefined_TF_Array_Not,

      --  LRM08 9.2.2 Logical operators
      Iir_Predefined_TF_Reduction_And,
      Iir_Predefined_TF_Reduction_Or,
      Iir_Predefined_TF_Reduction_Nand,
      Iir_Predefined_TF_Reduction_Nor,
      Iir_Predefined_TF_Reduction_Xor,
      Iir_Predefined_TF_Reduction_Xnor,
      Iir_Predefined_TF_Reduction_Not,

      --  LRM08 9.2.2 Logical operators
      Iir_Predefined_TF_Array_Element_And,
      Iir_Predefined_TF_Element_Array_And,
      Iir_Predefined_TF_Array_Element_Or,
      Iir_Predefined_TF_Element_Array_Or,
      Iir_Predefined_TF_Array_Element_Nand,
      Iir_Predefined_TF_Element_Array_Nand,
      Iir_Predefined_TF_Array_Element_Nor,
      Iir_Predefined_TF_Element_Array_Nor,
      Iir_Predefined_TF_Array_Element_Xor,
      Iir_Predefined_TF_Element_Array_Xor,
      Iir_Predefined_TF_Array_Element_Xnor,
      Iir_Predefined_TF_Element_Array_Xnor,

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Bit_Array_Match_Equality,
      Iir_Predefined_Bit_Array_Match_Inequality,

      --  LRM08 9.2.3 Relational Operators
      --  IEEE.Std_Logic_1164.Std_Ulogic
      Iir_Predefined_Std_Ulogic_Match_Equality,
      Iir_Predefined_Std_Ulogic_Match_Inequality,
      Iir_Predefined_Std_Ulogic_Match_Less,
      Iir_Predefined_Std_Ulogic_Match_Less_Equal,
      Iir_Predefined_Std_Ulogic_Match_Greater,
      Iir_Predefined_Std_Ulogic_Match_Greater_Equal,

      --  LRM08 9.2.3 Relational Operators
      Iir_Predefined_Std_Ulogic_Array_Match_Equality,
      Iir_Predefined_Std_Ulogic_Array_Match_Inequality,

      --  LRM08 5.2.6 Predefined operations on scalar types.
      Iir_Predefined_Enum_Minimum,
      Iir_Predefined_Enum_Maximum,
      Iir_Predefined_Enum_To_String,

      --  LRM08 5.2.6 Predefined operations on scalar types.
      Iir_Predefined_Integer_Minimum,
      Iir_Predefined_Integer_Maximum,
      Iir_Predefined_Integer_To_String,

      --  LRM08 5.2.6 Predefined operations on scalar types.
      Iir_Predefined_Bit_Rising_Edge,
      Iir_Predefined_Bit_Falling_Edge,

      --  LRM08 5.2.6 Predefined operations on scalar types.
      Iir_Predefined_Floating_Minimum,
      Iir_Predefined_Floating_Maximum,
      Iir_Predefined_Floating_To_String,
      Iir_Predefined_Real_To_String_Digits,
      Iir_Predefined_Real_To_String_Format,

      --  LRM08 5.2.6 Predefined operations on scalar types.
      Iir_Predefined_Physical_Minimum,
      Iir_Predefined_Physical_Maximum,
      Iir_Predefined_Physical_To_String,
      Iir_Predefined_Time_To_String_Unit,

      --  LRM08 5.3.2.4 Predefined operations on array types
      Iir_Predefined_Array_Char_To_String,
      Iir_Predefined_Bit_Vector_To_Ostring,
      Iir_Predefined_Bit_Vector_To_Hstring,

      --  --  Predefined attribute functions.
      --  Iir_Predefined_Attribute_Image,
      --  Iir_Predefined_Attribute_Value,
      --  Iir_Predefined_Attribute_Pos,
      --  Iir_Predefined_Attribute_Val,
      --  Iir_Predefined_Attribute_Succ,
      --  Iir_Predefined_Attribute_Pred,
      --  Iir_Predefined_Attribute_Leftof,
      --  Iir_Predefined_Attribute_Rightof,
      --  Iir_Predefined_Attribute_Left,
      --  Iir_Predefined_Attribute_Right,
      --  Iir_Predefined_Attribute_Event,
      --  Iir_Predefined_Attribute_Active,
      --  Iir_Predefined_Attribute_Last_Event,
      --  Iir_Predefined_Attribute_Last_Active,
      --  Iir_Predefined_Attribute_Last_Value,
      --  Iir_Predefined_Attribute_Driving,
      --  Iir_Predefined_Attribute_Driving_Value,

      --  Impure subprograms.

      --  LRM08 5.4.3 Allocation and deallocation of objects
      Iir_Predefined_Deallocate,

      --  LRM08 5.5.2 File operations
      Iir_Predefined_File_Open,
      Iir_Predefined_File_Open_Status,
      Iir_Predefined_File_Close,
      Iir_Predefined_Read,
      Iir_Predefined_Read_Length,
      Iir_Predefined_Flush,
      Iir_Predefined_Write,
      Iir_Predefined_Endfile,

      --  Misc impure functions.
      Iir_Predefined_Now_Function,
      Iir_Predefined_Real_Now_Function,
      Iir_Predefined_Frequency_Function,

      --  A not predefined and not known function.  User function.
      Iir_Predefined_None,

      --  Intrinsic foreign subprograms.
      Iir_Predefined_Foreign_Untruncated_Text_Read,
      Iir_Predefined_Foreign_Textio_Read_Real,
      Iir_Predefined_Foreign_Textio_Write_Real,

      --  Defined in package std.env
      Iir_Predefined_Std_Env_Stop_Status,
      Iir_Predefined_Std_Env_Stop,
      Iir_Predefined_Std_Env_Finish_Status,
      Iir_Predefined_Std_Env_Finish,
      Iir_Predefined_Std_Env_Resolution_Limit,

      --  Defined in package ieee.std_logic_1164

      --  Std_Ulogic operations.
      Iir_Predefined_Ieee_1164_Scalar_And,
      Iir_Predefined_Ieee_1164_Scalar_Nand,
      Iir_Predefined_Ieee_1164_Scalar_Or,
      Iir_Predefined_Ieee_1164_Scalar_Nor,
      Iir_Predefined_Ieee_1164_Scalar_Xor,
      Iir_Predefined_Ieee_1164_Scalar_Xnor,
      Iir_Predefined_Ieee_1164_Scalar_Not,

      --  Std_Logic_Vector or Std_Ulogic_Vector operations.
      --  Length of the result is the length of the left operand.
      Iir_Predefined_Ieee_1164_Vector_And,
      Iir_Predefined_Ieee_1164_Vector_Nand,
      Iir_Predefined_Ieee_1164_Vector_Or,
      Iir_Predefined_Ieee_1164_Vector_Nor,
      Iir_Predefined_Ieee_1164_Vector_Xor,
      Iir_Predefined_Ieee_1164_Vector_Xnor,
      Iir_Predefined_Ieee_1164_Vector_Not,

      Iir_Predefined_Ieee_1164_To_Bit,
      Iir_Predefined_Ieee_1164_To_Bitvector,
      Iir_Predefined_Ieee_1164_To_Stdulogic,
      Iir_Predefined_Ieee_1164_To_Stdlogicvector_Bv,
      Iir_Predefined_Ieee_1164_To_Stdlogicvector_Suv,
      Iir_Predefined_Ieee_1164_To_Stdulogicvector_Bv,
      Iir_Predefined_Ieee_1164_To_Stdulogicvector_Slv,

      Iir_Predefined_Ieee_1164_To_X01_Slv,
      Iir_Predefined_Ieee_1164_To_X01_Suv,
      Iir_Predefined_Ieee_1164_To_X01_Log,
      Iir_Predefined_Ieee_1164_To_X01_Bv_Slv,
      Iir_Predefined_Ieee_1164_To_X01_Bv_Suv,
      Iir_Predefined_Ieee_1164_To_X01_Bit_Log,

      Iir_Predefined_Ieee_1164_To_X01Z_Slv,
      Iir_Predefined_Ieee_1164_To_X01Z_Suv,
      Iir_Predefined_Ieee_1164_To_X01Z_Log,
      Iir_Predefined_Ieee_1164_To_X01Z_Bv_Slv,
      Iir_Predefined_Ieee_1164_To_X01Z_Bv_Suv,
      Iir_Predefined_Ieee_1164_To_X01Z_Bit_Log,

      Iir_Predefined_Ieee_1164_To_UX01_Slv,
      Iir_Predefined_Ieee_1164_To_UX01_Suv,
      Iir_Predefined_Ieee_1164_To_UX01_Log,
      Iir_Predefined_Ieee_1164_To_UX01_Bv_Slv,
      Iir_Predefined_Ieee_1164_To_UX01_Bv_Suv,
      Iir_Predefined_Ieee_1164_To_UX01_Bit_Log,

      Iir_Predefined_Ieee_1164_Vector_Is_X,
      Iir_Predefined_Ieee_1164_Scalar_Is_X,

      Iir_Predefined_Ieee_1164_Rising_Edge,
      Iir_Predefined_Ieee_1164_Falling_Edge,

      -- VHDL-2008 vector/element logic operators
      Iir_Predefined_Ieee_1164_And_Suv_Log,
      Iir_Predefined_Ieee_1164_And_Log_Suv,
      Iir_Predefined_Ieee_1164_Nand_Suv_Log,
      Iir_Predefined_Ieee_1164_Nand_Log_Suv,
      Iir_Predefined_Ieee_1164_Or_Suv_Log,
      Iir_Predefined_Ieee_1164_Or_Log_Suv,
      Iir_Predefined_Ieee_1164_Nor_Suv_Log,
      Iir_Predefined_Ieee_1164_Nor_Log_Suv,
      Iir_Predefined_Ieee_1164_Xor_Suv_Log,
      Iir_Predefined_Ieee_1164_Xor_Log_Suv,
      Iir_Predefined_Ieee_1164_Xnor_Suv_Log,
      Iir_Predefined_Ieee_1164_Xnor_Log_Suv,

      -- VHDL-2008 unary logic operators
      Iir_Predefined_Ieee_1164_And_Suv,
      Iir_Predefined_Ieee_1164_Nand_Suv,
      Iir_Predefined_Ieee_1164_Or_Suv,
      Iir_Predefined_Ieee_1164_Nor_Suv,
      Iir_Predefined_Ieee_1164_Xor_Suv,
      Iir_Predefined_Ieee_1164_Xnor_Suv,

      Iir_Predefined_Ieee_1164_Vector_Sll,
      Iir_Predefined_Ieee_1164_Vector_Srl,
      Iir_Predefined_Ieee_1164_Vector_Rol,
      Iir_Predefined_Ieee_1164_Vector_Ror,

      Iir_Predefined_Ieee_1164_Condition_Operator,

      Iir_Predefined_Ieee_1164_To_01_Log_Log,
      Iir_Predefined_Ieee_1164_To_01_Slv_Log,

      Iir_Predefined_Ieee_1164_To_Hstring,
      Iir_Predefined_Ieee_1164_To_Ostring,

      --  Numeric_Std.
      --  Abbreviations:
      --  Uns: Unsigned, Sgn: Signed, Nat: Natural, Int: Integer.

      --  To_Integer, To_Unsigned, to_Signed
      Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Sgn_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Sgn,

      --  Numeric_Std operators (Start)
      Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Add_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Add_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Add_Int_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Add_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Int_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Mul_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Div_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Div_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Rem_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Rem_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Mod_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Mod_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Gt_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Lt_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Le_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Le_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Ge_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Ge_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Eq_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Eq_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Ne_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Ne_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Match_Le_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Match_Le_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Sll_Uns_Int,
      Iir_Predefined_Ieee_Numeric_Std_Sll_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int,
      Iir_Predefined_Ieee_Numeric_Std_Srl_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Sla_Uns_Int,
      Iir_Predefined_Ieee_Numeric_Std_Sla_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Sra_Uns_Int,
      Iir_Predefined_Ieee_Numeric_Std_Sra_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Rol_Uns_Int,
      Iir_Predefined_Ieee_Numeric_Std_Rol_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Ror_Uns_Int,
      Iir_Predefined_Ieee_Numeric_Std_Ror_Sgn_Int,

      Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_And_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_And_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_And_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Or_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Or_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Log_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Log,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Log_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Log,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Log_Sgn,
      --  Numeric_Std binary operators (end)

      --  Unary functions for numeric_std
      Iir_Predefined_Ieee_Numeric_Std_Not_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Not_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Abs_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Neg_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn,

      --  Min and Max.
      Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Min_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Min_Int_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Max_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Std_Max_Int_Sgn,

      --  Shift and rotate functions.
      Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Sgn_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Sgn_Nat,

      Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Sgn_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Sgn_Nat,

      --  Reduction
      Iir_Predefined_Ieee_Numeric_Std_And_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Or_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_And_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Nand_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Or_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Nor_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Xor_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns,

      --  Find.
      Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Sgn,

      --  Std_Match functions.
      Iir_Predefined_Ieee_Numeric_Std_Match_Log,
      Iir_Predefined_Ieee_Numeric_Std_Match_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Match_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_Match_Slv,
      Iir_Predefined_Ieee_Numeric_Std_Match_Suv,

      Iir_Predefined_Ieee_Numeric_Std_To_01_Uns,
      Iir_Predefined_Ieee_Numeric_Std_To_01_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_To_X01_Uns,
      Iir_Predefined_Ieee_Numeric_Std_To_X01_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_To_X01Z_Uns,
      Iir_Predefined_Ieee_Numeric_Std_To_X01Z_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_To_UX01_Uns,
      Iir_Predefined_Ieee_Numeric_Std_To_UX01_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_Is_X_Uns,
      Iir_Predefined_Ieee_Numeric_Std_Is_X_Sgn,

      Iir_Predefined_Ieee_Numeric_Std_To_Hstring_Uns,
      Iir_Predefined_Ieee_Numeric_Std_To_Ostring_Uns,

      Iir_Predefined_Ieee_Numeric_Std_To_Hstring_Sgn,
      Iir_Predefined_Ieee_Numeric_Std_To_Ostring_Sgn,

      --  numeric_bit

      --  To_Integer, To_Unsigned, to_Signed
      Iir_Predefined_Ieee_Numeric_Bit_Toint_Uns_Nat,
      Iir_Predefined_Ieee_Numeric_Bit_Toint_Sgn_Int,
      Iir_Predefined_Ieee_Numeric_Bit_Touns_Nat_Nat_Uns,
      Iir_Predefined_Ieee_Numeric_Bit_Touns_Nat_Uns_Uns,
      Iir_Predefined_Ieee_Numeric_Bit_Tosgn_Int_Nat_Sgn,
      Iir_Predefined_Ieee_Numeric_Bit_Tosgn_Int_Sgn_Sgn,

      --  Numeric_Std_Unsigned (ieee2008)
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Slv,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Nat_Slv,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Slv_Slv,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Slv_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Nat_Slv,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Find_Rightmost,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Find_Leftmost,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Shift_Left,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Shift_Right,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Rotate_Left,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Rotate_Right,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Slv,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Suv,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Resize_Slv_Nat,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Resize_Slv_Slv,

      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Maximum_Slv_Slv,
      Iir_Predefined_Ieee_Numeric_Std_Unsigned_Minimum_Slv_Slv,

      --  Math_Real
      Iir_Predefined_Ieee_Math_Real_Sign,
      Iir_Predefined_Ieee_Math_Real_Ceil,
      Iir_Predefined_Ieee_Math_Real_Floor,
      Iir_Predefined_Ieee_Math_Real_Round,
      Iir_Predefined_Ieee_Math_Real_Trunc,
      Iir_Predefined_Ieee_Math_Real_Mod,
      Iir_Predefined_Ieee_Math_Real_Realmax,
      Iir_Predefined_Ieee_Math_Real_Realmin,
      Iir_Predefined_Ieee_Math_Real_Sqrt,
      Iir_Predefined_Ieee_Math_Real_Cbrt,
      Iir_Predefined_Ieee_Math_Real_Pow_Int_Real,
      Iir_Predefined_Ieee_Math_Real_Pow_Real_Real,
      Iir_Predefined_Ieee_Math_Real_Exp,
      Iir_Predefined_Ieee_Math_Real_Log,
      Iir_Predefined_Ieee_Math_Real_Log2,
      Iir_Predefined_Ieee_Math_Real_Log10,
      Iir_Predefined_Ieee_Math_Real_Log_Real_Real,
      Iir_Predefined_Ieee_Math_Real_Sin,
      Iir_Predefined_Ieee_Math_Real_Cos,
      Iir_Predefined_Ieee_Math_Real_Tan,
      Iir_Predefined_Ieee_Math_Real_Arcsin,
      Iir_Predefined_Ieee_Math_Real_Arccos,
      Iir_Predefined_Ieee_Math_Real_Arctan,
      Iir_Predefined_Ieee_Math_Real_Arctan_Real_Real,
      Iir_Predefined_Ieee_Math_Real_Sinh,
      Iir_Predefined_Ieee_Math_Real_Cosh,
      Iir_Predefined_Ieee_Math_Real_Tanh,
      Iir_Predefined_Ieee_Math_Real_Arcsinh,
      Iir_Predefined_Ieee_Math_Real_Arccosh,
      Iir_Predefined_Ieee_Math_Real_Arctanh,

      --  Std_Logic_Unsigned (synopsys extension).
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Log,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Log_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Log,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Log_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Id_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Mul_Slv_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Conv_Integer,

      Iir_Predefined_Ieee_Std_Logic_Unsigned_Shl,
      Iir_Predefined_Ieee_Std_Logic_Unsigned_Shr,

      --  Std_Logic_Signed (synopsys extension).
      Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Add_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Log,
      Iir_Predefined_Ieee_Std_Logic_Signed_Add_Log_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Log,
      Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Log_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Id_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Neg_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Abs_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Mul_Slv_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Le_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Le_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Le_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Slv_Slv,
      Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Slv_Int,
      Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Int_Slv,

      Iir_Predefined_Ieee_Std_Logic_Signed_Conv_Integer,

      Iir_Predefined_Ieee_Std_Logic_Signed_Shl,
      Iir_Predefined_Ieee_Std_Logic_Signed_Shr,

      --  std_logic_arith (synopsys extension).
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Log,

      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Log,

      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Log,

      Iir_Predefined_Ieee_Std_Logic_Arith_Ext,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sxt,

      Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Slv,

      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv,

      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv,

      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv,
      Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv,

      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Sgn,

      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Sgn,

      --  std_logic_misc (synopsys extension)
      Iir_Predefined_Ieee_Std_Logic_Misc_And_Reduce_Slv,
      Iir_Predefined_Ieee_Std_Logic_Misc_And_Reduce_Suv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Nand_Reduce_Slv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Nand_Reduce_Suv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Slv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Suv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Nor_Reduce_Slv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Nor_Reduce_Suv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Xor_Reduce_Slv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Xor_Reduce_Suv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Xnor_Reduce_Slv,
      Iir_Predefined_Ieee_Std_Logic_Misc_Xnor_Reduce_Suv
     );

   --  Return TRUE iff FUNC is a short-cut predefined function.
   function Iir_Predefined_Shortcut_P (Func : Iir_Predefined_Functions)
     return Boolean;

   --  Pure and impure functions form a partition of implicit functions.
   subtype Iir_Predefined_Pure_Functions is Iir_Predefined_Functions range
     Iir_Predefined_Boolean_And ..
     Iir_Predefined_Functions'Pred (Iir_Predefined_Deallocate);
   subtype Iir_Predefined_Operators is Iir_Predefined_Functions range
     Iir_Predefined_Boolean_And ..
     Iir_Predefined_Std_Ulogic_Array_Match_Inequality;
   subtype Iir_Predefined_Impure_Functions is Iir_Predefined_Functions range
     Iir_Predefined_Deallocate ..
     Iir_Predefined_Functions'Pred (Iir_Predefined_None);

   subtype Iir_Predefined_TF_Array_Functions
      is Iir_Predefined_Functions range
     Iir_Predefined_TF_Array_And ..
   --Iir_Predefined_TF_Array_Or
   --Iir_Predefined_TF_Array_Nand
   --Iir_Predefined_TF_Array_Nor
   --Iir_Predefined_TF_Array_Xor
   --Iir_Predefined_TF_Array_Xnor
     Iir_Predefined_TF_Array_Not;

   subtype Iir_Predefined_Dyadic_TF_Array_Functions
   is Iir_Predefined_Functions range
     Iir_Predefined_TF_Array_And ..
   --Iir_Predefined_TF_Array_Or
   --Iir_Predefined_TF_Array_Nand
   --Iir_Predefined_TF_Array_Nor
   --Iir_Predefined_TF_Array_Xor
     Iir_Predefined_TF_Array_Xnor;

   subtype Iir_Predefined_Shift_Functions is Iir_Predefined_Functions range
     Iir_Predefined_Array_Sll ..
   --Iir_Predefined_Array_Srl
   --Iir_Predefined_Array_Sla
   --Iir_Predefined_Array_Sra
   --Iir_Predefined_Array_Rol
     Iir_Predefined_Array_Ror;

   subtype Iir_Predefined_Concat_Functions is Iir_Predefined_Functions range
     Iir_Predefined_Array_Array_Concat ..
   --Iir_Predefined_Array_Element_Concat
   --Iir_Predefined_Element_Array_Concat
     Iir_Predefined_Element_Element_Concat;

   subtype Iir_Predefined_Std_Ulogic_Match_Ordering_Functions is
     Iir_Predefined_Functions range
     Iir_Predefined_Std_Ulogic_Match_Less ..
   --Iir_Predefined_Std_Ulogic_Match_Less_Equal
   --Iir_Predefined_Std_Ulogic_Match_Greater
     Iir_Predefined_Std_Ulogic_Match_Greater_Equal;

   --  Subtype for implicit subprograms.  These have no corresponding bodies.
   --  Implicit and explicit subprograms are partitions: they are disjoint
   --  and cover all the cases.
   subtype Iir_Predefined_Implicit is Iir_Predefined_Functions range
     Iir_Predefined_Error ..
     Iir_Predefined_Functions'Pred (Iir_Predefined_None);

   --  Subtype for explicit subprograms.  These require a corresponding body.
   subtype Iir_Predefined_Explicit is Iir_Predefined_Functions range
     Iir_Predefined_None ..
     Iir_Predefined_Functions'Last;

   --  Explicit known subprograms (from ieee)
   subtype Iir_Predefined_IEEE_Explicit is Iir_Predefined_Functions range
     Iir_Predefined_Functions'Succ (Iir_Predefined_None) ..
     Iir_Predefined_Functions'Last;

   subtype Iir_Predefined_Ieee_Numeric_Std_Binary_Operators
     is Iir_Predefined_Functions range
       Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns ..
       Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn;

   subtype Iir_Predefined_Ieee_Numeric_Std_Unsigned_Operators
      is Iir_Predefined_Functions range
     Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Slv ..
     Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Nat_Slv;

   --  Size of scalar types.
   --  Their size is determined during analysis (using the range), so that
   --  all backends have the same view.
   type Scalar_Size is
     (
      Scalar_8,
      Scalar_16,
      Scalar_32,
      Scalar_64
     );

   --  Staticness as defined by LRM93 6.1 and 7.4
   type Iir_Staticness is
     (
      Unknown,
      None,
      Globally,
      Locally
     );

   -- Staticness as defined by LRM93 6.1 and 7.4
   function Min (L, R : Iir_Staticness) return Iir_Staticness renames
     Iir_Staticness'Min;

   --  Purity state of a procedure.
   --  PURE means the procedure is pure.
   --  IMPURE means the procedure is impure: it references a file object or
   --    a signal or a variable declared outside a subprogram, or it calls an
   --    impure subprogram.
   --  MAYBE_IMPURE means the procedure references a signal or a variable
   --    declared in a subprogram.  The relative position of a parent has to
   --    be considered.  The list of callees must not be checked.
   --  UNKNOWN is like MAYBE_IMPURE, but the subprogram has a list of callees
   --    whose purity is not yet known.  As a consequence, a direct or
   --    indirect call to such a procedure cannot be proved to be allowed
   --    in a pure function.
   --  Note: UNKNOWN is the default state.  At any impure call, the state is
   --    set to IMPURE.  Only at the end of body analysis and only if the
   --    callee list is empty, the state can be set either to MAYBE_IMPURE or
   --    PURE.
   type Iir_Pure_State is (Unknown, Pure, Maybe_Impure, Impure);

   --  State of subprograms for validity of use in all-sensitized process.
   --  INVALID_SIGNAL means that the subprogram is in a package and
   --    reads a signal or that the subprogram calls (indirectly) such
   --    a subprogram.  In this case, the subprogram cannot be called from
   --    an all-sensitized process.
   --  READ_SIGNAL means that the subprogram reads a signal and is defined
   --    in an entity or an architecture or that the subprogram calls
   --    (indirectly) such a subprogram.  In this case, the subprogram can
   --    be called from an all-sensitized process and the reference will be
   --    part of the sensitivity list.
   --  NO_SIGNAL means that the subprogram doesn't read any signal and don't
   --    call such a subprogram.  The subprogram can be called from an
   --    all-sensitized process but there is no need to track this call.
   --  UNKNOWN means that the state is not yet defined.
   type Iir_All_Sensitized is
     (Unknown, No_Signal, Read_Signal, Invalid_Signal);

   --  Constraint state of a type.
   --  See LRM08 5.1 for definition.
   type Iir_Constraint is
     (
      Unconstrained,
      Partially_Constrained,
      Fully_Constrained
     );

   --  The kind of an interface list.
   type Interface_Kind_Type is
     (
      Generic_Interface_List,
      Port_Interface_List,
      Procedure_Parameter_Interface_List,
      Function_Parameter_Interface_List
     );
   subtype Parameter_Interface_List is Interface_Kind_Type range
     Procedure_Parameter_Interface_List ..
     Function_Parameter_Interface_List;

   -- iir_int32 is aimed at containing integer literal values.
   type Iir_Int32 is new Int32;

   --  iir_index32 is aimed at containing an array index.
   type Iir_Index32 is new Nat32;

   ---------------
   -- subranges --
   ---------------
   -- These subtypes are used for ranges, for `case' statements or for the `in'
   -- operator.

   -- In order to be correctly parsed by check_iir, the declaration must
   -- follow these rules:
   -- * the first line must be "subtype Iir_Kinds_NAME is Iir_Kind_range"
   -- * the second line must be the lowest bound of the range, followed by "..
   -- * comments line
   -- * the last line must be the highest bound of the range, followed by ";"

   subtype Iir_Kinds_Library_Unit is Iir_Kind range
     Iir_Kind_Foreign_Module ..
   --Iir_Kind_Entity_Declaration
   --Iir_Kind_Configuration_Declaration
   --Iir_Kind_Context_Declaration
   --Iir_Kind_Package_Declaration
   --Iir_Kind_Package_Instantiation_Declaration
   --Iir_Kind_Vmode_Declaration
   --Iir_Kind_Vprop_Declaration
   --Iir_Kind_Vunit_Declaration
   --Iir_Kind_Package_Body
     Iir_Kind_Architecture_Body;

   subtype Iir_Kinds_Primary_Unit is Iir_Kind range
     Iir_Kind_Entity_Declaration ..
   --Iir_Kind_Configuration_Declaration
   --Iir_Kind_Context_Declaration
   --Iir_Kind_Package_Declaration
   --Iir_Kind_Package_Instantiation_Declaration
   --Iir_Kind_Vmode_Declaration
   --Iir_Kind_Vprop_Declaration
     Iir_Kind_Vunit_Declaration;

   subtype Iir_Kinds_Secondary_Unit is Iir_Kind range
     Iir_Kind_Package_Body ..
     Iir_Kind_Architecture_Body;

   subtype Iir_Kinds_Package_Declaration is Iir_Kind range
     Iir_Kind_Package_Declaration ..
     Iir_Kind_Package_Instantiation_Declaration;

   subtype Iir_Kinds_Verification_Unit is Iir_Kind range
     Iir_Kind_Vmode_Declaration ..
   --Iir_Kind_Vprop_Declaration
     Iir_Kind_Vunit_Declaration;

   --  Note: does not include iir_kind_enumeration_literal since it is
   --  considered as a declaration.
   subtype Iir_Kinds_Literal is Iir_Kind range
     Iir_Kind_Integer_Literal ..
   --Iir_Kind_Floating_Point_Literal
   --Iir_Kind_Null_Literal
   --Iir_Kind_String_Literal8
   --Iir_Kind_Physical_Int_Literal
     Iir_Kind_Physical_Fp_Literal;

   subtype Iir_Kinds_Physical_Literal is Iir_Kind range
     Iir_Kind_Physical_Int_Literal ..
     Iir_Kind_Physical_Fp_Literal;

   subtype Iir_Kinds_Array_Type_Definition is Iir_Kind range
     Iir_Kind_Array_Type_Definition ..
     Iir_Kind_Array_Subtype_Definition;

   subtype Iir_Kinds_Type_And_Subtype_Definition is Iir_Kind range
     Iir_Kind_Access_Type_Definition ..
   --Iir_Kind_Incomplete_Type_Definition
   --Iir_Kind_Interface_Type_Definition
   --Iir_Kind_File_Type_Definition
   --Iir_Kind_Protected_Type_Declaration
   --Iir_Kind_Record_Type_Definition
   --Iir_Kind_Array_Type_Definition
   --Iir_Kind_Array_Subtype_Definition
   --Iir_Kind_Record_Subtype_Definition
   --Iir_Kind_Access_Subtype_Definition
   --Iir_Kind_Physical_Subtype_Definition
   --Iir_Kind_Floating_Subtype_Definition
   --Iir_Kind_Integer_Subtype_Definition
   --Iir_Kind_Enumeration_Subtype_Definition
   --Iir_Kind_Enumeration_Type_Definition
   --Iir_Kind_Integer_Type_Definition
   --Iir_Kind_Floating_Type_Definition
     Iir_Kind_Physical_Type_Definition;

   subtype Iir_Kinds_Subtype_Definition is Iir_Kind range
     Iir_Kind_Array_Subtype_Definition ..
   --Iir_Kind_Record_Subtype_Definition
   --Iir_Kind_Access_Subtype_Definition
   --Iir_Kind_Physical_Subtype_Definition
   --Iir_Kind_Floating_Subtype_Definition
   --Iir_Kind_Integer_Subtype_Definition
     Iir_Kind_Enumeration_Subtype_Definition;

   subtype Iir_Kinds_Scalar_Subtype_Definition is Iir_Kind range
     Iir_Kind_Physical_Subtype_Definition ..
   --Iir_Kind_Floating_Subtype_Definition
   --Iir_Kind_Integer_Subtype_Definition
     Iir_Kind_Enumeration_Subtype_Definition;

   subtype Iir_Kinds_Scalar_Type_And_Subtype_Definition is Iir_Kind range
     Iir_Kind_Physical_Subtype_Definition ..
   --Iir_Kind_Floating_Subtype_Definition
   --Iir_Kind_Integer_Subtype_Definition
   --Iir_Kind_Enumeration_Subtype_Definition
   --Iir_Kind_Enumeration_Type_Definition
   --Iir_Kind_Integer_Type_Definition
   --Iir_Kind_Floating_Type_Definition
     Iir_Kind_Physical_Type_Definition;

   subtype Iir_Kinds_Range_Type_Definition is Iir_Kind range
     Iir_Kind_Physical_Subtype_Definition ..
   --Iir_Kind_Floating_Subtype_Definition
   --Iir_Kind_Integer_Subtype_Definition
   --Iir_Kind_Enumeration_Subtype_Definition
     Iir_Kind_Enumeration_Type_Definition;

   subtype Iir_Kinds_Discrete_Type_Definition is Iir_Kind range
     Iir_Kind_Integer_Subtype_Definition ..
   --Iir_Kind_Enumeration_Subtype_Definition
   --Iir_Kind_Enumeration_Type_Definition
     Iir_Kind_Integer_Type_Definition;

--     subtype Iir_Kinds_Discrete_Subtype_Definition is Iir_Kind range
--       Iir_Kind_Integer_Subtype_Definition ..
--       Iir_Kind_Enumeration_Subtype_Definition;

   subtype Iir_Kinds_Composite_Type_Definition is Iir_Kind range
     Iir_Kind_Record_Type_Definition ..
   --Iir_Kind_Array_Type_Definition
   --Iir_Kind_Array_Subtype_Definition
     Iir_Kind_Record_Subtype_Definition;

   subtype Iir_Kinds_Composite_Subtype_Definition is Iir_Kind range
     Iir_Kind_Array_Subtype_Definition ..
     Iir_Kind_Record_Subtype_Definition;

   subtype Iir_Kinds_Type_Declaration is Iir_Kind range
     Iir_Kind_Type_Declaration ..
   --Iir_Kind_Anonymous_Type_Declaration
     Iir_Kind_Subtype_Declaration;

   subtype Iir_Kinds_Nature_Definition is Iir_Kind range
     Iir_Kind_Scalar_Nature_Definition ..
   --Iir_Kind_Record_Nature_Definition
     Iir_Kind_Array_Nature_Definition;

   subtype Iir_Kinds_Subnature_Definition is Iir_Kind range
     Iir_Kind_Array_Subnature_Definition ..
     Iir_Kind_Array_Subnature_Definition;

   subtype Iir_Kinds_Nature_Indication is Iir_Kind range
     Iir_Kind_Scalar_Nature_Definition ..
   --Iir_Kind_Record_Nature_Definition
   --Iir_Kind_Array_Nature_Definition
     Iir_Kind_Array_Subnature_Definition;

   subtype Iir_Kinds_Nonoverloadable_Declaration is Iir_Kind range
     Iir_Kind_Type_Declaration ..
   --Iir_Kind_Anonymous_Type_Declaration
   --Iir_Kind_Subtype_Declaration
   --Iir_Kind_Nature_Declaration
   --Iir_Kind_Subnature_Declaration
   --Iir_Kind_Package_Header
   --Iir_Kind_Unit_Declaration
   --Iir_Kind_Library_Declaration
   --Iir_Kind_Component_Declaration
   --Iir_Kind_Attribute_Declaration
   --Iir_Kind_Group_Template_Declaration
   --Iir_Kind_Group_Declaration
   --Iir_Kind_Element_Declaration
     Iir_Kind_Nature_Element_Declaration;

   subtype Iir_Kinds_Monadic_Operator is Iir_Kind range
     Iir_Kind_Identity_Operator ..
   --Iir_Kind_Negation_Operator
   --Iir_Kind_Absolute_Operator
   --Iir_Kind_Not_Operator
   --Iir_Kind_Implicit_Condition_Operator
   --Iir_Kind_Condition_Operator
   --Iir_Kind_Reduction_And_Operator
   --Iir_Kind_Reduction_Or_Operator
   --Iir_Kind_Reduction_Nand_Operator
   --Iir_Kind_Reduction_Nor_Operator
   --Iir_Kind_Reduction_Xor_Operator
     Iir_Kind_Reduction_Xnor_Operator;

   subtype Iir_Kinds_Dyadic_Operator is Iir_Kind range
     Iir_Kind_And_Operator ..
   --Iir_Kind_Or_Operator
   --Iir_Kind_Nand_Operator
   --Iir_Kind_Nor_Operator
   --Iir_Kind_Xor_Operator
   --Iir_Kind_Xnor_Operator
   --Iir_Kind_Equality_Operator
   --Iir_Kind_Inequality_Operator
   --Iir_Kind_Less_Than_Operator
   --Iir_Kind_Less_Than_Or_Equal_Operator
   --Iir_Kind_Greater_Than_Operator
   --Iir_Kind_Greater_Than_Or_Equal_Operator
   --Iir_Kind_Match_Equality_Operator
   --Iir_Kind_Match_Inequality_Operator
   --Iir_Kind_Match_Less_Than_Operator
   --Iir_Kind_Match_Less_Than_Or_Equal_Operator
   --Iir_Kind_Match_Greater_Than_Operator
   --Iir_Kind_Match_Greater_Than_Or_Equal_Operator
   --Iir_Kind_Sll_Operator
   --Iir_Kind_Sla_Operator
   --Iir_Kind_Srl_Operator
   --Iir_Kind_Sra_Operator
   --Iir_Kind_Rol_Operator
   --Iir_Kind_Ror_Operator
   --Iir_Kind_Addition_Operator
   --Iir_Kind_Substraction_Operator
   --Iir_Kind_Concatenation_Operator
   --Iir_Kind_Multiplication_Operator
   --Iir_Kind_Division_Operator
   --Iir_Kind_Modulus_Operator
   --Iir_Kind_Remainder_Operator
     Iir_Kind_Exponentiation_Operator;

   subtype Iir_Kinds_Psl_Builtin is Iir_Kind range
     Iir_Kind_Psl_Prev ..
   --Iir_Kind_Psl_Stable
   --Iir_Kind_Psl_Rose
   --Iir_Kind_Psl_Fell
   --Iir_Kind_Psl_Onehot
     Iir_Kind_Psl_Onehot0;

   subtype Iir_Kinds_Functions_And_Literals is Iir_Kind range
     Iir_Kind_Enumeration_Literal ..
     Iir_Kind_Function_Declaration;

   subtype Iir_Kinds_Subprogram_Declaration is Iir_Kind range
     Iir_Kind_Function_Declaration ..
     Iir_Kind_Procedure_Declaration;

   subtype Iir_Kinds_Subprogram_Body is Iir_Kind range
     Iir_Kind_Function_Body ..
     Iir_Kind_Procedure_Body;

   subtype Iir_Kinds_Interface_Object_Declaration is Iir_Kind range
     Iir_Kind_Interface_Constant_Declaration ..
   --Iir_Kind_Interface_Variable_Declaration
   --Iir_Kind_Interface_Signal_Declaration
   --Iir_Kind_Interface_File_Declaration
     Iir_Kind_Interface_Quantity_Declaration;

   subtype Iir_Kinds_Interface_Subprogram_Declaration is Iir_Kind range
     Iir_Kind_Interface_Function_Declaration ..
     Iir_Kind_Interface_Procedure_Declaration;

   subtype Iir_Kinds_Interface_Declaration is Iir_Kind range
     Iir_Kind_Interface_Constant_Declaration ..
   --Iir_Kind_Interface_Variable_Declaration
   --Iir_Kind_Interface_Signal_Declaration
   --Iir_Kind_Interface_File_Declaration
   --Iir_Kind_Interface_Quantity_Declaration
   --Iir_Kind_Interface_Terminal_Declaration
   --Iir_Kind_Interface_Type_Declaration
   --Iir_Kind_Interface_Package_Declaration
   --Iir_Kind_Interface_Function_Declaration
     Iir_Kind_Interface_Procedure_Declaration;

   --  LRM-AMS17 6.4 Objects
   --  An object is a named entity that is a terminal or that contains (has)
   --  a value of a type.
   --
   --  Note: Object_Declaration does not include terminals.

   subtype Iir_Kinds_Object_Declaration is Iir_Kind range
     Iir_Kind_Object_Alias_Declaration ..
   --Iir_Kind_Free_Quantity_Declaration
   --Iir_Kind_Spectrum_Quantity_Declaration
   --Iir_Kind_Noise_Quantity_Declaration
   --Iir_Kind_Across_Quantity_Declaration
   --Iir_Kind_Through_Quantity_Declaration
   --Iir_Kind_File_Declaration
   --Iir_Kind_Guard_Signal_Declaration
   --Iir_Kind_Signal_Declaration
   --Iir_Kind_Variable_Declaration
   --Iir_Kind_Constant_Declaration
   --Iir_Kind_Iterator_Declaration
   --Iir_Kind_Interface_Constant_Declaration
   --Iir_Kind_Interface_Variable_Declaration
   --Iir_Kind_Interface_Signal_Declaration
   --Iir_Kind_Interface_File_Declaration
     Iir_Kind_Interface_Quantity_Declaration;

   subtype Iir_Kinds_Branch_Quantity_Declaration is Iir_Kind range
     Iir_Kind_Across_Quantity_Declaration ..
     Iir_Kind_Through_Quantity_Declaration;

   subtype Iir_Kinds_Source_Quantity_Declaration is Iir_Kind range
     Iir_Kind_Spectrum_Quantity_Declaration ..
     Iir_Kind_Noise_Quantity_Declaration;

   subtype Iir_Kinds_Quantity_Declaration is Iir_Kind range
     Iir_Kind_Free_Quantity_Declaration ..
   --Iir_Kind_Spectrum_Quantity_Declaration
   --Iir_Kind_Noise_Quantity_Declaration
   --Iir_Kind_Across_Quantity_Declaration
     Iir_Kind_Through_Quantity_Declaration;

   subtype Iir_Kinds_Non_Alias_Object_Declaration is Iir_Kind range
     Iir_Kind_File_Declaration ..
   --Iir_Kind_Guard_Signal_Declaration
   --Iir_Kind_Signal_Declaration
   --Iir_Kind_Variable_Declaration
   --Iir_Kind_Constant_Declaration
   --Iir_Kind_Iterator_Declaration
   --Iir_Kind_Interface_Constant_Declaration
   --Iir_Kind_Interface_Variable_Declaration
   --Iir_Kind_Interface_Signal_Declaration
     Iir_Kind_Interface_File_Declaration;

   --  Association elements for parameters.
   subtype Iir_Kinds_Association_Element_Parameters is Iir_Kind range
     Iir_Kind_Association_Element_By_Expression ..
   --Iir_Kind_Association_Element_By_Name
   --Iir_Kind_Association_Element_By_Individual
     Iir_Kind_Association_Element_Open;

   subtype Iir_Kinds_Association_Element_By_Actual is Iir_Kind range
     Iir_Kind_Association_Element_By_Expression ..
     Iir_Kind_Association_Element_By_Name;

   subtype Iir_Kinds_Association_Element is Iir_Kind range
     Iir_Kind_Association_Element_By_Expression ..
   --Iir_Kind_Association_Element_By_Name
   --Iir_Kind_Association_Element_By_Individual
   --Iir_Kind_Association_Element_Open
   --Iir_Kind_Association_Element_Package
   --Iir_Kind_Association_Element_Type
   --Iir_Kind_Association_Element_Subprogram
     Iir_Kind_Association_Element_Terminal;

   subtype Iir_Kinds_Choice is Iir_Kind range
     Iir_Kind_Choice_By_Range ..
   --Iir_Kind_Choice_By_Expression
   --Iir_Kind_Choice_By_Others
   --Iir_Kind_Choice_By_None
     Iir_Kind_Choice_By_Name;

   --  Choices in a case statement.
   subtype Iir_Kinds_Case_Choice is Iir_Kind range
     Iir_Kind_Choice_By_Range ..
   --Iir_Kind_Choice_By_Expression
     Iir_Kind_Choice_By_Others;

   --  Choices in array aggregate.
   subtype Iir_Kinds_Array_Choice is Iir_Kind range
     Iir_Kind_Choice_By_Range ..
   --Iir_Kind_Choice_By_Expression
   --Iir_Kind_Choice_By_Others
     Iir_Kind_Choice_By_None;

   --  Choices in record aggregate.
   subtype Iir_Kinds_Record_Choice is Iir_Kind range
     Iir_Kind_Choice_By_Others ..
   --Iir_Kind_Choice_By_None
     Iir_Kind_Choice_By_Name;

   subtype Iir_Kinds_Entity_Aspect is Iir_Kind range
     Iir_Kind_Entity_Aspect_Entity ..
   --Iir_Kind_Entity_Aspect_Configuration
     Iir_Kind_Entity_Aspect_Open;

   subtype Iir_Kinds_Denoting_Name is Iir_Kind range
     Iir_Kind_Character_Literal ..
   --Iir_Kind_Simple_Name
   --Iir_Kind_Selected_Name
   --Iir_Kind_Operator_Symbol
     Iir_Kind_Reference_Name;

   subtype Iir_Kinds_Denoting_And_External_Name is Iir_Kind range
     Iir_Kind_Character_Literal ..
   --Iir_Kind_Simple_Name
   --Iir_Kind_Selected_Name
   --Iir_Kind_Operator_Symbol
   --Iir_Kind_Reference_Name
   --Iir_Kind_External_Constant_Name
   --Iir_Kind_External_Signal_Name
     Iir_Kind_External_Variable_Name;

   subtype Iir_Kinds_Name is Iir_Kind range
     Iir_Kind_Character_Literal ..
   --Iir_Kind_Simple_Name
   --Iir_Kind_Selected_Name
   --Iir_Kind_Operator_Symbol
   --Iir_Kind_Reference_Name
   --Iir_Kind_External_Constant_Name
   --Iir_Kind_External_Signal_Name
   --Iir_Kind_External_Variable_Name
   --Iir_Kind_Selected_By_All_Name
     Iir_Kind_Parenthesis_Name;

   subtype Iir_Kinds_Dereference is Iir_Kind range
     Iir_Kind_Dereference ..
     Iir_Kind_Implicit_Dereference;

   subtype Iir_Kinds_External_Name is Iir_Kind range
     Iir_Kind_External_Constant_Name ..
   --Iir_Kind_External_Signal_Name
     Iir_Kind_External_Variable_Name;

   --  Any attribute that is an expression.
   subtype Iir_Kinds_Expression_Attribute is Iir_Kind range
     Iir_Kind_Left_Type_Attribute ..
   --Iir_Kind_Right_Type_Attribute
   --Iir_Kind_High_Type_Attribute
   --Iir_Kind_Low_Type_Attribute
   --Iir_Kind_Ascending_Type_Attribute
   --Iir_Kind_Image_Attribute
   --Iir_Kind_Value_Attribute
   --Iir_Kind_Pos_Attribute
   --Iir_Kind_Val_Attribute
   --Iir_Kind_Succ_Attribute
   --Iir_Kind_Pred_Attribute
   --Iir_Kind_Leftof_Attribute
   --Iir_Kind_Rightof_Attribute
   --Iir_Kind_Signal_Slew_Attribute
   --Iir_Kind_Quantity_Slew_Attribute
   --Iir_Kind_Ramp_Attribute
   --Iir_Kind_Zoh_Attribute
   --Iir_Kind_Ltf_Attribute
   --Iir_Kind_Ztf_Attribute
   --Iir_Kind_Dot_Attribute
   --Iir_Kind_Integ_Attribute
   --Iir_Kind_Above_Attribute
   --Iir_Kind_Quantity_Delayed_Attribute
   --Iir_Kind_Delayed_Attribute
   --Iir_Kind_Stable_Attribute
   --Iir_Kind_Quiet_Attribute
   --Iir_Kind_Transaction_Attribute
   --Iir_Kind_Event_Attribute
   --Iir_Kind_Active_Attribute
   --Iir_Kind_Last_Event_Attribute
   --Iir_Kind_Last_Active_Attribute
   --Iir_Kind_Last_Value_Attribute
   --Iir_Kind_Driving_Attribute
   --Iir_Kind_Driving_Value_Attribute
   --Iir_Kind_Behavior_Attribute
   --Iir_Kind_Structure_Attribute
   --Iir_Kind_Simple_Name_Attribute
   --Iir_Kind_Instance_Name_Attribute
   --Iir_Kind_Path_Name_Attribute
   --Iir_Kind_Left_Array_Attribute
   --Iir_Kind_Right_Array_Attribute
   --Iir_Kind_High_Array_Attribute
   --Iir_Kind_Low_Array_Attribute
   --Iir_Kind_Length_Array_Attribute
     Iir_Kind_Ascending_Array_Attribute;

   --  All the attributes.
   subtype Iir_Kinds_Attribute is Iir_Kind range
     Iir_Kind_Base_Attribute ..
     Iir_Kind_Reverse_Range_Array_Attribute;

   --  Attributes of scalar types.
   subtype Iir_Kinds_Type_Attribute is Iir_Kind range
     Iir_Kind_Left_Type_Attribute ..
   --Iir_Kind_Right_Type_Attribute
   --Iir_Kind_High_Type_Attribute
   --Iir_Kind_Low_Type_Attribute
     Iir_Kind_Ascending_Type_Attribute;

   --  Attributes whose result is a type.
   subtype Iir_Kinds_Subtype_Attribute is Iir_Kind range
     Iir_Kind_Base_Attribute ..
   --Iir_Kind_Subtype_Attribute
     Iir_Kind_Element_Attribute;

   subtype Iir_Kinds_Scalar_Type_Attribute is Iir_Kind range
     Iir_Kind_Pos_Attribute ..
   --Iir_Kind_Val_Attribute
   --Iir_Kind_Succ_Attribute
   --Iir_Kind_Pred_Attribute
   --Iir_Kind_Leftof_Attribute
     Iir_Kind_Rightof_Attribute;

   subtype Iir_Kinds_Array_Attribute is Iir_Kind range
     Iir_Kind_Left_Array_Attribute ..
   --Iir_Kind_Right_Array_Attribute
   --Iir_Kind_High_Array_Attribute
   --Iir_Kind_Low_Array_Attribute
   --Iir_Kind_Length_Array_Attribute
   --Iir_Kind_Ascending_Array_Attribute
   --Iir_Kind_Range_Array_Attribute
     Iir_Kind_Reverse_Range_Array_Attribute;

   subtype Iir_Kinds_Range_Attribute is Iir_Kind range
     Iir_Kind_Range_Array_Attribute ..
     Iir_Kind_Reverse_Range_Array_Attribute;

   subtype Iir_Kinds_Signal_Attribute is Iir_Kind range
     Iir_Kind_Delayed_Attribute ..
   --Iir_Kind_Stable_Attribute
   --Iir_Kind_Quiet_Attribute
     Iir_Kind_Transaction_Attribute;

   subtype Iir_Kinds_Signal_Value_Attribute is Iir_Kind range
     Iir_Kind_Event_Attribute ..
   --Iir_Kind_Active_Attribute
   --Iir_Kind_Last_Event_Attribute
   --Iir_Kind_Last_Active_Attribute
   --Iir_Kind_Last_Value_Attribute
   --Iir_Kind_Driving_Attribute
     Iir_Kind_Driving_Value_Attribute;

   subtype Iir_Kinds_Name_Attribute is Iir_Kind range
     Iir_Kind_Simple_Name_Attribute ..
   --Iir_Kind_Instance_Name_Attribute
     Iir_Kind_Path_Name_Attribute;

   subtype Iir_Kinds_Concurrent_Statement is Iir_Kind range
     Iir_Kind_Sensitized_Process_Statement ..
   --Iir_Kind_Process_Statement
   --Iir_Kind_Concurrent_Simple_Signal_Assignment
   --Iir_Kind_Concurrent_Conditional_Signal_Assignment
   --Iir_Kind_Concurrent_Selected_Signal_Assignment
   --Iir_Kind_Concurrent_Assertion_Statement
   --Iir_Kind_Concurrent_Procedure_Call_Statement
   --Iir_Kind_Concurrent_Break_Statement
   --Iir_Kind_Psl_Assert_Directive
   --Iir_Kind_Psl_Assume_Directive
   --Iir_Kind_Psl_Cover_Directive
   --Iir_Kind_Psl_Restrict_Directive
   --Iir_Kind_Block_Statement
   --Iir_Kind_If_Generate_Statement
   --Iir_Kind_Case_Generate_Statement
   --Iir_Kind_For_Generate_Statement
     Iir_Kind_Component_Instantiation_Statement;

   --  Nice classification from AMS vhdl.
   subtype Iir_Kinds_Structural_Statement is Iir_Kind range
     Iir_Kind_Block_Statement ..
   --Iir_Kind_If_Generate_Statement
   --Iir_Kind_Case_Generate_Statement
   --Iir_Kind_For_Generate_Statement
     Iir_Kind_Component_Instantiation_Statement;

   subtype Iir_Kinds_Simple_Concurrent_Statement is Iir_Kind range
     Iir_Kind_Sensitized_Process_Statement ..
   --Iir_Kind_Process_Statement
   --Iir_Kind_Concurrent_Simple_Signal_Assignment
   --Iir_Kind_Concurrent_Conditional_Signal_Assignment
   --Iir_Kind_Concurrent_Selected_Signal_Assignment
   --Iir_Kind_Concurrent_Assertion_Statement
   --Iir_Kind_Concurrent_Procedure_Call_Statement
   --Iir_Kind_Concurrent_Break_Statement
   --Iir_Kind_Psl_Assert_Directive
   --Iir_Kind_Psl_Assume_Directive
   --Iir_Kind_Psl_Cover_Directive
     Iir_Kind_Psl_Restrict_Directive;

   subtype Iir_Kinds_Process_Statement is Iir_Kind range
     Iir_Kind_Sensitized_Process_Statement ..
     Iir_Kind_Process_Statement;

   subtype Iir_Kinds_Concurrent_Signal_Assignment is Iir_Kind range
     Iir_Kind_Concurrent_Simple_Signal_Assignment ..
   --Iir_Kind_Concurrent_Conditional_Signal_Assignment
     Iir_Kind_Concurrent_Selected_Signal_Assignment;

   subtype Iir_Kinds_Psl_Property_Directive is Iir_Kind range
     Iir_Kind_Psl_Assert_Directive ..
     Iir_Kind_Psl_Assume_Directive;

   subtype Iir_Kinds_Psl_Sequence_Directive is Iir_Kind range
     Iir_Kind_Psl_Cover_Directive ..
     Iir_Kind_Psl_Restrict_Directive;

   subtype Iir_Kinds_Psl_Directive is Iir_Kind range
     Iir_Kind_Psl_Assert_Directive ..
   --Iir_Kind_Psl_Assume_Directive
   --Iir_Kind_Psl_Cover_Directive
     Iir_Kind_Psl_Restrict_Directive;

   subtype Iir_Kinds_Generate_Statement is Iir_Kind range
     Iir_Kind_If_Generate_Statement ..
   --Iir_Kind_Case_Generate_Statement
     Iir_Kind_For_Generate_Statement;

   subtype Iir_Kinds_If_Case_Generate_Statement is Iir_Kind range
     Iir_Kind_If_Generate_Statement ..
     Iir_Kind_Case_Generate_Statement;

   subtype Iir_Kinds_Simultaneous_Statement is Iir_Kind range
     Iir_Kind_Simple_Simultaneous_Statement ..
   --Iir_Kind_Simultaneous_Null_Statement
   --Iir_Kind_Simultaneous_Procedural_Statement
   --Iir_Kind_Simultaneous_Case_Statement
     Iir_Kind_Simultaneous_If_Statement;

   subtype Iir_Kinds_Sequential_Statement is Iir_Kind range
     Iir_Kind_Simple_Signal_Assignment_Statement ..
   --Iir_Kind_Conditional_Signal_Assignment_Statement
   --Iir_Kind_Selected_Waveform_Assignment_Statement
   --Iir_Kind_Signal_Force_Assignment_Statement
   --Iir_Kind_Signal_Release_Assignment_Statement
   --Iir_Kind_Null_Statement
   --Iir_Kind_Assertion_Statement
   --Iir_Kind_Report_Statement
   --Iir_Kind_Wait_Statement
   --Iir_Kind_Variable_Assignment_Statement
   --Iir_Kind_Conditional_Variable_Assignment_Statement
   --Iir_Kind_Return_Statement
   --Iir_Kind_For_Loop_Statement
   --Iir_Kind_While_Loop_Statement
   --Iir_Kind_Next_Statement
   --Iir_Kind_Exit_Statement
   --Iir_Kind_Case_Statement
   --Iir_Kind_Procedure_Call_Statement
   --Iir_Kind_Break_Statement
     Iir_Kind_If_Statement;

   --  All sequential statements + suspend_state_statement.
   subtype Iir_Kinds_Sequential_Statement_Ext is Iir_Kind range
     Iir_Kind_Simple_Signal_Assignment_Statement ..
   --Iir_Kind_Conditional_Signal_Assignment_Statement
   --Iir_Kind_Selected_Waveform_Assignment_Statement
   --Iir_Kind_Signal_Force_Assignment_Statement
   --Iir_Kind_Signal_Release_Assignment_Statement
   --Iir_Kind_Null_Statement
   --Iir_Kind_Assertion_Statement
   --Iir_Kind_Report_Statement
   --Iir_Kind_Wait_Statement
   --Iir_Kind_Variable_Assignment_Statement
   --Iir_Kind_Conditional_Variable_Assignment_Statement
   --Iir_Kind_Return_Statement
   --Iir_Kind_For_Loop_Statement
   --Iir_Kind_While_Loop_Statement
   --Iir_Kind_Next_Statement
   --Iir_Kind_Exit_Statement
   --Iir_Kind_Case_Statement
   --Iir_Kind_Procedure_Call_Statement
   --Iir_Kind_Break_Statement
   --Iir_Kind_If_Statement
     Iir_Kind_Suspend_State_Statement;

   subtype Iir_Kinds_Next_Exit_Statement is Iir_Kind range
     Iir_Kind_Next_Statement ..
     Iir_Kind_Exit_Statement;

   subtype Iir_Kinds_Variable_Assignment_Statement is Iir_Kind range
     Iir_Kind_Variable_Assignment_Statement ..
     Iir_Kind_Conditional_Variable_Assignment_Statement;

   subtype Iir_Kinds_Allocator is Iir_Kind range
     Iir_Kind_Allocator_By_Expression ..
     Iir_Kind_Allocator_By_Subtype;

   subtype Iir_Kinds_Clause is Iir_Kind range
     Iir_Kind_Library_Clause ..
   --Iir_Kind_Use_Clause
     Iir_Kind_Context_Reference;

   subtype Iir_Kinds_Specification is Iir_Kind range
     Iir_Kind_Attribute_Specification ..
   --Iir_Kind_Disconnection_Specification
   --Iir_Kind_Step_Limit_Specification
     Iir_Kind_Configuration_Specification;

   --  Nodes and lists.

   subtype Iir is Vhdl.Nodes_Priv.Node_Type;
   subtype Node is Vhdl.Nodes_Priv.Node_Type;

   Null_Iir : constant Iir := Vhdl.Nodes_Priv.Null_Node;
   Null_Node : constant Node := Vhdl.Nodes_Priv.Null_Node;

   --  Return True iff Node is null / not set.
   function Is_Null (Node : Iir) return Boolean;
   pragma Inline (Is_Null);

   --  Return True iff Node is not null / set.
   function Is_Valid (Node : Iir) return Boolean;
   pragma Inline (Is_Valid);

   function "=" (L, R : Iir) return Boolean renames Vhdl.Nodes_Priv."=";

   --  Get the last node allocated.
   function Get_Last_Node return Iir;
   pragma Inline (Get_Last_Node);

   subtype Iir_List is Lists.List_Type;
   subtype Node_List is Lists.List_Type;
   Null_Iir_List : constant Iir_List := Lists.Null_List;
   Iir_List_All : constant Iir_List := Lists.List_All;

   subtype List_Iterator is Lists.Iterator;
   function Is_Null_List (Node : Iir_List) return Boolean;
   pragma Inline (Is_Null_List);

   function Create_Iir_List return Iir_List
     renames Lists.Create_List;
   procedure Append_Element (L : Iir_List; E : Iir)
     renames Lists.Append_Element;
   procedure Add_Element (L : Iir_List; E : Iir)
     renames Lists.Add_Element;
   procedure Destroy_Iir_List (L : in out Iir_List)
     renames Lists.Destroy_List;
   function Get_Nbr_Elements (L : Iir_List) return Natural
     renames Lists.Get_Nbr_Elements;
   function Get_First_Element (L : Iir_List) return Iir
     renames Lists.Get_First_Element;
   function Is_Empty (L : Iir_List) return Boolean
     renames Lists.Is_Empty;

   function List_Iterate (List : Iir_List) return List_Iterator
     renames Lists.Iterate;
   function List_Iterate_Safe (List : Iir_List) return List_Iterator
     renames Lists.Iterate_Safe;
   function Is_Valid (It : List_Iterator) return Boolean
     renames Lists.Is_Valid;
   procedure Next (It : in out List_Iterator)
     renames Lists.Next;
   function Get_Element (It : List_Iterator) return Iir
     renames Lists.Get_Element;
   procedure Set_Element (It : List_Iterator; El : Iir)
     renames Lists.Set_Element;

   function "=" (L, R : Iir_List) return Boolean renames Lists."=";

   subtype Iir_Flist is Flists.Flist_Type;
   subtype Node_Flist is Flists.Flist_Type;
   Null_Iir_Flist   : constant Iir_Flist := Flists.Null_Flist;
   Iir_Flist_Others : constant Iir_Flist := Flists.Flist_Others;
   Iir_Flist_All    : constant Iir_Flist := Flists.Flist_All;

   subtype Iir_Flists_All_Others is Iir_Flist
     range Iir_Flist_Others .. Iir_Flist_All;

   Flist_First : constant Natural := Flists.Ffirst;
   function Flist_Last (Flist : Iir_Flist) return Natural
     renames Flists.Flast;
   function Create_Iir_Flist (Len : Natural) return Iir_Flist
     renames Flists.Create_Flist;
   function Get_Nth_Element (Flist : Iir_Flist; N : Natural) return Iir
     renames Flists.Get_Nth_Element;
   procedure Set_Nth_Element (Flist : Iir_Flist; N : Natural; El : Iir)
     renames Flists.Set_Nth_Element;
   function Get_Nbr_Elements (Flist : Iir_Flist) return Natural
     renames Flists.Length;
   procedure Destroy_Iir_Flist (Flist : in out Iir_Flist)
     renames Flists.Destroy_Flist;
   function "=" (L, R : Iir_Flist) return Boolean renames Flists."=";

   -- This is used only for lists.
   type Iir_Array is array (Natural range <>) of Iir;
   type Iir_Array_Acc is access Iir_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Iir_Array, Name => Iir_Array_Acc);

   --  Date State.
   --  This indicates the origin of the data information.
   --  This also indicates the state of the unit (loaded or not).
   type Date_State_Type is
     (
      --  The unit is not yet in the library.
      Date_Extern,

      --  The unit is not loaded (still on the disk).
      --  All the information come from the library file.
      Date_Disk,

      --  The unit has been parsed, but not analyzed.
      --  Only the date information come from the library.
      Date_Parse,

      --  The unit has been analyzed.
      Date_Analyze
     );

   --  A date is used for analysis order.  All design units from a library
   --  are ordered according to the date.
   type Date_Type is new Nat32;

   --  The unit is obsoleted (ie replaced) by a more recently analyzed design
   --  unit.
   --  If another design unit depends (directly or not) on an obsoleted design
   --  unit, it is also obsolete, and cannot be defined.
   Date_Obsolete      : constant Date_Type := 0;
   --  A unit with the same name (could also be the same unit) is being
   --  analyzed.  Used to detect circular dependencies.
   Date_Replacing     : constant Date_Type := 1;
   --  The unit was not analyzed.
   Date_Parsed        : constant Date_Type := 4;
   --  The unit is being analyzed.
   Date_Analyzing     : constant Date_Type := 5;
   --  This unit has just been analyzed and should be marked at the last
   --  analyzed unit.
   Date_Analyzed      : constant Date_Type := 6;
   --  Used only for default configuration.
   --  Such units are always up-to-date.
   Date_Uptodate      : constant Date_Type := 7;
   subtype Date_Valid is Date_Type range 10 .. Date_Type'Last;

   --  Predefined depth values.
   --  Depth of a subprogram not declared in another subprogram.
   Iir_Depth_Top : constant Iir_Int32 := 0;
   --  Purity depth of a pure subprogram.
   Iir_Depth_Pure : constant Iir_Int32 := Iir_Int32'Last;
   --  Purity depth of an impure subprogram.
   Iir_Depth_Impure : constant Iir_Int32 := -1;

   type Number_Base_Type is
     (
      Base_None,
      Base_2,
      Base_8,
      Base_10,
      Base_16
     );

   -- design file
   subtype Iir_Design_File is Iir;

   subtype Iir_Design_Unit is Iir;

   subtype Iir_Library_Clause is Iir;

   -- Literals.
   --subtype Iir_Text_Literal is Iir;

   subtype Iir_Character_Literal is Iir;

   subtype Iir_Integer_Literal is Iir;

   subtype Iir_Floating_Point_Literal is Iir;

   subtype Iir_Null_Literal is Iir;

   subtype Iir_Physical_Int_Literal is Iir;

   subtype Iir_Physical_Fp_Literal is Iir;

   subtype Iir_Enumeration_Literal is Iir;

   subtype Iir_Simple_Aggregate is Iir;

   subtype Iir_Enumeration_Type_Definition is Iir;

   subtype Iir_Enumeration_Subtype_Definition is Iir;

   subtype Iir_Range_Expression is Iir;

   subtype Iir_Integer_Subtype_Definition is Iir;

   subtype Iir_Integer_Type_Definition is Iir;

   subtype Iir_Floating_Subtype_Definition is Iir;

   subtype Iir_Floating_Type_Definition is Iir;

   subtype Iir_Array_Type_Definition is Iir;

   subtype Iir_Record_Type_Definition is Iir;

   subtype Iir_Protected_Type_Declaration is Iir;

   subtype Iir_Protected_Type_Body is Iir;

   subtype Iir_Subtype_Definition is Iir;

   subtype Iir_Array_Subtype_Definition is Iir;

   subtype Iir_Physical_Type_Definition is Iir;

   subtype Iir_Physical_Subtype_Definition is Iir;

   subtype Iir_Access_Type_Definition is Iir;

   subtype Iir_Access_Subtype_Definition is Iir;

   subtype Iir_File_Type_Definition is Iir;

   subtype Iir_Waveform_Element is Iir;

   subtype Iir_Conditional_Waveform is Iir;

   subtype Iir_Association_Element_By_Expression is Iir;

   subtype Iir_Association_Element_By_Individual is Iir;

   subtype Iir_Association_Element_Open is Iir;

   subtype Iir_Signature is Iir;

   subtype Iir_Unit_Declaration is Iir;

   subtype Iir_Entity_Aspect_Entity is Iir;

   subtype Iir_Entity_Aspect_Configuration is Iir;

   subtype Iir_Entity_Aspect_Open is Iir;

   subtype Iir_Block_Configuration is Iir;

   subtype Iir_Block_Header is Iir;

   subtype Iir_Component_Configuration is Iir;

   subtype Iir_Binding_Indication is Iir;

   subtype Iir_Entity_Class is Iir;

   subtype Iir_Attribute_Specification is Iir;

   subtype Iir_Attribute_Value is Iir;

   subtype Iir_Selected_Element is Iir;

   subtype Iir_Implicit_Dereference is Iir;

   subtype Iir_Aggregate_Info is Iir;

   subtype Iir_Procedure_Call is Iir;

   subtype Iir_Disconnection_Specification is Iir;

   -- Lists.

   subtype Iir_Design_Unit_List is Iir_List;

   subtype Iir_Attribute_Value_Chain is Iir_List;

   subtype Iir_Overload_List is Iir;

   subtype Iir_Callees_List is Iir_List;

   -- Declaration and children.
   subtype Iir_Entity_Declaration is Iir;

   subtype Iir_Architecture_Body is Iir;

   subtype Iir_Interface_Signal_Declaration is Iir;

   subtype Iir_Configuration_Declaration is Iir;

   subtype Iir_Type_Declaration is Iir;

   subtype Iir_Anonymous_Type_Declaration is Iir;

   subtype Iir_Subtype_Declaration is Iir;

   subtype Iir_Package_Declaration is Iir;
   subtype Iir_Package_Body is Iir;

   subtype Iir_Library_Declaration is Iir;

   subtype Iir_Function_Declaration is Iir;

   subtype Iir_Function_Body is Iir;

   subtype Iir_Procedure_Declaration is Iir;

   subtype Iir_Procedure_Body is Iir;

   subtype Iir_Use_Clause is Iir;

   subtype Iir_Constant_Declaration is Iir;

   subtype Iir_Iterator_Declaration is Iir;

   subtype Iir_Interface_Constant_Declaration is Iir;

   subtype Iir_Interface_Variable_Declaration is Iir;

   subtype Iir_Interface_File_Declaration is Iir;

   subtype Iir_Guard_Signal_Declaration is Iir;

   subtype Iir_Signal_Declaration is Iir;

   subtype Iir_Variable_Declaration is Iir;

   subtype Iir_Component_Declaration is Iir;

   subtype Iir_Element_Declaration is Iir;

   subtype Iir_Object_Alias_Declaration is Iir;

   subtype Iir_Non_Object_Alias_Declaration is Iir;

   subtype Iir_Interface_Declaration is Iir;

   subtype Iir_Configuration_Specification is Iir;

   subtype Iir_File_Declaration is Iir;

   subtype Iir_Attribute_Declaration is Iir;

   subtype Iir_Group_Template_Declaration is Iir;

   subtype Iir_Group_Declaration is Iir;

   -- concurrent_statement and children.
   subtype Iir_Concurrent_Statement is Iir;

   subtype Iir_Concurrent_Conditional_Signal_Assignment is Iir;

   subtype Iir_Sensitized_Process_Statement is Iir;

   subtype Iir_Process_Statement is Iir;

   subtype Iir_Component_Instantiation_Statement is Iir;

   subtype Iir_Block_Statement is Iir;

   subtype Iir_Generate_Statement is Iir;

   -- sequential statements.
   subtype Iir_If_Statement is Iir;

   subtype Iir_Elsif is Iir;

   subtype Iir_For_Loop_Statement is Iir;

   subtype Iir_While_Loop_Statement is Iir;

   subtype Iir_Exit_Statement is Iir;
   subtype Iir_Next_Statement is Iir;

   subtype Iir_Variable_Assignment_Statement is Iir;

   subtype Iir_Signal_Assignment_Statement is Iir;

   subtype Iir_Assertion_Statement is Iir;

   subtype Iir_Report_Statement is Iir;

   subtype Iir_Wait_Statement is Iir;

   subtype Iir_Return_Statement is Iir;

   subtype Iir_Case_Statement is Iir;

   subtype Iir_Procedure_Call_Statement is Iir;

   -- expression and children.
   subtype Iir_Expression is Iir;

   subtype Iir_Function_Call is Iir;

   subtype Iir_Aggregate is Iir;

   subtype Iir_Qualified_Expression is Iir;

   subtype Iir_Type_Conversion is Iir;

   subtype Iir_Allocator_By_Expression is Iir;

   subtype Iir_Allocator_By_Subtype is Iir;

   -- names.
   subtype Iir_Simple_Name is Iir;

   subtype Iir_Slice_Name is Iir;

   subtype Iir_Selected_Name is Iir;

   subtype Iir_Selected_By_All_Name is Iir;

   subtype Iir_Indexed_Name is Iir;

   subtype Iir_Parenthesis_Name is Iir;

   -- attributes.
   subtype Iir_Attribute_Name is Iir;

   -- General methods.

   -- Get the kind of the iir.
   function Get_Kind (N : Iir) return Iir_Kind;
   pragma Inline (Get_Kind);

   function Next_Node (N : Iir) return Iir;

   --  Create a new IIR of kind NEW_KIND, and copy fields from SRC to this
   --  iir.  Src fields are cleaned.
   --function Clone_Iir (Src: Iir; New_Kind : Iir_Kind) return Iir;

   procedure Set_Location (N : Iir; Location : Location_Type);
   function Get_Location (N : Iir) return Location_Type;

   procedure Location_Copy (Target : Iir; Src : Iir);

   function Create_Iir (Kind : Iir_Kind) return Iir;
   function Create_Iir_Error return Iir;
   procedure Free_Iir (Target : Iir);

   --  Hooks called when a node is free.
   --  Note: The register procedure must be called only during elaboration, so
   --   the set of hooks is defined forever.
   type Free_Iir_Hook is access procedure (N : Iir);
   procedure Register_Free_Hook (Hook : Free_Iir_Hook);

   --  Initialize.
   procedure Initialize;

   --  Free all the memory.
   procedure Finalize;

   --  Disp statistics about node usage.
   procedure Disp_Stats;

   --  Design units contained in a design file.
   --  Field: Field5 Chain
   function Get_First_Design_Unit (Design : Iir) return Iir;
   procedure Set_First_Design_Unit (Design : Iir; Chain : Iir);

   --  Field: Field6 Ref
   function Get_Last_Design_Unit (Design : Iir) return Iir;
   procedure Set_Last_Design_Unit (Design : Iir; Chain : Iir);

   --  Library declaration of a library clause.  This is Forward_Ref as the
   --  dependency of the unit on the library is not tracked.
   --  Field: Field1 Forward_Ref
   function Get_Library_Declaration (Design : Iir) return Iir;
   procedure Set_Library_Declaration (Design : Iir; Library : Iir);

   -- File time stamp is the system time of the file last modification.
   --  Field: Field4 (uc)
   function Get_File_Checksum (Design : Iir) return File_Checksum_Id;
   procedure Set_File_Checksum (Design : Iir; Checksum : File_Checksum_Id);

   -- Time stamp of the last analysis system time.
   --  Field: Field3 (uc)
   function Get_Analysis_Time_Stamp (Design : Iir) return Time_Stamp_Id;
   procedure Set_Analysis_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id);

   --  Field: Field7 (uc)
   function Get_Design_File_Source (Design : Iir) return Source_File_Entry;
   procedure Set_Design_File_Source (Design : Iir; Sfe : Source_File_Entry);

   --  The library which FILE belongs to.
   --  Field: Field0 Ref
   function Get_Library (File : Iir_Design_File) return Iir;
   procedure Set_Library (File : Iir_Design_File; Lib : Iir);

   --  List of files which this design file depends on.
   --  Field: Field1 (uc)
   function Get_File_Dependence_List (File : Iir_Design_File) return Iir_List;
   procedure Set_File_Dependence_List (File : Iir_Design_File; Lst : Iir_List);

   --  Identifier for the design file file name.
   --  Field: Field12 (pos)
   function Get_Design_File_Filename (File : Iir_Design_File) return Name_Id;
   procedure Set_Design_File_Filename (File : Iir_Design_File; Name : Name_Id);

   --  Directory of a design file.
   --  Field: Field11 (pos)
   function Get_Design_File_Directory (File : Iir_Design_File) return Name_Id;
   procedure Set_Design_File_Directory (File : Iir_Design_File; Dir : Name_Id);

   --  The parent of a design unit is a design file.
   --  Field: Field0 Ref
   function Get_Design_File (Unit : Iir_Design_Unit) return Iir;
   procedure Set_Design_File (Unit : Iir_Design_Unit; File : Iir);

   --  Design files of a library.
   --  Field: Field1 Chain
   function Get_Design_File_Chain (Library : Iir) return Iir;
   procedure Set_Design_File_Chain (Library : Iir; Chain : Iir);

   --  System directory where the library is stored.
   --  Field: Field5 (pos)
   function Get_Library_Directory (Library : Iir) return Name_Id;
   procedure Set_Library_Directory (Library : Iir; Dir : Name_Id);

   -- Symbolic date, used to order design units in a library.
   --  Field: Field4 (pos)
   function Get_Date (Target : Iir) return Date_Type;
   procedure Set_Date (Target : Iir; Date : Date_Type);

   --  Chain of context clauses.
   --  Field: Field1 Chain
   function Get_Context_Items (Design_Unit : Iir) return Iir;
   procedure Set_Context_Items (Design_Unit : Iir; Items_Chain : Iir);

   --  List of design units on which the design unit depends. There is an
   --  exception: the architecture of an entity aspect (of a component
   --  instantiation) may not have been analyzed.  The Entity_Aspect_Entity
   --  is added to this list (instead of the non-existing design unit).
   --  Field: Field8 Of_Ref (uc)
   function Get_Dependence_List (Unit : Iir) return Iir_List;
   procedure Set_Dependence_List (Unit : Iir; List : Iir_List);

   --  List of functions or sensitized processes whose analysis checks are not
   --  complete.
   --  These elements have direct or indirect calls to procedure whose body is
   --  not yet analyzed.  Therefore, purity or wait checks are not complete.
   --  Field: Field9 Of_Ref (uc)
   function Get_Analysis_Checks_List (Unit : Iir) return Iir_List;
   procedure Set_Analysis_Checks_List (Unit : Iir; List : Iir_List);

   --  Whether the unit is on disk, parsed or analyzed.
   --  Field: State1 (pos)
   function Get_Date_State (Unit : Iir_Design_Unit) return Date_State_Type;
   procedure Set_Date_State (Unit : Iir_Design_Unit; State : Date_State_Type);

   --  If TRUE, the target of the signal assignment is guarded.
   --  If FALSE, the target is not guarded.
   --  This is determined during sem by examining the declaration(s) of the
   --  target (there may be several declarations in the case of a aggregate
   --  target).
   --  If UNKNOWN, this is not determined at compile time but at run-time.
   --  This is the case for formal signal interfaces of subprograms.
   --  Field: State1 (pos)
   function Get_Guarded_Target_State (Stmt : Iir) return Tri_State_Type;
   procedure Set_Guarded_Target_State (Stmt : Iir; State : Tri_State_Type);

   --  Library unit of a design unit.
   --  Field: Field7
   function Get_Library_Unit (Design_Unit : Iir_Design_Unit) return Iir;
   procedure Set_Library_Unit (Design_Unit : Iir_Design_Unit; Lib_Unit : Iir);
   pragma Inline (Get_Library_Unit);

   --  Every design unit is put in an hash table to find quickly found by its
   --  name.  This field is a single chain for collisions.
   --  Field: Field5 Forward_Ref
   function Get_Hash_Chain (Design_Unit : Iir_Design_Unit) return Iir;
   procedure Set_Hash_Chain (Design_Unit : Iir_Design_Unit; Chain : Iir);

   -- Set the line and the offset in the line, only for the library manager.
   -- This is valid until the file is really loaded in memory.  On loading,
   -- location will contain all this information.
   --  Field: Field10 (uc)
   function Get_Design_Unit_Source_Pos (Design_Unit : Iir) return Source_Ptr;
   procedure Set_Design_Unit_Source_Pos (Design_Unit : Iir; Pos : Source_Ptr);

   --  Field: Field11 (uc)
   function Get_Design_Unit_Source_Line (Design_Unit : Iir) return Int32;
   procedure Set_Design_Unit_Source_Line (Design_Unit : Iir; Line : Int32);

   --  Field: Field12 (uc)
   function Get_Design_Unit_Source_Col (Design_Unit : Iir) return Int32;
   procedure Set_Design_Unit_Source_Col (Design_Unit : Iir; Line : Int32);

   --  literals.

   --  Value of an integer/physical literal.
   --  Field: Field4,Field5 (grp)
   function Get_Value (Lit : Iir) return Int64;
   procedure Set_Value (Lit : Iir; Val : Int64);

   --  Position (same as lit_type'pos) of an enumeration literal.
   --  Field: Field5 (pos)
   function Get_Enum_Pos (Lit : Iir) return Iir_Int32;
   procedure Set_Enum_Pos (Lit : Iir; Val : Iir_Int32);

   --  Field: Field4
   function Get_Physical_Literal (Unit : Iir) return Iir;
   procedure Set_Physical_Literal (Unit : Iir; Lit : Iir);

   --  Value of a floating point literal.
   --  Field: Field4,Field5 (grp)
   function Get_Fp_Value (Lit : Iir) return Fp64;
   procedure Set_Fp_Value (Lit : Iir; Val : Fp64);

   --  List of elements of a simple aggregate.
   --  Field: Field4 Ref (uc)
   function Get_Simple_Aggregate_List (Target : Iir) return Iir_Flist;
   procedure Set_Simple_Aggregate_List (Target : Iir; List : Iir_Flist);

   --  For a string literal: the string identifier.
   --  Field: Field5 (uc)
   function Get_String8_Id (Lit : Iir) return String8_Id;
   procedure Set_String8_Id (Lit : Iir; Id : String8_Id);

   --  For a string literal: the string length.
   --  Field: Field4 (uc)
   function Get_String_Length (Lit : Iir) return Int32;
   procedure Set_String_Length (Lit : Iir; Len : Int32);

   --  Base of a bit string.  Base_None for a string literal.
   --  Field: Flag12,Flag13,Flag14 (grp)
   function Get_Bit_String_Base (Lit : Iir) return Number_Base_Type;
   procedure Set_Bit_String_Base (Lit : Iir; Base : Number_Base_Type);

   --  Bit string is signed.
   --  Field: Flag1
   function Get_Has_Signed (Lit : Iir) return Boolean;
   procedure Set_Has_Signed (Lit : Iir; Flag : Boolean);

   --  Bit string sign is explicit
   --  Field: Flag2
   function Get_Has_Sign (Lit : Iir) return Boolean;
   procedure Set_Has_Sign (Lit : Iir; Flag : Boolean);

   --  Bit string length is explicit
   --  Field: Flag3
   function Get_Has_Length (Lit : Iir) return Boolean;
   procedure Set_Has_Length (Lit : Iir; Flag : Boolean);

   --  Length of the literal in characters.  Used for pretty print.  Set to 0
   --  when doesn't come from the sources.
   --  Field: Field0 (uc)
   function Get_Literal_Length (Lit : Iir) return Int32;
   procedure Set_Literal_Length (Lit : Iir; Len : Int32);

   --  The origin of a literal can be null_iir for a literal generated by the
   --  parser, or a node which was statically evaluated to this literal.
   --  Such nodes are created by eval_expr.
   --  Field: Field2
   function Get_Literal_Origin (Lit : Iir) return Iir;
   procedure Set_Literal_Origin (Lit : Iir; Orig : Iir);

   --  Field: Field0
   function Get_Range_Origin (Lit : Iir) return Iir;
   procedure Set_Range_Origin (Lit : Iir; Orig : Iir);

   --  Same as Type, but not marked as Ref.  This is when a literal has a
   --  subtype (such as string or bit_string) created specially for the
   --  literal.
   --  Field: Field3
   function Get_Literal_Subtype (Lit : Iir) return Iir;
   procedure Set_Literal_Subtype (Lit : Iir; Atype : Iir);

   --  Field: Field3 Ref
   function Get_Allocator_Subtype (Lit : Iir) return Iir;
   procedure Set_Allocator_Subtype (Lit : Iir; Atype : Iir);

   --  Field: Field3 (uc)
   function Get_Entity_Class (Target : Iir) return Token_Type;
   procedure Set_Entity_Class (Target : Iir; Kind : Token_Type);

   --  Field: Field8 (uc)
   function Get_Entity_Name_List (Target : Iir) return Iir_Flist;
   procedure Set_Entity_Name_List (Target : Iir; Names : Iir_Flist);

   --  Field: Field6
   function Get_Attribute_Designator (Target : Iir) return Iir;
   procedure Set_Attribute_Designator (Target : Iir; Designator : Iir);

   --  Chain of attribute specifications.  This is used only during sem, to
   --  check that no named entity of a given class appear after an attr. spec.
   --  with the entity name list OTHERS or ALL.
   --  Field: Field7 Ref
   function Get_Attribute_Specification_Chain (Target : Iir) return Iir;
   procedure Set_Attribute_Specification_Chain (Target : Iir; Chain : Iir);

   --  Field: Field4 Ref
   function Get_Attribute_Specification (Val : Iir) return Iir;
   procedure Set_Attribute_Specification (Val : Iir; Attr : Iir);

   --  True for attributes on entity, configuration and architecture.  They
   --  are expected to be read from anywhere so the value is expected to be
   --  locally static, but this is not followed by many users and
   --  implementations.
   --  Field: Flag2
   function Get_Static_Attribute_Flag (Attr : Iir) return Boolean;
   procedure Set_Static_Attribute_Flag (Attr : Iir; Flag : Boolean);

   --  Field: Field3 Of_Maybe_Ref (uc)
   function Get_Signal_List (Target : Iir) return Iir_Flist;
   procedure Set_Signal_List (Target : Iir; List : Iir_Flist);

   --  Field: Field3 Of_Maybe_Ref (uc)
   function Get_Quantity_List (Target : Iir) return Iir_Flist;
   procedure Set_Quantity_List (Target : Iir; List : Iir_Flist);

   --  Field: Field3 Forward_Ref
   function Get_Designated_Entity (Val : Iir_Attribute_Value) return Iir;
   procedure Set_Designated_Entity (Val : Iir_Attribute_Value; Entity : Iir);

   --  Field: Field1
   function Get_Formal (Target : Iir) return Iir;
   procedure Set_Formal (Target : Iir; Formal : Iir);

   --  Field: Field3
   function Get_Actual (Target : Iir) return Iir;
   procedure Set_Actual (Target : Iir; Actual : Iir);

   --  Field: Field4
   function Get_Actual_Conversion (Target : Iir) return Iir;
   procedure Set_Actual_Conversion (Target : Iir; Conv : Iir);

   --  Field: Field5
   function Get_Formal_Conversion (Target : Iir) return Iir;
   procedure Set_Formal_Conversion (Target : Iir; Conv : Iir);

   --  This flag is set when the formal is associated in whole (ie, not
   --  individually).
   --  Field: Flag1
   function Get_Whole_Association_Flag (Target : Iir) return Boolean;
   procedure Set_Whole_Association_Flag (Target : Iir; Flag : Boolean);

   --  This flag is set when the formal signal can be the actual signal.  In
   --  this case, the formal signal is not created, and the actual is shared.
   --  This is the signal collapsing optimisation.
   --  Field: Flag2
   function Get_Collapse_Signal_Flag (Target : Iir) return Boolean;
   procedure Set_Collapse_Signal_Flag (Target : Iir; Flag : Boolean);

   --  Set when the node was artificially created, eg by canon.
   --  Currently used only by association_element_open.
   --  Field: Flag3
   function Get_Artificial_Flag (Target : Iir) return Boolean;
   procedure Set_Artificial_Flag (Target : Iir; Flag : Boolean);

   --  This flag is set for a very short time during the check that no in
   --  port is unconnected.
   --  Field: Flag7
   function Get_Open_Flag (Target : Iir) return Boolean;
   procedure Set_Open_Flag (Target : Iir; Flag : Boolean);

   --  This flag is set by trans_analyze if there is a projected waveform
   --  assignment in the process.
   --  Field: Flag5
   function Get_After_Drivers_Flag (Target : Iir) return Boolean;
   procedure Set_After_Drivers_Flag (Target : Iir; Flag : Boolean);

   --  Field: Field1
   function Get_We_Value (We : Iir_Waveform_Element) return Iir;
   procedure Set_We_Value (We : Iir_Waveform_Element; An_Iir : Iir);

   --  Field: Field3
   function Get_Time (We : Iir_Waveform_Element) return Iir;
   procedure Set_Time (We : Iir_Waveform_Element; An_Iir : Iir);

   --  Node associated with a choice.
   --  Field: Field3
   function Get_Associated_Expr (Target : Iir) return Iir;
   procedure Set_Associated_Expr (Target : Iir; Associated : Iir);

   --  Node associated with a choice.
   --  Field: Field3
   function Get_Associated_Block (Target : Iir) return Iir;
   procedure Set_Associated_Block (Target : Iir; Associated : Iir);

   --  Chain associated with a choice.
   --  Field: Field4 Chain
   function Get_Associated_Chain (Target : Iir) return Iir;
   procedure Set_Associated_Chain (Target : Iir; Associated : Iir);

   --  Field: Field5
   function Get_Choice_Name (Choice : Iir) return Iir;
   procedure Set_Choice_Name (Choice : Iir; Name : Iir);

   --  Field: Field5
   function Get_Choice_Expression (Choice : Iir) return Iir;
   procedure Set_Choice_Expression (Choice : Iir; Name : Iir);

   --  Field: Field5
   function Get_Choice_Range (Choice : Iir) return Iir;
   procedure Set_Choice_Range (Choice : Iir; Name : Iir);

   --  Set when a choice belongs to the same alternative as the previous one.
   --  Field: Flag1
   function Get_Same_Alternative_Flag (Target : Iir) return Boolean;
   procedure Set_Same_Alternative_Flag (Target : Iir; Val : Boolean);

   --  For one-dimensional aggregates: the value associated of the type of the
   --  element (vs of the type of the aggregate).  Always true before vhdl-08.
   --  Field: Flag2
   function Get_Element_Type_Flag (Target : Iir) return Boolean;
   procedure Set_Element_Type_Flag (Target : Iir; Val : Boolean);

   --  Field: Field3
   function Get_Architecture (Target : Iir_Entity_Aspect_Entity) return Iir;
   procedure Set_Architecture (Target : Iir_Entity_Aspect_Entity; Arch : Iir);

   --  Field: Field5
   function Get_Block_Specification (Target : Iir) return Iir;
   procedure Set_Block_Specification (Target : Iir; Block : Iir);

   --  Return the link of the previous block_configuration of a
   --  block_configuration.
   --  This single linked list is used to list all the block_configuration that
   --  configuration the same block (which can only be an iterative generate
   --  statement).
   --  All elements of this list must belong to the same block configuration.
   --  The order is not important.
   --  Field: Field4 Ref
   function Get_Prev_Block_Configuration (Target : Iir) return Iir;
   procedure Set_Prev_Block_Configuration (Target : Iir; Block : Iir);

   --  Field: Field3 Chain
   function Get_Configuration_Item_Chain (Target : Iir) return Iir;
   procedure Set_Configuration_Item_Chain (Target : Iir; Chain : Iir);

   --  Chain of attribute values for declared items.
   --  To be used with Get/Set_Value_Chain.
   --  There is no order, therefore, a new attribute value may be always
   --  prepended.
   --  Field: Field5 Ref
   function Get_Attribute_Value_Chain (Target : Iir) return Iir;
   procedure Set_Attribute_Value_Chain (Target : Iir; Chain : Iir);

   --  Next attribute value in the attribute specification chain (of attribute
   --  value).
   --  FIXME: should be a Chain.
   --  Field: Field2
   function Get_Spec_Chain (Target : Iir) return Iir;
   procedure Set_Spec_Chain (Target : Iir; Chain : Iir);

   --  Next attribute value in the parent chain (of attribute value).
   --  Field: Field0 Ref
   function Get_Value_Chain (Target : Iir) return Iir;
   procedure Set_Value_Chain (Target : Iir; Chain : Iir);

   --  Chain of attribute values for attribute specification.
   --  To be used with Get/Set_Spec_Chain.
   --  Field: Field4
   function Get_Attribute_Value_Spec_Chain (Target : Iir) return Iir;
   procedure Set_Attribute_Value_Spec_Chain (Target : Iir; Chain : Iir);

   --  The entity name for an architecture or a configuration.
   --  Field: Field2
   function Get_Entity_Name (Arch : Iir) return Iir;
   procedure Set_Entity_Name (Arch : Iir; Entity : Iir);

   --  The package declaration corresponding to the body.
   --  Field: Field4 Ref
   function Get_Package (Package_Body : Iir) return Iir;
   procedure Set_Package (Package_Body : Iir; Decl : Iir);

   --  The package body corresponding to the package declaration.
   --  Field: Field4 Forward_Ref
   function Get_Package_Body (Pkg : Iir) return Iir;
   procedure Set_Package_Body (Pkg : Iir; Decl : Iir);

   --  The package body corresponding to the package declaration.
   --  Field: Field4
   function Get_Instance_Package_Body (Pkg : Iir) return Iir;
   procedure Set_Instance_Package_Body (Pkg : Iir; Decl : Iir);

   --  Field: Flag1
   function Get_Need_Body (Decl : Iir_Package_Declaration) return Boolean;
   procedure Set_Need_Body (Decl : Iir_Package_Declaration; Flag : Boolean);

   --  Field: Flag2
   function Get_Macro_Expanded_Flag (Decl : Iir) return Boolean;
   procedure Set_Macro_Expanded_Flag (Decl : Iir; Flag : Boolean);

   --  Field: Flag3
   function Get_Need_Instance_Bodies (Decl : Iir) return Boolean;
   procedure Set_Need_Instance_Bodies (Decl : Iir; Flag : Boolean);

   --  Field: Field1
   function Get_Hierarchical_Name (Vunit : Iir) return Iir;
   procedure Set_Hierarchical_Name (Vunit : Iir; Name : Iir);

   --  Field: Field6 Chain
   function Get_Vunit_Item_Chain (Vunit : Iir) return Iir;
   procedure Set_Vunit_Item_Chain (Vunit : Iir; Chain : Iir);

   --  Chain of vunit declarations bound to an entity or an architecture.
   --  Field: Field8 Chain
   function Get_Bound_Vunit_Chain (Unit : Iir) return Iir;
   procedure Set_Bound_Vunit_Chain (Unit : Iir; Vunit : Iir);

   --  Field: Field4
   function Get_Verification_Block_Configuration (Vunit : Iir) return Iir;
   procedure Set_Verification_Block_Configuration (Vunit : Iir; Conf : Iir);

   --  Field: Field4
   function Get_Block_Configuration (Target : Iir) return Iir;
   procedure Set_Block_Configuration (Target : Iir; Block : Iir);

   --  Field: Field4 Chain
   function Get_Concurrent_Statement_Chain (Target : Iir) return Iir;
   procedure Set_Concurrent_Statement_Chain (Target : Iir; First : Iir);

   --  Field: Field2 Chain_Next
   function Get_Chain (Target : Iir) return Iir;
   procedure Set_Chain (Target : Iir; Chain : Iir);
   pragma Inline (Get_Chain);

   --  Field: Field7 Chain
   function Get_Port_Chain (Target : Iir) return Iir;
   procedure Set_Port_Chain (Target : Iir; Chain : Iir);

   --  Field: Field6 Chain
   function Get_Generic_Chain (Target : Iir) return Iir;
   procedure Set_Generic_Chain (Target : Iir; Generics : Iir);

   --  Field: Field1 Ref
   function Get_Type (Target : Iir) return Iir;
   procedure Set_Type (Target : Iir; Atype : Iir);
   pragma Inline (Get_Type);

   --  The subtype indication of a declaration.  If several declarations share
   --  the same subtype_indication like in:
   --    variable a, b : integer := 5;
   --  then only the first declaration is the owner of the subtype_indication.
   --  Field: Field5 Maybe_Ref
   function Get_Subtype_Indication (Target : Iir) return Iir;
   procedure Set_Subtype_Indication (Target : Iir; Atype : Iir);

   --  Discrete range of an iterator.  During analysis, a subtype indication
   --  is created from this range.
   --  Field: Field4
   function Get_Discrete_Range (Target : Iir) return Iir;
   procedure Set_Discrete_Range (Target : Iir; Rng : Iir);

   --  Field: Field1
   function Get_Type_Definition (Decl : Iir) return Iir;
   procedure Set_Type_Definition (Decl : Iir; Atype : Iir);

   --  The subtype definition associated with the type declaration (if any).
   --  Field: Field4 Forward_Ref
   function Get_Subtype_Definition (Target : Iir) return Iir;
   procedure Set_Subtype_Definition (Target : Iir; Def : Iir);

   --  Set if the type declaration completes an incomplete type declaration
   --  Field: Field5 Ref
   function Get_Incomplete_Type_Declaration (N : Iir) return Iir;
   procedure Set_Incomplete_Type_Declaration (N : Iir; Decl : Iir);

   --  Implicit operations of an interface type declaration.
   --  Field: Field4 Chain
   function Get_Interface_Type_Subprograms (Target : Iir) return Iir;
   procedure Set_Interface_Type_Subprograms (Target : Iir; Subprg : Iir);

   --  Field: Field1
   function Get_Nature_Definition (Target : Iir) return Iir;
   procedure Set_Nature_Definition (Target : Iir; Def : Iir);

   --  Field: Field1 Ref
   function Get_Nature (Target : Iir) return Iir;
   procedure Set_Nature (Target : Iir; Nature : Iir);

   --  Field: Field5
   function Get_Subnature_Indication (Decl : Iir) return Iir;
   procedure Set_Subnature_Indication (Decl : Iir; Sub_Nature : Iir);

   --  Mode of interfaces or file (v87).
   --  Field: Flag13,Flag14,Flag15 (grp)
   function Get_Mode (Target : Iir) return Iir_Mode;
   procedure Set_Mode (Target : Iir; Mode : Iir_Mode);

   --  True if the signal is guarded (has a signal kind).
   --  Field: Flag8
   function Get_Guarded_Signal_Flag (Target : Iir) return Boolean;
   procedure Set_Guarded_Signal_Flag (Target : Iir; Guarded : Boolean);

   --  Field: Flag9 (uc)
   function Get_Signal_Kind (Target : Iir) return Iir_Signal_Kind;
   procedure Set_Signal_Kind (Target : Iir; Signal_Kind : Iir_Signal_Kind);

   --  The base name of a name is the node at the origin of the name.
   --  The base name is a declaration (signal, object, constant or interface),
   --  a selected_by_all name, an implicit_dereference name.
   --  Field: Field5 Ref
   function Get_Base_Name (Target : Iir) return Iir;
   procedure Set_Base_Name (Target : Iir; Name : Iir);
   pragma Inline (Get_Base_Name);

   --  Field: Field5 Chain
   function Get_Interface_Declaration_Chain (Target : Iir) return Iir;
   procedure Set_Interface_Declaration_Chain (Target : Iir; Chain : Iir);
   pragma Inline (Get_Interface_Declaration_Chain);

   --  Field: Field6 Ref
   function Get_Subprogram_Specification (Target : Iir) return Iir;
   procedure Set_Subprogram_Specification (Target : Iir; Spec : Iir);

   --  Field: Field4 Chain
   function Get_Sequential_Statement_Chain (Target : Iir) return Iir;
   procedure Set_Sequential_Statement_Chain (Target : Iir; Chain : Iir);

   --  Field: Field4 Chain
   function Get_Simultaneous_Statement_Chain (Target : Iir) return Iir;
   procedure Set_Simultaneous_Statement_Chain (Target : Iir; Chain : Iir);

   --  The body of a subprogram (from the subprogram specification).
   --  Note that this field is only set when the body has been analyzed (ok,
   --  that's obvious).  For subprogram specifications in instantiated package,
   --  this field is in general not set because the package specification may
   --  be instantiated before the package body is analyzed and there is no
   --  tracking of all instantiated packages.  So when the package body is
   --  analyzed, there is no way to set this field for the subprograms in all
   --  instantiated specifications.
   --  You could use Get_Subprogram_Body_Origin to extract the body.  It uses
   --  the Origin link to find the original specification which has this field
   --  set.
   --  Field: Field9 Forward_Ref
   function Get_Subprogram_Body (Target : Iir) return Iir;
   procedure Set_Subprogram_Body (Target : Iir; A_Body : Iir);

   --  Several subprograms in a declarative region may have the same
   --  identifier.  If the overload number is not 0, it is the rank of the
   --  subprogram.  If the overload number is 0, then the identifier is not
   --  overloaded in the declarative region.
   --  Field: Field12 (pos)
   function Get_Overload_Number (Target : Iir) return Iir_Int32;
   procedure Set_Overload_Number (Target : Iir; Val : Iir_Int32);

   --  Depth of a subprogram.
   --  For a subprogram declared immediately within an entity, architecture,
   --  package, process, block, generate, the depth is 0.
   --  For a subprogram declared immediately within a subprogram of level N,
   --  the depth is N + 1.
   --  Depth is used with depth of impure objects to check purity rules.
   --  Field: Field10 (pos)
   function Get_Subprogram_Depth (Target : Iir) return Iir_Int32;
   procedure Set_Subprogram_Depth (Target : Iir; Depth : Iir_Int32);

   --  Hash of a subprogram profile.
   --  This is used to speed up subprogram profile comparison, which is very
   --  often used by overload.
   --  Field: Field4 (pos)
   function Get_Subprogram_Hash (Target : Iir) return Iir_Int32;
   procedure Set_Subprogram_Hash (Target : Iir; Val : Iir_Int32);
   pragma Inline (Get_Subprogram_Hash);

   --  Depth of the deepest impure object.
   --  Field: Field3 (uc)
   function Get_Impure_Depth (Target : Iir) return Iir_Int32;
   procedure Set_Impure_Depth (Target : Iir; Depth : Iir_Int32);

   --  Field: Field1 Ref
   function Get_Return_Type (Target : Iir) return Iir;
   procedure Set_Return_Type (Target : Iir; Decl : Iir);
   pragma Inline (Get_Return_Type);

   --  Code of an implicit subprogram definition.
   --  Field: Field7 (pos)
   function Get_Implicit_Definition (D : Iir) return Iir_Predefined_Functions;
   procedure Set_Implicit_Definition (D : Iir; Def : Iir_Predefined_Functions);

   --  Field: Field7
   function Get_Uninstantiated_Subprogram_Name (N : Iir) return Iir;
   procedure Set_Uninstantiated_Subprogram_Name (N : Iir; Name : Iir);

   --  Get the default value of an object declaration.
   --  Null_iir if no default value.
   --  Note that this node can be shared between declarations if they are
   --  separated by comma, such as in:
   --    variable a, b : integer := 5;
   --    procedure p (a, b : natural := 7);
   --  Field: Field4 Maybe_Ref
   function Get_Default_Value (Target : Iir) return Iir;
   procedure Set_Default_Value (Target : Iir; Value : Iir);

   --  The deferred_declaration field points to the deferred constant
   --  declaration for a full constant declaration, or is null_iir for a
   --  usual or deferred constant declaration.
   --  Set only during sem.
   --  Field: Field6 Forward_Ref
   function Get_Deferred_Declaration (Target : Iir) return Iir;
   procedure Set_Deferred_Declaration (Target : Iir; Decl : Iir);

   --  The deferred_declaration_flag must be set if the constant declaration is
   --  a deferred_constant declaration.
   --  Set only during sem.
   --  Field: Flag1
   function Get_Deferred_Declaration_Flag (Target : Iir) return Boolean;
   procedure Set_Deferred_Declaration_Flag (Target : Iir; Flag : Boolean);

   --  If true, the variable is declared shared.
   --  Field: Flag2
   function Get_Shared_Flag (Target : Iir) return Boolean;
   procedure Set_Shared_Flag (Target : Iir; Shared : Boolean);

   --  Get the design unit in which the target is declared.
   --  For a library unit, this is to get the design unit node.
   --  Field: Field0
   function Get_Design_Unit (Target : Iir) return Iir;
   procedure Set_Design_Unit (Target : Iir; Unit : Iir);

   --  Corresponding block statement for an implicit guard signal.
   --  Field: Field5 Ref
   function Get_Block_Statement (Target : Iir) return Iir;
   procedure Set_Block_Statement (Target : Iir; Block : Iir);

   --  For a non-resolved signal: null_iir if the signal has no driver, or
   --  a process/concurrent_statement for which the signal should have a
   --  driver.  This is used to catch at analyse time unresolved signals with
   --  several drivers.
   --  Field: Field7
   function Get_Signal_Driver (Target : Iir_Signal_Declaration) return Iir;
   procedure Set_Signal_Driver (Target : Iir_Signal_Declaration; Driver : Iir);

   --  Field: Field1 Chain
   function Get_Declaration_Chain (Target : Iir) return Iir;
   procedure Set_Declaration_Chain (Target : Iir; Decls : Iir);

   --  Field: Field6
   function Get_File_Logical_Name (Target : Iir_File_Declaration) return Iir;
   procedure Set_File_Logical_Name (Target : Iir_File_Declaration; Name : Iir);

   --  Field: Field7
   function Get_File_Open_Kind (Target : Iir_File_Declaration) return Iir;
   procedure Set_File_Open_Kind (Target : Iir_File_Declaration; Kind : Iir);

   --  Field: Field4 (pos)
   function Get_Element_Position (Target : Iir) return Iir_Index32;
   procedure Set_Element_Position (Target : Iir; Pos : Iir_Index32);

   --  Selected names of an use_clause are chained.
   --  Field: Field3
   function Get_Use_Clause_Chain (Target : Iir) return Iir;
   procedure Set_Use_Clause_Chain (Target : Iir; Chain : Iir);

   --  Selected names of a context_reference are chained.
   --  Field: Field3
   function Get_Context_Reference_Chain (Target : Iir) return Iir;
   procedure Set_Context_Reference_Chain (Target : Iir; Chain : Iir);

   --  Field: Field3 Chain
   function Get_Inherit_Spec_Chain (Target : Iir) return Iir;
   procedure Set_Inherit_Spec_Chain (Target : Iir; Chain : Iir);

   --  Selected name of an use_clause or context_reference
   --  Field: Field1
   function Get_Selected_Name (Target : Iir) return Iir;
   procedure Set_Selected_Name (Target : Iir; Name : Iir);

   --  The type declarator which declares the type definition DEF.  Can also
   --  be a nature declarator for composite nature definition.
   --  Field: Field3 Ref
   function Get_Type_Declarator (Def : Iir) return Iir;
   procedure Set_Type_Declarator (Def : Iir; Decl : Iir);

   --  Field: Field5 Forward_Ref
   function Get_Complete_Type_Definition (N : Iir) return Iir;
   procedure Set_Complete_Type_Definition (N : Iir; Def : Iir);

   --  Field: Field0 Forward_Ref
   function Get_Incomplete_Type_Ref_Chain (N : Iir) return Iir;
   procedure Set_Incomplete_Type_Ref_Chain (N : Iir; Def : Iir);

   --  Field: Field5 Ref
   function Get_Associated_Type (Def : Iir) return Iir;
   procedure Set_Associated_Type (Def : Iir; Atype : Iir);

   --  Field: Field2 (uc)
   function Get_Enumeration_Literal_List (Target : Iir) return Iir_Flist;
   procedure Set_Enumeration_Literal_List (Target : Iir; List : Iir_Flist);

   --  Field: Field1 Chain
   function Get_Entity_Class_Entry_Chain (Target : Iir) return Iir;
   procedure Set_Entity_Class_Entry_Chain (Target : Iir; Chain : Iir);

   --  Field: Field1 (uc)
   function Get_Group_Constituent_List (Group : Iir) return Iir_Flist;
   procedure Set_Group_Constituent_List (Group : Iir; List : Iir_Flist);

   --  Chain of physical type units.
   --  The first unit is the primary unit.  If you really need the primary
   --  unit (and not the chain), you'd better to use Get_Primary_Unit.
   --  Field: Field2 Chain
   function Get_Unit_Chain (Target : Iir) return Iir;
   procedure Set_Unit_Chain (Target : Iir; Chain : Iir);

   --  Alias of Get_Unit_Chain.
   --  Return the primary unit of a physical type.
   --  Field: Field2 Ref
   function Get_Primary_Unit (Target : Iir) return Iir;
   procedure Set_Primary_Unit (Target : Iir; Unit : Iir);

   --  Get/Set the identifier of a declaration.
   --  Can also be used instead of get/set_label.
   --  Field: Field3 (uc)
   function Get_Identifier (Target : Iir) return Name_Id;
   procedure Set_Identifier (Target : Iir; Identifier : Name_Id);
   pragma Inline (Get_Identifier);

   --  Field: Field3 (uc)
   function Get_Label (Target : Iir) return Name_Id;
   procedure Set_Label (Target : Iir; Label : Name_Id);

   --  Return a subtype declaration for the return subtype (vhdl-19)
   --  Field: Field11
   function Get_Return_Identifier (Target : Iir) return Iir;
   procedure Set_Return_Identifier (Target : Iir; Decl : Iir);

   --  Get/Set the visible flag of a declaration.
   --  The visible flag is true to make invalid the use of the identifier
   --  during its declaration.  It is set to false when the identifier is added
   --  to the name table, and set to true when the declaration is finished.
   --  Field: Flag4
   function Get_Visible_Flag (Target : Iir) return Boolean;
   procedure Set_Visible_Flag (Target : Iir; Flag : Boolean);

   --  Field: Field1 Maybe_Ref
   function Get_Range_Constraint (Target : Iir) return Iir;
   procedure Set_Range_Constraint (Target : Iir; Constraint : Iir);

   --  Field: Flag1 (uc)
   function Get_Direction (Decl : Iir) return Direction_Type;
   procedure Set_Direction (Decl : Iir; Dir : Direction_Type);

   --  Field: Field4 Ref
   function Get_Left_Limit (Decl : Iir_Range_Expression) return Iir;
   procedure Set_Left_Limit (Decl : Iir_Range_Expression; Limit : Iir);

   --  Field: Field5 Ref
   function Get_Right_Limit (Decl : Iir_Range_Expression) return Iir;
   procedure Set_Right_Limit (Decl : Iir_Range_Expression; Limit : Iir);

   --  Field: Field2
   function Get_Left_Limit_Expr (Decl : Iir_Range_Expression) return Iir;
   procedure Set_Left_Limit_Expr (Decl : Iir_Range_Expression; Limit : Iir);

   --  Field: Field3
   function Get_Right_Limit_Expr (Decl : Iir_Range_Expression) return Iir;
   procedure Set_Right_Limit_Expr (Decl : Iir_Range_Expression; Limit : Iir);

   --  Field: Field4 Ref
   function Get_Parent_Type (Decl : Iir) return Iir;
   procedure Set_Parent_Type (Decl : Iir; Base_Type : Iir);
   pragma Inline (Get_Parent_Type);

   --  Only for composite base nature: the simple nature.
   --  Field: Field7 Ref
   function Get_Simple_Nature (Def : Iir) return Iir;
   procedure Set_Simple_Nature (Def : Iir; Nature : Iir);

   --  Field: Field4 Ref
   function Get_Base_Nature (Decl : Iir) return Iir;
   procedure Set_Base_Nature (Decl : Iir; Base_Nature : Iir);

   --  Either a resolution function name, an array_element_resolution or a
   --  record_resolution
   --  Field: Field5
   function Get_Resolution_Indication (Decl : Iir) return Iir;
   procedure Set_Resolution_Indication (Decl : Iir; Ind : Iir);

   --  Field: Field1 Chain
   function Get_Record_Element_Resolution_Chain (Res : Iir) return Iir;
   procedure Set_Record_Element_Resolution_Chain (Res : Iir; Chain : Iir);

   --  Field: Field7
   function Get_Tolerance (Def : Iir) return Iir;
   procedure Set_Tolerance (Def : Iir; Tol : Iir);

   --  Field: Field8
   function Get_Plus_Terminal_Name (Def : Iir) return Iir;
   procedure Set_Plus_Terminal_Name (Def : Iir; Name : Iir);

   --  Field: Field9
   function Get_Minus_Terminal_Name (Def : Iir) return Iir;
   procedure Set_Minus_Terminal_Name (Def : Iir; Name : Iir);

   --  Field: Field10 Ref
   function Get_Plus_Terminal (Def : Iir) return Iir;
   procedure Set_Plus_Terminal (Def : Iir; Terminal : Iir);

   --  Field: Field11 Ref
   function Get_Minus_Terminal (Def : Iir) return Iir;
   procedure Set_Minus_Terminal (Def : Iir; Terminal : Iir);

   --  Field: Field6
   function Get_Magnitude_Expression (Decl : Iir) return Iir;
   procedure Set_Magnitude_Expression (Decl : Iir; Expr : Iir);

   --  Field: Field7
   function Get_Phase_Expression (Decl : Iir) return Iir;
   procedure Set_Phase_Expression (Decl : Iir; Expr : Iir);

   --  Field: Field4
   function Get_Power_Expression (Decl : Iir) return Iir;
   procedure Set_Power_Expression (Decl : Iir; Expr : Iir);

   --  Field: Field5
   function Get_Simultaneous_Left (Def : Iir) return Iir;
   procedure Set_Simultaneous_Left (Def : Iir; Expr : Iir);

   --  Field: Field6
   function Get_Simultaneous_Right (Def : Iir) return Iir;
   procedure Set_Simultaneous_Right (Def : Iir; Expr : Iir);

   --  True if ATYPE defines std.textio.text file type.
   --  Field: Flag4
   function Get_Text_File_Flag (Atype : Iir) return Boolean;
   procedure Set_Text_File_Flag (Atype : Iir; Flag : Boolean);

   --  True if enumeration type ATYPE has only character literals.
   --  Field: Flag4
   function Get_Only_Characters_Flag (Atype : Iir) return Boolean;
   procedure Set_Only_Characters_Flag (Atype : Iir; Flag : Boolean);

   --  True if enumeration type ATYPE is a character type.
   --  Field: Flag5
   function Get_Is_Character_Type (Atype : Iir) return Boolean;
   procedure Set_Is_Character_Type (Atype : Iir; Flag : Boolean);

   --  Field: State1 (pos)
   function Get_Nature_Staticness (Anat : Iir) return Iir_Staticness;
   procedure Set_Nature_Staticness (Anat : Iir; Static : Iir_Staticness);

   --  Field: State1 (pos)
   function Get_Type_Staticness (Atype : Iir) return Iir_Staticness;
   procedure Set_Type_Staticness (Atype : Iir; Static : Iir_Staticness);

   --  Field: State2 (pos)
   function Get_Constraint_State (Atype : Iir) return Iir_Constraint;
   procedure Set_Constraint_State (Atype : Iir; State : Iir_Constraint);

   --  Reference either index_subtype_definition_list of array_type_definition
   --  or index_constraint_list of array_subtype_definition.  Set only when
   --  the index_sutype is constrained (to differentiate with unconstrained
   --  index type).
   --  Field: Field9 Ref (uc)
   function Get_Index_Subtype_List (Decl : Iir) return Iir_Flist;
   procedure Set_Index_Subtype_List (Decl : Iir; List : Iir_Flist);

   --  List of type marks for indexes type of array types.
   --  Field: Field6 (uc)
   function Get_Index_Subtype_Definition_List (Def : Iir) return Iir_Flist;
   procedure Set_Index_Subtype_Definition_List (Def : Iir; Idx : Iir_Flist);

   --  The subtype_indication as it appears in a array type declaration.
   --  Field: Field2
   function Get_Element_Subtype_Indication (Decl : Iir) return Iir;
   procedure Set_Element_Subtype_Indication (Decl : Iir; Sub_Type : Iir);

   --  Field: Field1 Ref
   function Get_Element_Subtype (Decl : Iir) return Iir;
   procedure Set_Element_Subtype (Decl : Iir; Sub_Type : Iir);

   --  Field: Field2
   function Get_Element_Subnature_Indication (Decl : Iir) return Iir;
   procedure Set_Element_Subnature_Indication (Decl : Iir; Sub_Nature : Iir);

   --  Field: Field1 Ref
   function Get_Element_Subnature (Decl : Iir) return Iir;
   procedure Set_Element_Subnature (Decl : Iir; Sub_Nature : Iir);

   --  Field: Field6 (uc)
   function Get_Index_Constraint_List (Def : Iir) return Iir_Flist;
   procedure Set_Index_Constraint_List (Def : Iir; List : Iir_Flist);

   --  Field: Field8
   function Get_Array_Element_Constraint (Def : Iir) return Iir;
   procedure Set_Array_Element_Constraint (Def : Iir; El : Iir);

   --  Field: Flag5
   function Get_Has_Array_Constraint_Flag (Def : Iir) return Boolean;
   procedure Set_Has_Array_Constraint_Flag (Def : Iir; Flag : Boolean);

   --  Field: Flag6
   function Get_Has_Element_Constraint_Flag (Def : Iir) return Boolean;
   procedure Set_Has_Element_Constraint_Flag (Def : Iir; Flag : Boolean);

   --  List of elements of a record.
   --  For a record_type_definition: Is_Ref is false, as the elements
   --   declaration are owned by the type definition.
   --  For a record_subtype_definition: Is_Ref is false, as new constrained
   --   elements are owned through the Owned_Elements_Chain list.
   --  Field: Field1 Of_Maybe_Ref (uc)
   function Get_Elements_Declaration_List (Decl : Iir) return Iir_Flist;
   procedure Set_Elements_Declaration_List (Decl : Iir; List : Iir_Flist);

   --  Field: Field6 Chain
   function Get_Owned_Elements_Chain (Atype : Iir) return Iir;
   procedure Set_Owned_Elements_Chain (Atype : Iir; Chain : Iir);

   --  Field: Field1 Forward_Ref
   function Get_Designated_Type (Target : Iir) return Iir;
   procedure Set_Designated_Type (Target : Iir; Dtype : Iir);

   --  Field: Field5
   function Get_Designated_Subtype_Indication (Target : Iir) return Iir;
   procedure Set_Designated_Subtype_Indication (Target : Iir; Dtype : Iir);

   --  List of indexes for indexed name.
   --  Field: Field2 (uc)
   function Get_Index_List (Decl : Iir) return Iir_Flist;
   procedure Set_Index_List (Decl : Iir; List : Iir_Flist);

   --  The terminal declaration for the reference (ground) of a nature
   --  Field: Field2 Forward_Ref
   function Get_Reference (Def : Iir) return Iir;
   procedure Set_Reference (Def : Iir; Ref : Iir);

   --  Field: Field3 Ref
   function Get_Nature_Declarator (Def : Iir) return Iir;
   procedure Set_Nature_Declarator (Def : Iir; Decl : Iir);

   --  Field: Field9
   function Get_Across_Type_Mark (Def : Iir) return Iir;
   procedure Set_Across_Type_Mark (Def : Iir; Name : Iir);

   --  Field: Field10
   function Get_Through_Type_Mark (Def : Iir) return Iir;
   procedure Set_Through_Type_Mark (Def : Iir; Atype : Iir);

   --  For array and record nature: the owner of the across type.
   --  Field: Field10
   function Get_Across_Type_Definition (Def : Iir) return Iir;
   procedure Set_Across_Type_Definition (Def : Iir; Atype : Iir);

   --  For array and record nature: the owner of the through type.
   --  Field: Field5
   function Get_Through_Type_Definition (Def : Iir) return Iir;
   procedure Set_Through_Type_Definition (Def : Iir; Atype : Iir);

   --  Field: Field11 Ref
   function Get_Across_Type (Def : Iir) return Iir;
   procedure Set_Across_Type (Def : Iir; Atype : Iir);

   --  Field: Field12 Ref
   function Get_Through_Type (Def : Iir) return Iir;
   procedure Set_Through_Type (Def : Iir; Atype : Iir);

   --  Field: Field1 Maybe_Ref
   function Get_Target (Target : Iir) return Iir;
   procedure Set_Target (Target : Iir; Atarget : Iir);

   --  Field: Field5 Chain
   function Get_Waveform_Chain (Target : Iir) return Iir;
   procedure Set_Waveform_Chain (Target : Iir; Chain : Iir);

   --  Field: Field8 Ref
   function Get_Guard (Target : Iir) return Iir;
   procedure Set_Guard (Target : Iir; Guard : Iir);

   --  Field: Flag1 (uc)
   function Get_Delay_Mechanism (Target : Iir) return Iir_Delay_Mechanism;
   procedure Set_Delay_Mechanism (Target : Iir; Kind : Iir_Delay_Mechanism);

   --  Field: Field4
   function Get_Reject_Time_Expression (Target : Iir) return Iir;
   procedure Set_Reject_Time_Expression (Target : Iir; Expr : Iir);

   --  Field: Flag1 (uc)
   function Get_Force_Mode (Stmt : Iir) return Iir_Force_Mode;
   procedure Set_Force_Mode (Stmt : Iir; Mode : Iir_Force_Mode);

   --  Field: Flag2
   function Get_Has_Force_Mode (Stmt : Iir) return Boolean;
   procedure Set_Has_Force_Mode (Stmt : Iir; Flag : Boolean);

   --  The Is_Ref flag is set for extracted sensitivity lists.
   --  Field: Field6 Of_Maybe_Ref (uc)
   function Get_Sensitivity_List (Wait : Iir) return Iir_List;
   procedure Set_Sensitivity_List (Wait : Iir; List : Iir_List);

   --  Field: Field8
   function Get_Process_Origin (Proc : Iir) return Iir;
   procedure Set_Process_Origin (Proc : Iir; Orig : Iir);

   --  Field: Field7
   function Get_Package_Origin (Pkg : Iir) return Iir;
   procedure Set_Package_Origin (Pkg : Iir; Orig : Iir);

   --  Field: Field5
   function Get_Condition_Clause (Wait : Iir_Wait_Statement) return Iir;
   procedure Set_Condition_Clause (Wait : Iir_Wait_Statement; Cond : Iir);

   --  Field: Field4 Chain
   function Get_Break_Element (Stmt : Iir) return Iir;
   procedure Set_Break_Element (Stmt : Iir; El : Iir);

   --  Field: Field3
   function Get_Selector_Quantity (Stmt : Iir) return Iir;
   procedure Set_Selector_Quantity (Stmt : Iir; Sel : Iir);

   --  Field: Field4
   function Get_Break_Quantity (Stmt : Iir) return Iir;
   procedure Set_Break_Quantity (Stmt : Iir; Sel : Iir);

   --  Field: Field1
   function Get_Timeout_Clause (Wait : Iir_Wait_Statement) return Iir;
   procedure Set_Timeout_Clause (Wait : Iir_Wait_Statement; Timeout : Iir);

   --  If set, the concurrent statement is postponed.
   --  Field: Flag3
   function Get_Postponed_Flag (Target : Iir) return Boolean;
   procedure Set_Postponed_Flag (Target : Iir; Value : Boolean);

   --  Returns the list of subprogram called in this subprogram or process.
   --  Note: implicit function (such as implicit operators) are omitted
   --  from this list, since the purpose of this list is to correctly set
   --  flags for side effects (purity_state, wait_state).
   --  Can return null_iir if there is no subprogram called.
   --  Field: Field7 Of_Ref (uc)
   function Get_Callees_List (Proc : Iir) return Iir_List;
   procedure Set_Callees_List (Proc : Iir; List : Iir_List);

   --  Get/Set the passive flag of a process.
   --   TRUE if the process must be passive.
   --   FALSE if the process may be not passive.
   --  For a procedure declaration, set if it is passive.
   --  Field: Flag2
   function Get_Passive_Flag (Proc : Iir) return Boolean;
   procedure Set_Passive_Flag (Proc : Iir; Flag : Boolean);

   --  True if the function is used as a resolution function.
   --  Field: Flag13
   function Get_Resolution_Function_Flag (Func : Iir) return Boolean;
   procedure Set_Resolution_Function_Flag (Func : Iir; Flag : Boolean);

   --  Get/Set the wait state of the current subprogram or process.
   --  TRUE if it contains a wait statement, either directly or
   --   indirectly.
   --  FALSE if it doesn't contain a wait statement.
   --  UNKNOWN if the wait status is not yet known.
   --  Field: State1 (pos)
   function Get_Wait_State (Proc : Iir) return Tri_State_Type;
   procedure Set_Wait_State (Proc : Iir; State : Tri_State_Type);

   --  Get/Set whether the subprogram may be called by a sensitized process
   --  whose sensitivity list is ALL.
   --  FALSE if declared in a package unit and reads a signal that is not
   --    one of its interface, or if it calls such a subprogram.
   --  TRUE if it doesn't call a subprogram whose state is False and
   --    either doesn't read a signal or declared within an entity or
   --    architecture.
   --  UNKNOWN if the status is not yet known.
   --  Field: State3 (pos)
   function Get_All_Sensitized_State (Proc : Iir) return Iir_All_Sensitized;
   procedure Set_All_Sensitized_State (Proc : Iir; State : Iir_All_Sensitized);

   --  Get/Set the seen flag.
   --  Used when the graph of callees is walked, to avoid infinite loops, since
   --  the graph is not a DAG (there may be cycles).
   --  Field: Flag1
   function Get_Seen_Flag (Proc : Iir) return Boolean;
   procedure Set_Seen_Flag (Proc : Iir; Flag : Boolean);

   --  Get/Set the pure flag of a function.
   --  TRUE if the function is declared pure.
   --  FALSE if the function is declared impure.
   --  Field: Flag2
   function Get_Pure_Flag (Func : Iir) return Boolean;
   procedure Set_Pure_Flag (Func : Iir; Flag : Boolean);

   --  Get/Set the foreign flag of a declaration.
   --  TRUE if the declaration was decorated with the std.foreign attribute.
   --  Field: Flag3
   function Get_Foreign_Flag (Decl : Iir) return Boolean;
   procedure Set_Foreign_Flag (Decl : Iir; Flag : Boolean);

   --  Get/Set the resolved flag of a subtype definition.
   --  A subtype definition may be resolved either because a
   --  resolution_indication is present in the subtype_indication, or
   --  because all elements type are resolved.
   --  Field: Flag1
   function Get_Resolved_Flag (Atype : Iir) return Boolean;
   procedure Set_Resolved_Flag (Atype : Iir; Flag : Boolean);

   --  Get/Set the signal_type flag of a type/subtype definition.
   --  This flags indicates whether the type can be used as a signal type.
   --  Access types, file types and composite types whose a sub-element is
   --  an access type cannot be used as a signal type.
   --  Field: Flag2
   function Get_Signal_Type_Flag (Atype : Iir) return Boolean;
   procedure Set_Signal_Type_Flag (Atype : Iir; Flag : Boolean);

   --  True if ATYPE is used to declare a signal or to handle a signal
   --   (such as slice or aliases).
   --  Field: Flag3
   function Get_Has_Signal_Flag (Atype : Iir) return Boolean;
   procedure Set_Has_Signal_Flag (Atype : Iir; Flag : Boolean);

   --  Get/Set the purity status of a subprogram.
   --  Field: State2 (pos)
   function Get_Purity_State (Proc : Iir) return Iir_Pure_State;
   procedure Set_Purity_State (Proc : Iir; State : Iir_Pure_State);

   --  Set during binding when DESIGN is added in a list of file to bind.
   --  Field: Flag3
   function Get_Elab_Flag (Design : Iir) return Boolean;
   procedure Set_Elab_Flag (Design : Iir; Flag : Boolean);

   --  Field: Flag1
   function Get_Vendor_Library_Flag (Lib : Iir) return Boolean;
   procedure Set_Vendor_Library_Flag (Lib : Iir; Flag : Boolean);

   --  Used only by configuration to mark a design unit as already inserted in
   --  the list of units.  Used to avoid double insertion.
   --  Field: Flag4
   function Get_Configuration_Mark_Flag (Design : Iir) return Boolean;
   procedure Set_Configuration_Mark_Flag (Design : Iir; Flag : Boolean);

   --  Used only by configuration to flag units completely handled.  Used to
   --  detect recursion.
   --  Field: Flag5
   function Get_Configuration_Done_Flag (Design : Iir) return Boolean;
   procedure Set_Configuration_Done_Flag (Design : Iir; Flag : Boolean);

   --  Set on an array_subtype if there is an index constraint.
   --  If not set, the subtype is unconstrained.
   --  Field: Flag4
   function Get_Index_Constraint_Flag (Atype : Iir) return Boolean;
   procedure Set_Index_Constraint_Flag (Atype : Iir; Flag : Boolean);

   --  Field: Flag12
   function Get_Hide_Implicit_Flag (Subprg : Iir) return Boolean;
   procedure Set_Hide_Implicit_Flag (Subprg : Iir; Flag : Boolean);

   --  Condition of an assertion.
   --  Field: Field1
   function Get_Assertion_Condition (Target : Iir) return Iir;
   procedure Set_Assertion_Condition (Target : Iir; Cond : Iir);

   --  Report expression of an assertion or report statement.
   --  Field: Field5
   function Get_Report_Expression (Target : Iir) return Iir;
   procedure Set_Report_Expression (Target : Iir; Expr : Iir);

   --  Severity expression of an assertion or report statement.
   --  Field: Field4
   function Get_Severity_Expression (Target : Iir) return Iir;
   procedure Set_Severity_Expression (Target : Iir; Expr : Iir);

   --  Instantiated unit of a component instantiation statement.
   --  Field: Field1
   function Get_Instantiated_Unit (Target : Iir) return Iir;
   procedure Set_Instantiated_Unit (Target : Iir; Unit : Iir);

   --  Generic map aspect list.
   --  Field: Field8 Chain
   function Get_Generic_Map_Aspect_Chain (Target : Iir) return Iir;
   procedure Set_Generic_Map_Aspect_Chain (Target : Iir; Generics : Iir);

   --  Port map aspect list.
   --  Field: Field9 Chain
   function Get_Port_Map_Aspect_Chain (Target : Iir) return Iir;
   procedure Set_Port_Map_Aspect_Chain (Target : Iir; Port : Iir);

   --  Configuration of an entity_aspect_configuration.
   --  Field: Field1
   function Get_Configuration_Name (Target : Iir) return Iir;
   procedure Set_Configuration_Name (Target : Iir; Conf : Iir);

   --  Component configuration for a component_instantiation_statement.
   --  Field: Field6 Forward_Ref
   function Get_Component_Configuration (Target : Iir) return Iir;
   procedure Set_Component_Configuration (Target : Iir; Conf : Iir);

   --  Configuration specification for a component_instantiation_statement.
   --  Field: Field7 Ref
   function Get_Configuration_Specification (Target : Iir) return Iir;
   procedure Set_Configuration_Specification (Target : Iir; Conf : Iir);

   --  Set/Get the default binding indication of a configuration specification
   --  or a component configuration.
   --  Field: Field5
   function Get_Default_Binding_Indication (Target : Iir) return Iir;
   procedure Set_Default_Binding_Indication (Target : Iir; Conf : Iir);

   --  Set/Get the default configuration of an architecture.
   --  Field: Field6
   function Get_Default_Configuration_Declaration (Target : Iir) return Iir;
   procedure Set_Default_Configuration_Declaration (Target : Iir; Conf : Iir);

   --  Expression for an various nodes.
   --  Field: Field5
   function Get_Expression (Target : Iir) return Iir;
   procedure Set_Expression (Target : Iir; Expr : Iir);

   --  A conditional expression.
   --  Node kind is a Iir_Kind_Conditional_Expression.
   --  Field: Field5 Chain
   function Get_Conditional_Expression_Chain (Target : Iir) return Iir;
   procedure Set_Conditional_Expression_Chain (Target : Iir; Chain : Iir);

   --  Set to the designated type (either the type of the expression or the
   --  subtype) when the expression is analyzed.
   --  Field: Field2 Ref
   function Get_Allocator_Designated_Type (Target : Iir) return Iir;
   procedure Set_Allocator_Designated_Type (Target : Iir; A_Type : Iir);

   --  Field: Field7 Chain
   function Get_Selected_Waveform_Chain (Target : Iir) return Iir;
   procedure Set_Selected_Waveform_Chain (Target : Iir; Chain : Iir);

   --  Field: Field5 Chain
   function Get_Conditional_Waveform_Chain (Target : Iir) return Iir;
   procedure Set_Conditional_Waveform_Chain (Target : Iir; Chain : Iir);

   --  Expression defining the value of the implicit guard signal.
   --  Field: Field2
   function Get_Guard_Expression (Target : Iir) return Iir;
   procedure Set_Guard_Expression (Target : Iir; Expr : Iir);

   --  The declaration (if any) of the implicit guard signal of a block
   --  statement.
   --  Field: Field8
   function Get_Guard_Decl (Target : Iir_Block_Statement) return Iir;
   procedure Set_Guard_Decl (Target : Iir_Block_Statement; Decl : Iir);

   --  Sensitivity list for the implicit guard signal.
   --  Field: Field4 Of_Ref (uc)
   function Get_Guard_Sensitivity_List (Guard : Iir) return Iir_List;
   procedure Set_Guard_Sensitivity_List (Guard : Iir; List : Iir_List);

   --  Field: Field3 Forward_Ref
   function Get_Signal_Attribute_Chain (Decl : Iir) return Iir;
   procedure Set_Signal_Attribute_Chain (Decl : Iir; Chain : Iir);

   --  Block_Configuration that applies to this block statement.
   --  Field: Field6 Forward_Ref
   function Get_Block_Block_Configuration (Block : Iir) return Iir;
   procedure Set_Block_Block_Configuration (Block : Iir; Conf : Iir);

   --  Field: Field6
   function Get_Package_Header (Pkg : Iir) return Iir;
   procedure Set_Package_Header (Pkg : Iir; Header : Iir);

   --  Field: Field7
   function Get_Block_Header (Target : Iir) return Iir;
   procedure Set_Block_Header (Target : Iir; Header : Iir);

   --  Field: Field7
   function Get_Uninstantiated_Package_Name (Inst : Iir) return Iir;
   procedure Set_Uninstantiated_Package_Name (Inst : Iir; Name : Iir);

   --  Field: Field9 Ref
   function Get_Uninstantiated_Package_Decl (Inst : Iir) return Iir;
   procedure Set_Uninstantiated_Package_Decl (Inst : Iir; Pkg : Iir);

   --  The created pseudo-file for relocating the instantiated nodes
   --  (generics and declarations).
   --  Field: Field10 (uc)
   function Get_Instance_Source_File (Inst : Iir) return Source_File_Entry;
   procedure Set_Instance_Source_File (Inst : Iir; File : Source_File_Entry);

   --  Get/Set the block_configuration (there may be several
   --  block_configuration through the use of prev_configuration singly linked
   --  list) that apply to this generate statement.
   --  Field: Field2 Forward_Ref
   function Get_Generate_Block_Configuration (Target : Iir) return Iir;
   procedure Set_Generate_Block_Configuration (Target : Iir; Conf : Iir);

   --  Field: Field4
   function Get_Generate_Statement_Body (Target : Iir) return Iir;
   procedure Set_Generate_Statement_Body (Target : Iir; Bod : Iir);

   --  Field: Field3 (uc)
   function Get_Alternative_Label (Target : Iir) return Name_Id;
   procedure Set_Alternative_Label (Target : Iir; Label : Name_Id);

   --  Field: Field5
   function Get_Generate_Else_Clause (Target : Iir) return Iir;
   procedure Set_Generate_Else_Clause (Target : Iir; Clause : Iir);

   --  Condition of a conditional_waveform, if_statement, elsif,
   --  while_loop_statement, next_statement or exit_statement.
   --  Field: Field1 Maybe_Ref
   function Get_Condition (Target : Iir) return Iir;
   procedure Set_Condition (Target : Iir; Condition : Iir);

   --  Field: Field5
   function Get_Else_Clause (Target : Iir) return Iir;
   procedure Set_Else_Clause (Target : Iir; Clause : Iir);

   --  Iterator of a for_loop_statement.
   --  Field: Field1
   function Get_Parameter_Specification (Target : Iir) return Iir;
   procedure Set_Parameter_Specification (Target : Iir; Param : Iir);

   --  Get/Set the statement in which TARGET appears.  This is used to check
   --  if next/exit is in a loop.
   --  Field: Field0 Ref
   function Get_Parent (Target : Iir) return Iir;
   procedure Set_Parent (Target : Iir; Parent : Iir);

   --  Loop label for an exit_statement or next_statement.
   --  Field: Field5
   function Get_Loop_Label (Target : Iir) return Iir;
   procedure Set_Loop_Label (Target : Iir; Stmt : Iir);

   --  True if there is an exit statement targeting this loop statement.
   --  Field: Flag1
   function Get_Exit_Flag (Stmt : Iir) return Boolean;
   procedure Set_Exit_Flag (Stmt : Iir; Flag : Boolean);

   --  True if there is a next statement targeting this loop statement.
   --  Field: Flag2
   function Get_Next_Flag (Stmt : Iir) return Boolean;
   procedure Set_Next_Flag (Stmt : Iir; Flag : Boolean);

   --  Component name for a component_configuration or
   --  a configuration_specification.
   --  Field: Field5
   function Get_Component_Name (Target : Iir) return Iir;
   procedure Set_Component_Name (Target : Iir; Name : Iir);

   --  Field: Field1 (uc)
   function Get_Instantiation_List (Target : Iir) return Iir_Flist;
   procedure Set_Instantiation_List (Target : Iir; List : Iir_Flist);

   --  Field: Field3
   function Get_Entity_Aspect (Target : Iir_Binding_Indication) return Iir;
   procedure Set_Entity_Aspect (Target : Iir_Binding_Indication; Entity : Iir);

   --  Field: Field1
   function Get_Default_Entity_Aspect (Target : Iir) return Iir;
   procedure Set_Default_Entity_Aspect (Target : Iir; Aspect : Iir);

   --  Field: Field3 Maybe_Ref
   function Get_Binding_Indication (Target : Iir) return Iir;
   procedure Set_Binding_Indication (Target : Iir; Binding : Iir);

   --  The named entity designated by a name.
   --  Field: Field4 Maybe_Forward_Ref
   function Get_Named_Entity (Name : Iir) return Iir;
   procedure Set_Named_Entity (Name : Iir; Val : Iir);

   --  Field: Field2 Ref
   function Get_Referenced_Name (N : Iir) return Iir;
   procedure Set_Referenced_Name (N : Iir; Name : Iir);

   --  Expression staticness, defined by rules of LRM 7.4
   --  Field: State1 (pos)
   function Get_Expr_Staticness (Target : Iir) return Iir_Staticness;
   procedure Set_Expr_Staticness (Target : Iir; Static : Iir_Staticness);

   --  Field: Flag6,Flag7 (grp)
   function Get_Scalar_Size (N : Iir) return Scalar_Size;
   procedure Set_Scalar_Size (N : Iir; Sz : Scalar_Size);

   --  Node which couldn't be correctly analyzed.
   --  Field: Field2
   function Get_Error_Origin (Target : Iir) return Iir;
   procedure Set_Error_Origin (Target : Iir; Origin : Iir);

   --  Operand of a monadic operator.
   --  Field: Field2
   function Get_Operand (Target : Iir) return Iir;
   procedure Set_Operand (Target : Iir; An_Iir : Iir);

   --  Left operand of a dyadic operator.
   --  Field: Field2
   function Get_Left (Target : Iir) return Iir;
   procedure Set_Left (Target : Iir; An_Iir : Iir);

   --  Right operand of a dyadic operator.
   --  Field: Field4
   function Get_Right (Target : Iir) return Iir;
   procedure Set_Right (Target : Iir; An_Iir : Iir);

   --  Field: Field3
   function Get_Unit_Name (Target : Iir) return Iir;
   procedure Set_Unit_Name (Target : Iir; Name : Iir);

   --  Field: Field4
   function Get_Name (Target : Iir) return Iir;
   procedure Set_Name (Target : Iir; Name : Iir);

   --  Field: Field5
   function Get_Group_Template_Name (Target : Iir) return Iir;
   procedure Set_Group_Template_Name (Target : Iir; Name : Iir);

   --  Staticness of a name, according to rules of LRM 6.1
   --  Field: State2 (pos)
   function Get_Name_Staticness (Target : Iir) return Iir_Staticness;
   procedure Set_Name_Staticness (Target : Iir; Static : Iir_Staticness);

   --  Prefix of a name.
   --  Field: Field0
   function Get_Prefix (Target : Iir) return Iir;
   procedure Set_Prefix (Target : Iir; Prefix : Iir);

   --  Prefix of a name signature
   --  Field: Field1
   function Get_Signature_Prefix (Sign : Iir) return Iir;
   procedure Set_Signature_Prefix (Sign : Iir; Prefix : Iir);

   --  External pathname for an external name.
   --  Field: Field3
   function Get_External_Pathname (Name : Iir) return Iir;
   procedure Set_External_Pathname (Name : Iir; Path : Iir);

   --  Field: Field2
   function Get_Pathname_Suffix (Path : Iir) return Iir;
   procedure Set_Pathname_Suffix (Path : Iir; Suffix : Iir);

   --  Field: Field5
   function Get_Pathname_Expression (Path : Iir) return Iir;
   procedure Set_Pathname_Expression (Path : Iir; Expr : Iir);

   --  True if the name appears in a formal_part.  In that case, some
   --  checks must be disabled (eg: the expression of a type conversion can
   --  be a write-only interface).
   --  Field: Flag4
   function Get_In_Formal_Flag (Name : Iir) return Boolean;
   procedure Set_In_Formal_Flag (Name : Iir; Flag : Boolean);

   --  The subtype of a slice.  Contrary to the Type field, this is not a
   --  reference.
   --  Field: Field3
   function Get_Slice_Subtype (Slice : Iir) return Iir;
   procedure Set_Slice_Subtype (Slice : Iir; Atype : Iir);

   --  Suffix of a slice or attribute.
   --  Field: Field2
   function Get_Suffix (Target : Iir) return Iir;
   procedure Set_Suffix (Target : Iir; Suffix : Iir);

   --  Set the designated index subtype of an array attribute.
   --  Field: Field2 Ref
   function Get_Index_Subtype (Attr : Iir) return Iir;
   procedure Set_Index_Subtype (Attr : Iir; St : Iir);

   --  Parameter of an attribute.
   --  Field: Field4
   function Get_Parameter (Target : Iir) return Iir;
   procedure Set_Parameter (Target : Iir; Param : Iir);

   --  Second parameter of an attribute (for AMS VHDL).
   --  Field: Field6
   function Get_Parameter_2 (Target : Iir) return Iir;
   procedure Set_Parameter_2 (Target : Iir; Param : Iir);

   --  Third parameter of an attribute (for AMS VHDL).
   --  Field: Field7
   function Get_Parameter_3 (Target : Iir) return Iir;
   procedure Set_Parameter_3 (Target : Iir; Param : Iir);

   --  Fourth parameter of an attribute (for AMS VHDL).
   --  Field: Field8
   function Get_Parameter_4 (Target : Iir) return Iir;
   procedure Set_Parameter_4 (Target : Iir; Param : Iir);

   --  Field: Field2 Forward_Ref
   function Get_Attr_Chain (Attr : Iir) return Iir;
   procedure Set_Attr_Chain (Attr : Iir; Chain : Iir);

   --  Field: Field3 Forward_Ref
   function Get_Signal_Attribute_Declaration (Attr : Iir) return Iir;
   procedure Set_Signal_Attribute_Declaration (Attr : Iir; Decl : Iir);

   --  Type of the actual for an association by individual.
   --    Unless the formal is an unconstrained array type, this is the same as
   --    the formal type.
   --  Subtype indication for a type association.
   --  Field: Field5 Ref
   function Get_Actual_Type (Target : Iir) return Iir;
   procedure Set_Actual_Type (Target : Iir; Atype : Iir);

   --  Field: Field3
   function Get_Actual_Type_Definition (Target : Iir) return Iir;
   procedure Set_Actual_Type_Definition (Target : Iir; Atype : Iir);

   --  List of individual associations for association_element_by_individual.
   --  Associations for parenthesis_name.
   --  Field: Field2 Chain
   function Get_Association_Chain (Target : Iir) return Iir;
   procedure Set_Association_Chain (Target : Iir; Chain : Iir);

   --  List of choices for association_element_by_individual.
   --  Field: Field4 Chain
   function Get_Individual_Association_Chain (Target : Iir) return Iir;
   procedure Set_Individual_Association_Chain (Target : Iir; Chain : Iir);

   --  Chain of implicit subprogram associations for a type association.
   --  Field: Field4 Chain
   function Get_Subprogram_Association_Chain (Target : Iir) return Iir;
   procedure Set_Subprogram_Association_Chain (Target : Iir; Chain : Iir);

   --  Get/Set info for the aggregate.
   --  There is one aggregate_info for for each dimension.
   --  Field: Field5
   function Get_Aggregate_Info (Target : Iir) return Iir;
   procedure Set_Aggregate_Info (Target : Iir; Info : Iir);

   --  Get/Set the info node for the next dimension.
   --  Field: Field1
   function Get_Sub_Aggregate_Info (Target : Iir) return Iir;
   procedure Set_Sub_Aggregate_Info (Target : Iir; Info : Iir);

   --  TRUE when the length of the aggregate is not locally static.
   --  Field: Flag3
   function Get_Aggr_Dynamic_Flag (Target : Iir) return Boolean;
   procedure Set_Aggr_Dynamic_Flag (Target : Iir; Val : Boolean);

   --  Get/Set the minimum number of elements for the lowest dimension of
   --  the aggregate or for the current dimension of a sub-aggregate.
   --  The real number of elements may be greater than this number if there
   --  is an 'other' choice.
   --  Field: Field4 (uc)
   function Get_Aggr_Min_Length (Info : Iir_Aggregate_Info) return Iir_Int32;
   procedure Set_Aggr_Min_Length (Info : Iir_Aggregate_Info; Nbr : Iir_Int32);

   --  Highest index choice, if any.
   --  Field: Field2 Ref
   function Get_Aggr_Low_Limit (Target : Iir_Aggregate_Info) return Iir;
   procedure Set_Aggr_Low_Limit (Target : Iir_Aggregate_Info; Limit : Iir);

   --  Highest index choice, if any.
   --  Field: Field3 Ref
   function Get_Aggr_High_Limit (Target : Iir_Aggregate_Info) return Iir;
   procedure Set_Aggr_High_Limit (Target : Iir_Aggregate_Info; Limit : Iir);

   --  True if the aggregate has an 'others' choice.
   --  Field: Flag2
   function Get_Aggr_Others_Flag (Target : Iir_Aggregate_Info) return Boolean;
   procedure Set_Aggr_Others_Flag (Target : Iir_Aggregate_Info; Val : Boolean);

   --  True if the aggregate have named associations.
   --  Field: Flag4
   function Get_Aggr_Named_Flag (Target : Iir_Aggregate_Info) return Boolean;
   procedure Set_Aggr_Named_Flag (Target : Iir_Aggregate_Info; Val : Boolean);

   --  True if the aggregate can be statically built.
   --  Field: Flag1
   function Get_Aggregate_Expand_Flag (Aggr : Iir) return Boolean;
   procedure Set_Aggregate_Expand_Flag (Aggr : Iir; Flag : Boolean);

   --  Chain of choices.
   --  Field: Field4 Chain
   function Get_Association_Choices_Chain (Target : Iir) return Iir;
   procedure Set_Association_Choices_Chain (Target : Iir; Chain : Iir);

   --  Chain of choices.
   --  Field: Field1 Chain
   function Get_Case_Statement_Alternative_Chain (Target : Iir) return Iir;
   procedure Set_Case_Statement_Alternative_Chain (Target : Iir; Chain : Iir);

   --  Matching condition for case statement.
   --  Field: Flag1
   function Get_Matching_Flag (Target : Iir) return Boolean;
   procedure Set_Matching_Flag (Target : Iir; Flag : Boolean);

   --  Staticness of the choice.
   --  Field: State1 (pos)
   function Get_Choice_Staticness (Target : Iir) return Iir_Staticness;
   procedure Set_Choice_Staticness (Target : Iir; Staticness : Iir_Staticness);

   --  Field: Field1
   function Get_Procedure_Call (Stmt : Iir) return Iir;
   procedure Set_Procedure_Call (Stmt : Iir; Call : Iir);

   --  Subprogram to be called by a procedure, function call or operator.  This
   --  is the declaration of the subprogram (or a list of during analysis).
   --  Field: Field3 Ref
   function Get_Implementation (Target : Iir) return Iir;
   procedure Set_Implementation (Target : Iir; Decl : Iir);

   --  Parameter associations for procedure and function call.
   --  Field: Field2 Chain
   function Get_Parameter_Association_Chain (Target : Iir) return Iir;
   procedure Set_Parameter_Association_Chain (Target : Iir; Chain : Iir);

   --  Object of a method call.  NULL_IIR if the subprogram is not a method.
   --  Field: Field4 Ref
   function Get_Method_Object (Target : Iir) return Iir;
   procedure Set_Method_Object (Target : Iir; Object : Iir);

   --  The type_mark that appeared in the subtype indication.  This is a name.
   --  May be null_iir if there is no type mark (as in an iterator).
   --  Field: Field2
   function Get_Subtype_Type_Mark (Target : Iir) return Iir;
   procedure Set_Subtype_Type_Mark (Target : Iir; Mark : Iir);

   --  Field: Field2
   function Get_Subnature_Nature_Mark (Target : Iir) return Iir;
   procedure Set_Subnature_Nature_Mark (Target : Iir; Mark : Iir);

   --  Field: Field3
   function Get_Type_Conversion_Subtype (Target : Iir) return Iir;
   procedure Set_Type_Conversion_Subtype (Target : Iir; Atype : Iir);

   --  The type_mark that appeared in qualified expressions or type
   --  conversions.
   --  Field: Field4
   function Get_Type_Mark (Target : Iir) return Iir;
   procedure Set_Type_Mark (Target : Iir; Mark : Iir);

   --  The type of values for a type file.
   --  Field: Field2
   function Get_File_Type_Mark (Target : Iir) return Iir;
   procedure Set_File_Type_Mark (Target : Iir; Mark : Iir);

   --  Field: Field8
   function Get_Return_Type_Mark (Target : Iir) return Iir;
   procedure Set_Return_Type_Mark (Target : Iir; Mark : Iir);

   --  This flag is set on a signal_declaration, when a disconnection
   --  specification applies to the signal (or a subelement of it).
   --  This is used to check 'others' and 'all' designators.
   --  Field: Flag1
   function Get_Has_Disconnect_Flag (Target : Iir) return Boolean;
   procedure Set_Has_Disconnect_Flag (Target : Iir; Val : Boolean);

   --  This flag is set on a signal when its activity is read by the user.
   --  Some signals handling can be optimized when this flag is set.
   --  Field: Flag2
   function Get_Has_Active_Flag (Target : Iir) return Boolean;
   procedure Set_Has_Active_Flag (Target : Iir; Val : Boolean);

   --  This flag is set is code being analyzed is textually within TARGET.
   --  This is used for selected by name rule.
   --  Field: Flag5
   function Get_Is_Within_Flag (Target : Iir) return Boolean;
   procedure Set_Is_Within_Flag (Target : Iir; Val : Boolean);

   --  List of type_mark for an Iir_Kind_Signature
   --  Field: Field2 (uc)
   function Get_Type_Marks_List (Target : Iir) return Iir_Flist;
   procedure Set_Type_Marks_List (Target : Iir; List : Iir_Flist);

   --  Field: Flag1
   function Get_Implicit_Alias_Flag (Decl : Iir) return Boolean;
   procedure Set_Implicit_Alias_Flag (Decl : Iir; Flag : Boolean);

   --  Field: Field5
   function Get_Alias_Signature (Alias : Iir) return Iir;
   procedure Set_Alias_Signature (Alias : Iir; Signature : Iir);

   --  Field: Field2
   function Get_Attribute_Signature (Attr : Iir) return Iir;
   procedure Set_Attribute_Signature (Attr : Iir; Signature : Iir);

   --  Field: Field1 Of_Ref (uc)
   function Get_Overload_List (Target : Iir) return Iir_List;
   procedure Set_Overload_List (Target : Iir; List : Iir_List);

   --  Identifier of the simple_name attribute.
   --  Field: Field3 (uc)
   function Get_Simple_Name_Identifier (Target : Iir) return Name_Id;
   procedure Set_Simple_Name_Identifier (Target : Iir; Ident : Name_Id);

   --  Subtype for Simple_Name attribute.
   --  Field: Field4
   function Get_Simple_Name_Subtype (Target : Iir) return Iir;
   procedure Set_Simple_Name_Subtype (Target : Iir; Atype : Iir);

   --  Body of a protected type declaration.
   --  Field: Field2 Forward_Ref
   function Get_Protected_Type_Body (Target : Iir) return Iir;
   procedure Set_Protected_Type_Body (Target : Iir; Bod : Iir);

   --  Corresponding protected type declaration of a protected type body.
   --  Field: Field4 Ref
   function Get_Protected_Type_Declaration (Target : Iir) return Iir;
   procedure Set_Protected_Type_Declaration (Target : Iir; Decl : Iir);

   --  For a declaration: true if the declaration is used somewhere.
   --  Field: Flag6
   function Get_Use_Flag (Decl : Iir) return Boolean;
   procedure Set_Use_Flag (Decl : Iir; Val : Boolean);

   --  Layout flag: true if 'end' is followed by the reserved identifier.
   --  Field: Flag8
   function Get_End_Has_Reserved_Id (Decl : Iir) return Boolean;
   procedure Set_End_Has_Reserved_Id (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'end' is followed by the identifier.
   --  Field: Flag9
   function Get_End_Has_Identifier (Decl : Iir) return Boolean;
   procedure Set_End_Has_Identifier (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'end' is followed by 'postponed'.
   --  Field: Flag10
   function Get_End_Has_Postponed (Decl : Iir) return Boolean;
   procedure Set_End_Has_Postponed (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if a label is present.
   --  Field: Flag6
   function Get_Has_Label (Decl : Iir) return Boolean;
   procedure Set_Has_Label (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'begin' is present.
   --  Field: Flag10
   function Get_Has_Begin (Decl : Iir) return Boolean;
   procedure Set_Has_Begin (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'end' is present (only for generate body).
   --  Field: Flag11
   function Get_Has_End (Decl : Iir) return Boolean;
   procedure Set_Has_End (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'is' is present.
   --  Field: Flag7
   function Get_Has_Is (Decl : Iir) return Boolean;
   procedure Set_Has_Is (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'pure' or 'impure' is present.
   --  Field: Flag8
   function Get_Has_Pure (Decl : Iir) return Boolean;
   procedure Set_Has_Pure (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if body appears just after the specification.
   --  Field: Flag9
   function Get_Has_Body (Decl : Iir) return Boolean;
   procedure Set_Has_Body (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'parameter' reserved identifier is present.
   --  Field: Flag10
   function Get_Has_Parameter (Decl : Iir) return Boolean;
   procedure Set_Has_Parameter (Decl : Iir; Flag : Boolean);

   --  Layout flag: true if 'component' reserved identifier is present.
   --  Field: Flag5
   function Get_Has_Component (Decl : Iir) return Boolean;
   procedure Set_Has_Component (Decl : Iir; Flag : Boolean);

   --  Layout flag for object declaration.  If True, the identifier of this
   --  declaration is followed by an identifier (and separated by a comma).
   --  This flag is set on all but the last declarations.
   --  Eg: on 'signal A, B, C : Bit', the flag is set on A and B (but not C).
   --  Field: Flag3
   function Get_Has_Identifier_List (Decl : Iir) return Boolean;
   procedure Set_Has_Identifier_List (Decl : Iir; Flag : Boolean);

   --  Layout flag for object declaration.  If True, the mode is present.
   --  Field: Flag10
   function Get_Has_Mode (Decl : Iir) return Boolean;
   procedure Set_Has_Mode (Decl : Iir; Flag : Boolean);

   --  Layout flag for object declaration.  If True, the object class is
   --  present.
   --  Field: Flag11
   function Get_Has_Class (Decl : Iir) return Boolean;
   procedure Set_Has_Class (Decl : Iir; Flag : Boolean);

   --  Layout flag for signal assignment.  If True, the delay mechanism is
   --  present.  This is obviously true for transport or inertial with reject,
   --  but the simple 'inertial' is optional.
   --  Field: Flag2
   function Get_Has_Delay_Mechanism (Stmt : Iir) return Boolean;
   procedure Set_Has_Delay_Mechanism (Stmt : Iir; Flag : Boolean);

   --  Set on wait, procedure call and composite statements when there is a
   --  sub-statement that can suspend a procedure or a process.  Also set
   --  on procedure declaration.  Note that the flag is conservative: it must
   --  be true if the node contains directly or indirectly a wait statement,
   --  but need not to be false otherwise.
   --  Field: Flag11
   function Get_Suspend_Flag (Stmt : Iir) return Boolean;
   procedure Set_Suspend_Flag (Stmt : Iir; Flag : Boolean);

   --  Set to True if Maybe_Ref fields are references.  This cannot be shared
   --  with Has_Identifier_List as: Is_Ref is set to True on all items but
   --  the first, while Has_Identifier_List is set to True on all items but
   --  the last.  Furthermore Is_Ref appears in nodes where Has_Identifier_List
   --  is not present.
   --  Field: Flag12
   function Get_Is_Ref (N : Iir) return Boolean;
   procedure Set_Is_Ref (N : Iir; Ref : Boolean);

   --  Field: Flag1
   function Get_Is_Forward_Ref (N : Iir) return Boolean;
   procedure Set_Is_Forward_Ref (N : Iir; Ref : Boolean);

   --  Field: Field1 (uc)
   function Get_Psl_Property (Decl : Iir) return PSL_Node;
   procedure Set_Psl_Property (Decl : Iir; Prop : PSL_Node);

   --  Field: Field1 (uc)
   function Get_Psl_Sequence (Decl : Iir) return PSL_Node;
   procedure Set_Psl_Sequence (Decl : Iir; Prop : PSL_Node);

   --  Field: Field6 (uc)
   function Get_Psl_Declaration (Decl : Iir) return PSL_Node;
   procedure Set_Psl_Declaration (Decl : Iir; Prop : PSL_Node);

   --  Field: Field3 (uc)
   function Get_Psl_Expression (Decl : Iir) return PSL_Node;
   procedure Set_Psl_Expression (Decl : Iir; Prop : PSL_Node);

   --  Field: Field1 (uc)
   function Get_Psl_Boolean (N : Iir) return PSL_Node;
   procedure Set_Psl_Boolean (N : Iir; Bool : PSL_Node);

   --  Field: Field7 (uc)
   function Get_PSL_Clock (N : Iir) return PSL_Node;
   procedure Set_PSL_Clock (N : Iir; Clock : PSL_Node);

   --  Field: Field8 (uc)
   function Get_PSL_NFA (N : Iir) return PSL_NFA;
   procedure Set_PSL_NFA (N : Iir; Fa : PSL_NFA);

   --  Field: Field9 (uc)
   function Get_PSL_Nbr_States (N : Iir) return Int32;
   procedure Set_PSL_Nbr_States (N : Iir; Nbr : Int32);

   --  Field: Field10 (uc)
   function Get_PSL_Clock_Sensitivity (N : Iir) return Iir_List;
   procedure Set_PSL_Clock_Sensitivity (N : Iir; List : Iir_List);

   --  Field: Flag1
   function Get_PSL_EOS_Flag (N : Iir) return Boolean;
   procedure Set_PSL_EOS_Flag (N : Iir; Flag : Boolean);

   --  Field: Flag2
   function Get_PSL_Abort_Flag (N : Iir) return Boolean;
   procedure Set_PSL_Abort_Flag (N : Iir; Flag : Boolean);

   --  Field: Field2
   function Get_Count_Expression (N : Iir) return Iir;
   procedure Set_Count_Expression (N : Iir; Count : Iir);

   --  Field: Field4
   function Get_Clock_Expression (N : Iir) return Iir;
   procedure Set_Clock_Expression (N : Iir; Clk : Iir);

   --  Reference to the default_clock node.
   --  Field: Field3 Ref
   function Get_Default_Clock (N : Iir) return Iir;
   procedure Set_Default_Clock (N : Iir; Clk : Iir);

   --  Field: Field1 (uc)
   function Get_Foreign_Node (N : Iir) return Int32;
   procedure Set_Foreign_Node (N : Iir; En : Int32);

   --  Field: Field3 (uc)
   function Get_Suspend_State_Index (N : Iir) return Int32;
   procedure Set_Suspend_State_Index (N : Iir; Num : Int32);

   --  Field: Field4 Forward_Ref
   function Get_Suspend_State_Chain (N : Iir) return Iir;
   procedure Set_Suspend_State_Chain (N : Iir; Chain : Iir);
end Vhdl.Nodes;
