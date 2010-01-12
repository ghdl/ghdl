--  Tree node definitions.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ada.Unchecked_Deallocation;
with Types; use Types;
with Tokens; use Tokens;
with Nodes;
with Lists;

package Iirs is
   -- This package defines the semantic tree and functions to handle it.
   -- The tree is roughly based on IIR (Internal Intermediate Representation),
   -- [AIRE/CE Advanced Intermediate Representation with Extensibility,
   --  Common Environment.  http://www.vhdl.org/aire/index.html ]
   -- but oriented object features are not used, and sometimes, functions
   -- or fields have changed.

   -- Note: this tree is also used during syntaxic analysis, but with
   -- a little bit different meanings for the fields.
   -- The parser (parse package) build the tree.
   -- The semantic pass (sem, sem_expr, sem_name) transforms it into a
   -- semantic tree.

   -- Documentation:
   -- Only the semantic aspect is to be fully documented.
   -- The syntaxic aspect is only used between parse and sem.

   -- Each node of the tree is a record of type iir.  The record has only
   -- one discriminent, which contains the kind of the node.  There is
   -- currenlty no variant (but this can change, this is not public).

   -- The root of a semantic tree is a library_declaration.
   -- All the library_declarations are kept in a private list, held by
   -- package libraries.
   -- Exemple of a tree:
   --   library_declaration
   --   +-- design_file
   --       +-- design_unit
   --       |   +-- entity_declaration
   --       +-- design_unit
   --           +-- architecture_declaration
   -- ...

   -- Since the tree can represent all the libraries and their contents, it
   -- is not always loaded into memory.
   -- When a library is loaded, only library_declaration, design_file,
   -- design_unit and library_unit nodes are created.  When a design_unit is
   -- really loaded, the design_unit node is not replaced but modified (ie,
   -- access to this node are still valid).

   -- To add a new kind of node:
   --   the name should be of the form iir_kind_NAME
   --   add iir_kind_NAME in the definition of type iir_kind_type
   --   add a declaration of access type of name iir_kind_NAME_acc
   --   document the node below: grammar, methods.
   --   for each methods, add the name if the case statement in the body
   --     (this enables the methods)
   --   add an entry in create_iir and free_iir
   --   add an entry in disp_tree (debugging)

   -------------------------------------------------
   -- General methods (can be used on all nodes): --
   -------------------------------------------------

   -- Create a node of kind KIND.
   --   function Create_Iir (Kind: Iir_Kind) return Iir;
   --
   -- Deallocate a node.  Deallocate fields that where allocated by create_iir.
   --   procedure Free_Iir (Target: in out Iir);
   --
   -- Get the kind of the iir.
   -- See below for the (public) list of kinds.
   --   function Get_Kind (An_Iir: Iir) return Iir_Kind;

   -- Get the location of the node: ie the current position in the source
   -- file when the node was created.  This is a little bit fuzzy.
   --
   --   procedure Set_Location (Target: in out Iir; Location: Location_Type);
   --   function Get_Location (Target: in out Iir) return Location_Type;
   --
   -- Copy a location from a node to another one.
   --   procedure Location_Copy (Target: in out Iir; Src: in Iir);


   -- The next line marks the start of the node description.
   -- Start of Iir_Kind.

   -------------------------------------------------
   -- A set of methods are associed with a kind.  --
   -------------------------------------------------

   -- Iir_Kind_Design_File (Medium)
   -- LRM93 11
   -- DESIGN_FILE ::= DESIGN_UNIT { DESIGN_UNIT}
   --
   -- The library containing this design file.
   --   Get/Set_Library (Field0)
   --   Get/Set_Parent (Alias Field0)
   --
   --   Get/Set_File_Dependence_List (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Analysis_Time_Stamp (Field3)
   --
   --   Get/Set_File_Time_Stamp (Field4)
   --
   -- Get the chain of unit contained in the file.  This is a simply linked
   -- chain, but the tail is kept to speed-up appending operation.
   --   Get/Set_First_Design_Unit (Field5)
   --
   --   Get/Set_Last_Design_Unit (Field6)
   --
   -- Identifier for the design file file name and dirname.
   --   Get/Set_Design_File_Filename (Field12)
   --   Get/Set_Design_File_Directory (Field11)
   --
   -- Flag used during elaboration.  Set when the file was already seen.
   --   Get/Set_Elab_Flag (Flag3)

   -- Iir_Kind_Design_Unit (Medium)
   -- LRM93 11
   -- DESIGN_UNIT ::= CONTEXT_CLAUSE LIBRARY_UNIT
   --
   -- The design_file containing this design unit.
   --   Get/Set_Design_File (Field0)
   --   Get/Set_Parent (Alias Field0)
   --
   -- Get the chain of context clause.
   --   Get_Context_Items (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   -- Get/Set the library unit, which can be an entity, an architecture,
   -- a package, a package body or a configuration.
   --   Get/Set_Library_Unit (Field5)
   --
   --   Get/Set_End_Location (Field6)
   --
   -- Collision chain for units.
   --   Get/Set_Hash_Chain (Field7)
   --
   -- Get the list of design units that must be analysed before this unit.
   -- See LRM93 11.4 for the rules defining the order of analysis.
   --   Get/Set_Dependence_List (Field8)
   --
   -- FIXME: this field can be put in the library_unit, since it is only used
   -- when the units have been analyzed.
   --   Get/Set_Analysis_Checks_List (Field9)
   --
   -- This is a symbolic date, only used as a order of analysis of design
   -- units.
   --   Get/Set_Date (Field10)
   --
   -- Set the line and the offset in the line, only for the library manager.
   -- This is valid until the file is really loaded in memory.  On loading,
   -- location will contain all this informations.
   -- Get/Set_Pos_Line_Off (Field1,Field11,Field12)
   --
   -- Get/Set the date state, which indicates whether this design unit is in
   -- memory or not.
   --   Get/Set_Date_State (State1)
   --
   -- Flag used during elaboration.  Set when the file was already seen.
   --   Get/Set_Elab_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Library_Clause (Short)
   -- Note: a library_clause node is created for every logical_name.
   -- As a consequence, the scope of the library starts after the logical_name
   -- and not after the library_clause.  However, since an identifier
   -- can only be used as a logical_name, and since the second occurence has
   -- no effect, this is correct.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Library_Declaration (Field1)
   --
   --   Get/Set_Chain (Field2)

   --------------
   -- Literals --
   --------------

   -- Iir_Kind_Character_Literal (Short)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Type (Field1)

   -- Iir_Kind_String_Literal (Short)
   -- Iir_Kind_Bit_String_Literal (Medium)
   --
   --   Get/Set_Type (Field1)
   --
   -- Used for computed literals.  Literal_Origin contains the expression whose
   -- value was computed during analysis and replaces the expression.
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_String_Id (Field3)
   --
   -- As bit-strings are expanded to '0'/'1' strings, this is the number of
   -- characters.
   --   Get/Set_String_Length (Field0)
   --
   -- For bit string only:
   -- Enumeration literal which correspond to '0' and '1'.
   -- This cannot be defined only in the enumeration type definition, due to
   -- possible aliases.
   -- Only for Iir_Kind_Bit_String_Literal:
   --   Get/Set_Bit_String_0 (Field4)
   -- Only for Iir_Kind_Bit_String_Literal:
   --   Get/Set_Bit_String_1 (Field5)
   --
   -- Only for Iir_Kind_Bit_String_Literal:
   --   Get/Set_Bit_String_Base (Field11)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Integer_Literal (Int)
   --   Get/Set_Type (Field1)
   --
   -- Get/Set the value of the integer.
   --   Get/Set_Value (Int64)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Floating_Point_Literal (Fp)
   --   Get/Set_Type (Field1)
   --
   -- Get/Set the value of the literal.
   --   Get/Set_Fp_Value (Fp64)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Null_Literal (Short)
   -- The null literal, which can be a disconnection or a null access.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Physical_Int_Literal (Int)
   -- Iir_Kind_Physical_Fp_Literal (Fp)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   -- Get/Set the physical unit of the literal.
   --   Get/Set_Unit_Name (Field3)
   --
   -- Must be set to locally except for time literal, which is globally.
   --   Get/Set_Expr_Staticness (State1)
   --
   -- Only for Iir_Kind_Physical_Int_Literal:
   -- The multiplicand.
   --   Get/Set_Value (Int64)
   --
   -- Only for Iir_Kind_Physical_Fp_Literal:
   -- The multiplicand.
   --   Get/Set_Fp_Value (Fp64)

   -- Iir_Kind_Simple_Aggregate (Short)
   -- This node can only be generated by evaluation: it is an unidimentional
   -- positional aggregate.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   -- List of elements
   --   Get/Set_Simple_Aggregate_List (Field3)

   ------------
   -- Tuples --
   ------------

   -- Iir_Kind_Association_Element_By_Expression (Short)
   -- Iir_Kind_Association_Element_Open (Short)
   -- Iir_Kind_Association_Element_By_Individual (Short)
   -- These are used for association element of an association list with
   -- an interface (ie subprogram call, port map, generic map).
   --
   --   Get/Set_Formal (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   -- Only for Iir_Kind_Association_Element_By_Expression:
   --   Get/Set_Actual (Field3)
   --
   -- Only for Iir_Kind_Association_Element_By_Individual:
   --   Get/Set_Actual_Type (Field3)
   --
   -- Only for Iir_Kind_Association_Element_By_Individual:
   --   Get/Set_Individual_Association_Chain (Field4)
   --
   -- Only for Iir_Kind_Association_Element_By_Expression:
   --   Get/Set_In_Conversion (Field4)
   --
   -- Only for Iir_Kind_Association_Element_By_Expression:
   --   Get/Set_Out_Conversion (Field5)
   --
   -- Get/Set the whole association flag (true if the formal is associated in
   -- whole, see LRM 4.3.2.2)
   --   Get/Set_Whole_Association_Flag (Flag1)
   --
   --   Get/Set_Collapse_Signal_Flag (Flag2)
   --
   -- Only for Iir_Kind_Association_Element_Open:
   --   Get/Set_Artificial_Flag (Flag3)

   -- Iir_Kind_Proxy (Short)
   -- A proxy is used to avoid duplication of a node.
   -- Ex: instead of copying a default value of an insterface in the subprogram
   -- call, a proxy is used.  The default value can't be so easily aliased
   -- due to annotation.
   --
   -- Create a proxy for PROXY.
   --   function Create_Proxy (Proxy: Iir) return Iir_Proxy;
   --
   -- Get/Set the value of the proxy.
   --   Get/Set_Proxy (Field1)

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

   -- Iir_Kind_Choice_By_Others (Short)
   -- Iir_Kind_Choice_By_None (Short)
   -- Iir_Kind_Choice_By_Range (Short)
   -- Iir_Kind_Choice_By_Name (Short)
   -- Iir_Kind_Choice_By_Expression (Short)
   -- (Iir_Kinds_Choice)
   --
   --   Get/Set_Parent (Field0)
   --
   -- These are elements of an choice chain, which is used for
   -- case_statement, concurrent_select_signal_assignment, aggregates.
   --
   -- Get/Set what is associated with the choice.  This can be:
   -- * a waveform_chain for a concurrent_select_signal_assignment,
   -- * an expression for an aggregate,
   -- * a sequential statement list for a case_statement.
   -- For a list of choices, only the first one is associated, the following
   -- associations have the same_alternative_flag set.
   --   Get/Set_Associated (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   -- Only for Iir_Kind_Choice_By_Name:
   -- Get/Set the name.
   --   Get/Set_Name (Field4)
   --
   -- Only for Iir_Kind_Choice_By_Expression:
   --   Get/Set_Expression (Field5)
   --
   -- Only for Iir_Kind_Choice_By_Range:
   -- Get/Set the range.
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Same_Alternative_Flag (Flag1)
   --
   -- Only for Iir_Kind_Choice_By_Range:
   -- Only for Iir_Kind_Choice_By_Expression:
   --   Get/Set_Choice_Staticness (State2)

   -- Iir_Kind_Entity_Aspect_Entity (Short)
   --
   -- Parse: a name
   -- Sem: a design unit
   --   Get/Set_Entity (Field4)
   --
   -- parse: a simple name.
   -- sem: an architecture declaration or NULL_IIR.
   --   Get/Set_Architecture (Field2)

   -- Iir_Kind_Entity_Aspect_Open (Short)

   -- Iir_Kind_Entity_Aspect_Configuration (Short)
   --
   -- Parse: a name
   -- Sem: a design unit
   --   Get/Set_Configuration (Field1)

   -- Iir_Kind_Block_Configuration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Configuration_Item_Chain (Field3)
   --
   -- Note: for default block configurations of iterative generate statement,
   -- the block specification is a selected_name, whose identifier is others.
   --   Get/Set_Block_Specification (Field5)
   --
   -- Single linked list of block configuration that apply to the same
   -- for scheme generate block.
   --   Get/Set_Prev_Block_Configuration (Field4)

   -- Iir_Kind_Binding_Indication (Medium)
   --
   --   Get/Set_Default_Entity_Aspect (Field1)
   --
   -- The entity aspect.
   -- It is a iir_kind_entity_aspect_entity, iir_kind_entity_aspect_open or
   -- iir_kind_entity_aspect_configuration.  This may be transformed into a
   -- declaration by semantic.
   --   Get/Set_Entity_Aspect (Field3)
   --
   --   Get/Set_Default_Generic_Map_Aspect_Chain (Field6)
   --
   --   Get/Set_Default_Port_Map_Aspect_Chain (Field7)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Port_Map_Aspect_Chain (Field9)

   -- Iir_Kind_Component_Configuration (Short)
   -- Iir_Kind_Configuration_Specification (Short)
   --
   -- The declaration containing this type declaration.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Component_Name (Field4)
   --
   -- Must be one of designator_list, designator_by_others or
   -- designator_by_all.
   --   Get/Set_Instantiation_List (Field1)
   --
   -- Only for Iir_Kind_Component_Configuration:
   --   Get/Set_Block_Configuration (Field5)
   --
   --   Get/Set_Binding_Indication (Field3)
   --
   --   Get/Set_Chain (Field2)

   -- Iir_Kind_Disconnection_Specification (Short)
   --
   -- The declaration containing this type declaration.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Signal_List (Field4)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Chain (Field2)

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
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Entity_Name_List (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Entity_Class (Field3)
   --
   --   Get/Set_Attribute_Value_Spec_Chain (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Attribute_Designator (Field6)
   --
   --   Get/Set_Attribute_Specification_Chain (Field7)

   -- Iir_Kind_Attribute_Value (Short)
   -- An attribute value is the element of the chain of attribute of an entity,
   -- marking the entity as decorated by the attribute.
   -- This node is built only by sem.
   -- In fact, the node is member of the chain of attribute of an entity, and
   -- of the chain of entity of the attribute specification.
   -- This makes elaboration (and more precisely, expression evaluation)
   -- easier.
   --
   --   Get/Set_Spec_Chain (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
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

   -- Iir_Kind_Selected_Element (Short)
   -- A record element selection.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Selected_Element (Field2)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Implicit_Dereference (Short)
   -- Iir_Kind_Dereference (Short)
   -- An implicit access dereference.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
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

   -- Iir_Kind_Signature (Short)
   --
   --   Get/Set_Return_Type (Field1)
   --
   --   Get/Set_Type_Marks_List (Field2)
   --
   -- Used only for attribute specification.
   --   Get/Set_Name (Field4)

   -- Iir_Kind_Overload_List (Short)
   --
   --   Get/Set_Overload_List (Field1)

   ------------------
   -- Declarations --
   ------------------

   -- Iir_Kind_Entity_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field5)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Port_Chain (Field7)
   --
   --   Get/Set_Is_Within_Flag (Flag5)

   -- Iir_Kind_Architecture_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   -- Set the entity of an architecture.
   -- Before the semantic pass, it can be a name.
   --   Get/Set_Entity (Field4)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field5)
   --
   -- The default configuration created by canon.  This is a design unit.
   --   Get/Set_Default_Configuration_Declaration (Field6)
   --
   --   Get/Set_Foreign_Flag (Flag3)
   --
   --   Get/Set_Is_Within_Flag (Flag5)

   -- Iir_Kind_Configuration_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   -- Set the entity of a configuration (a design_unit)
   -- Before the semantic pass, it can be an identifier.
   --   Get/Set_Entity (Field4)
   --
   --   Get/Set_Block_Configuration (Field5)

   -- Iir_Kind_Package_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Package_Body (Field4)
   --
   --   Get/Set_Need_Body (Flag1)

   -- Iir_Kind_Package_Body (Short)
   -- Note: a body is not a declaration, that's the reason why there is no
   -- _declaration suffix in the name.
   --
   --   Get/Set_Parent (Field0)
   --   Get/Set_Design_Unit (Alias Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Package (Field4)

   -- Iir_Kind_Library_Declaration (Medium)
   --
   -- Design files in the library.
   --   Get/Set_Design_File_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   -- This node is used to contain all a library.  Only internaly used.
   -- Name (identifier) of the library.
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Date (Field10)
   --
   --   Get/Set_Library_Directory (Field11)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Component_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Generic_Chain (Field6)
   --
   --   Get/Set_Port_Chain (Field7)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Object_Alias_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Name (Field4)
   --
   -- Note: base name is the alias itself.
   --   Get/Set_Base_Name (Field5)
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
   --   Get/Set_Signature (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Anonymous_Type_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   -- Used for informative purpose only.
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Subtype_Definition (Field4)

   -- Iir_Kind_Type_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   -- Definition of the type.
   -- Note: the type definition can be a real type (unconstrained array,
   -- enumeration, file, record, access) or a subtype (integer, floating
   -- point).
   -- The parser set this field to null_iir for an incomplete type declaration.
   -- This field is set to an incomplete_type_definition node when first
   -- semantized.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Subtype_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Signal_Interface_Declaration (Medium)
   -- Iir_Kind_Constant_Interface_Declaration (Medium)
   -- Iir_Kind_Variable_Interface_Declaration (Medium)
   -- Iir_Kind_File_Interface_Declaration (Medium)
   --
   -- Note: If type is an iir_kind_proxy node, then type *and* default value
   -- (if any) must be extracted from proxy.
   --
   -- Get/Set the parent of an interface declaration.
   -- The parent is an entity declaration, a subprogram specification, a
   -- component declaration, a loop statement, a block declaration or ??
   -- Useful to distinguish a port and an interface.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   -- Must always be null_iir for iir_kind_file_interface_declaration.
   --   Get/Set_Default_Value (Field6)
   --
   -- Only for Iir_Kind_Signal_Interface_Declaration:
   --   Get/Set_Extra_Info (Field8)
   --
   --   Get/Set_Mode (Odigit1)
   --
   --   Get/Set_Lexical_Layout (Odigit2)
   --
   -- Only for Iir_Kind_Signal_Interface_Declaration:
   --   Get/Set_Has_Disconnect_Flag (Flag1)
   --
   -- Only for Iir_Kind_Signal_Interface_Declaration:
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   -- Only for Iir_Kind_Signal_Interface_Declaration:
   --   Get/Set_Open_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_After_Drivers_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   -- Only for Iir_Kind_Signal_Interface_Declaration:
   --   Get/Set_Signal_Kind (State3)

   -- Iir_Kind_Function_Declaration (Medium)
   -- Iir_Kind_Procedure_Declaration (Medium)
   --
   -- Subprogram declaration.
   --
   -- The declaration containing this subrogram declaration.
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
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get_Interface_Declaration_Chain (Field5)
   --
   --   Get/Set_Subprogram_Body (Field6)
   --
   --   Get/Set_Callees_List (Field7)
   --
   --   Get/Set_Extra_Info (Field8)
   --
   --   Get/Set_Overload_Number (Field9)
   --
   --   Get/Set_Subprogram_Depth (Field10)
   --
   --   Get/Set_Subprogram_Hash (Field11)
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
   --   Get/Set_Resolution_Function_Flag (Flag7)
   --
   --   Get/Set_Wait_State (State1)
   --
   -- Only for Iir_Kind_Procedure_Declaration:
   --   Get/Set_Purity_State (State2)
   --
   --   Get/Set_All_Sensitized_State (State3)

   -- Iir_Kind_Function_Body (Short)
   -- Iir_Kind_Procedure_Body (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   -- The parse stage always puts a declaration before a body.
   -- Sem will remove the declaration if there is a forward declaration.
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Impure_Depth (Field3)
   --
   --   Get/Set_Subprogram_Specification (Field4)
   --
   --   Get/Set_Sequential_Statement_Chain (Field5)

   -- Iir_Kind_Implicit_Procedure_Declaration (Medium)
   -- Iir_Kind_Implicit_Function_Declaration (Medium)
   --
   -- This node contains a subprogram_declaration that was implicitly defined
   -- just after a type declaration.
   -- This declaration is inserted by sem.
   --
   --   Get/Set_Parent (Field0)
   --
   -- Only for Iir_Kind_Implicit_Function_Declaration:
   --   Get/Set_Return_Type (Field1)
   --
   -- Only for Iir_Kind_Implicit_Function_Declaration:
   --   Get/Set_Type (Alias Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get_Interface_Declaration_Chain (Field5)
   --
   --   Get/Set_Implicit_Definition (Field6)
   --
   --   Get/Set_Callees_List (Field7)
   --
   --   Get/Set_Extra_Info (Field8)
   --
   --   Get/Set_Overload_Number (Field9)
   --
   --   Get/Set_Type_Reference (Field10)
   --
   --   Get/Set_Subprogram_Hash (Field11)
   --
   --   Get/Set_Wait_State (State1)
   --
   --   Get/Set_Seen_Flag (Flag1)
   --
   -- Only for Iir_Kind_Implicit_Function_Declaration:
   --   Get/Set_Pure_Flag (Flag2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Signal_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Default_Value (Field6)
   --
   -- For a non-resolved signal: null_iir if the signal has no driver, or
   -- a process/concurrent_statement for which the signal should have a
   -- driver.  This is used to catch at analyse time unresolved signals with
   -- several drivers.
   --   Get/Set_Signal_Driver (Field7)
   --
   --   Get/Set_Extra_Info (Field8)
   --
   --   Get/Set_Has_Disconnect_Flag (Flag1)
   --
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_After_Drivers_Flag (Flag5)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Signal_Kind (State3)

   -- Iir_Kind_Guard_Signal_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Guard_Expression (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Guard_Sensitivity_List (Field6)
   --
   --   Get/Set_Block_Statement (Field7)
   --
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Signal_Kind (State3)

   -- Iir_Kind_Constant_Declaration (Medium)
   -- Iir_Kind_Iterator_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   -- Only for Iir_Kind_Constant_Declaration:
   -- Default value of a deferred constant points to the full constant
   -- declaration.
   --   Get/Set_Default_Value (Field6)
   --
   -- Only for Iir_Kind_Constant_Declaration:
   -- Summary:
   -- | constant C1 : integer;          -- Deferred declaration (in a package)
   -- |  constant C2 : integer := 4;     -- Declaration
   -- |  constant C1 : integer := 3;     -- Full declaration (in a body)
   -- | NAME   Deferred_declaration  Deferred_declaration_flag
   -- |  C1      Null_iir or C1' (*)     True
   -- |  C2      Null_Iir                False
   -- |  C1'     C1                      False
   -- |(*): Deferred_declaration is Null_Iir as long as the full declaration
   -- |   has not been analyzed.
   --   Get/Set_Deferred_Declaration (Field7)
   --
   -- Only for Iir_Kind_Constant_Declaration:
   --   Get/Set_Deferred_Declaration_Flag (Flag1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Variable_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Default_Value (Field6)
   --
   -- True if the variable is a shared variable.
   --   Get/Set_Shared_Flag (Flag2)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_File_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_File_Logical_Name (Field6)
   --
   -- This is not used in vhdl 87.
   --   Get/Set_File_Open_Kind (Field7)
   --
   -- This is used only in vhdl 87.
   --   Get/Set_Mode (Odigit1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Element_Declaration (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Identifier (Field3)
   --
   -- Return the position of the element in the record, starting from 0 for the
   -- first record element, increasing by one for each successive element.
   --   Get/Set_Element_Position (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Record_Element_Constraint (Short)
   --
   -- Record subtype definition which defines this constraint.
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Element_Declaration (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   -- Return the position of the element in the record, starting from 0 for the
   -- first record element, increasing by one for each successive element.
   --   Get/Set_Element_Position (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Attribute_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Group_Template_Declaration (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   -- List of entity class entry.
   -- To handle `<>', the last element of the list can be an entity_class of
   -- kind tok_box.
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
   -- The declaration containing this type declaration.
   --   Get/Set_Parent (Field0)
   --
   -- List of constituent.
   --   Get/Set_Group_Constituent_List (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Group_Template_Name (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Psl_Declaration (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Psl_Declaration (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   -- Valid only for property declaration.
   --   Get/Set_PSL_Clock (Field7)
   --
   -- Valid only for property declaration without parameters.
   --   Get/Set_PSL_NFA (Field8)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Use_Flag (Flag6)

   -- Iir_Kind_Use_Clause (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Selected_Name (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Use_Clause_Chain (Field3)


   ----------------------
   -- type definitions --
   ----------------------

   -- For Iir_Kinds_Type_And_Subtype_Definition:
   --
   -- Type_Declarator:
   -- Points to the type declaration or subtype declaration that has created
   -- this definition. For some types, such as integer and floating point
   -- types, both type and subtype points to the declaration.
   -- However, there are cases where a type definition doesn't point to
   -- a declarator: anonymous subtype created by index contraints, or
   -- anonymous subtype created by an object declaration.
   -- Note: a type definition cannot be anoynymous.
   --   Get/Set_Type_Declarator (Field3)
   --
   -- Get/Set the base type.
   -- For a subtype, it returns the type.
   -- For a type, it must return the type itself.
   --   Get/Set_Base_Type (Field4)
   --
   -- Get/Set the staticness of a type, according to LRM93 7.4.1.
   -- Note: These types definition are always locally static:
   -- enumeration, integer, floating.
   -- However, their subtype are not necessary locally static.
   --   Get/Set_Type_Staticness (State1)
   --
   -- Get/Set the resolved flag of a subtype, according to LRM93 2.4
   --   Get/Set_Resolved_Flag (Flag1)
   --
   -- Get/Set the signal_type flag of a type definition.
   -- It is true when the type can be used for a signal.
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)

   -- Iir_Kind_Enumeration_Type_Definition (Short)
   --
   -- Get the range of the type (This is just an ascending range from the
   -- first literal to the last declared literal).
   --   Get/Set_Range_Constraint (Field1)
   --
   -- Return the list of literals.  This list is created when the node is
   -- created.
   --   Get/Set_Enumeration_Literal_List (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Only_Characters_Flag (Flag4)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Enumeration_Literal (Medium)
   --
   -- Nota: two literals of the same type are equal iff their value is the
   -- same; in other words, there may be severals literals with the same value.
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Type (Field1)
   --   Get/Set_Return_Type (Alias Field1)
   --
   --   Get/Set_Literal_Origin (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   -- The declaration of the literal.  If LITERAL_ORIGIN is not set, then this
   -- is the node itself, else this is the literal defined.
   --   Get/Set_Enumeration_Decl (Field6)
   --
   -- The value of an enumeration literal is the position.
   --   Get/Set_Enum_Pos (Field10)
   --
   --   Get/Set_Subprogram_Hash (Field11)
   --
   --   Get/Set_Seen_Flag (Flag1)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Physical_Type_Definition (Short)
   --
   --   Get/Set_Unit_Chain (Field1)
   --   Get_Primary_Unit (Alias Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Unit_Declaration (Medium)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Physical_Literal (Field6)
   --
   --   Get/Set_Physical_Unit_Value (Field7)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Integer_Type_Definition (Short)
   -- Iir_Kind_Floating_Type_Definition (Short)
   --
   -- Get/Set the declarator that has created this integer type.
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   -- Type staticness is always locally.
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)

   -- Iir_Kind_Array_Type_Definition (Medium)
   -- This defines an unconstrained array type.
   --
   --   Get/Set_Element_Subtype (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Index_Subtype_List (Field6)
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
   --   Get/Set_Index_Constraint_Flag (Flag4)

   -- Iir_Kind_Record_Type_Definition (Short)
   --
   --   Get/Set_Elements_Declaration_List (Field1)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
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

   -- Iir_Kind_Access_Type_Definition (Short)
   --
   --   Get/Set_Designated_Type (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   -- FIXME: Only for access_subtype.
   -- FIXME:  Get/Set_Resolution_Function (Field5)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_File_Type_Definition (Short)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   -- True if this is the std.textio.text file type, which may require special
   -- handling.
   --   Get/Set_Text_File_Flag (Flag4)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Incomplete_Type_Definition (Short)
   -- Type definition for an incomplete type.  This is created during the
   -- semantisation of the incomplete type declaration.
   --
   --   Get/Set_Incomplete_Type_List (Field2)
   --
   -- Set to the incomplete type declaration when semantized, and set to the
   -- complete type declaration when the latter one is semantized.
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
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
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)

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

   -------------------------
   -- subtype definitions --
   -------------------------

   -- Iir_Kind_Enumeration_Subtype_Definition (Short)
   -- Iir_Kind_Integer_Subtype_Definition (Short)
   -- Iir_Kind_Floating_Subtype_Definition (Short)
   -- Iir_Kind_Physical_Subtype_Definition (Short)
   --
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Resolution_Function (Field5)
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)
   --
   --   Get/Set_Has_Signal_Flag (Flag3)
   --
   --   Get/Set_Type_Staticness (State1)

   -- Iir_Kind_Access_Subtype_Definition (Short)
   --
   --   Get/Set_Type_Staticness (State1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   -- Note: no resolution function for access subtype.
   --
   --   Get/Set_Resolved_Flag (Flag1)
   --
   --   Get/Set_Signal_Type_Flag (Flag2)

   -- Iir_Kind_Record_Subtype_Definition (Short)
   --
   --   Get/Set_Elements_Declaration_List (Field1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Resolution_Function (Field5)
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

   -- Iir_Kind_Array_Subtype_Definition (Medium)
   --
   --   Get/Set_Element_Subtype (Field1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
   --
   --   Get/Set_Resolution_Function (Field5)
   --
   --   Get/Set_Index_Subtype_List (Field6)
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
   --   Get/Set_Index_Constraint_Flag (Flag4)

   -- Iir_Kind_Range_Expression (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Left_Limit (Field2)
   --
   --   Get/Set_Right_Limit (Field3)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Direction (State2)

   -- Iir_Kind_Subtype_Definition (Short)
   -- Such a node is only created by parse and transformed into the correct
   -- kind (enumeration_subtype, integer_subtype...) by sem.
   --
   --   Get/Set_Range_Constraint (Field1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Resolution_Function (Field5)

   ---------------------------
   -- concurrent statements --
   ---------------------------

   -- Iir_Kind_Concurrent_Conditional_Signal_Assignment (Medium)
   -- Iir_Kind_Concurrent_Selected_Signal_Assignment (Medium)
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   -- Only for Iir_Kind_Concurrent_Selected_Signal_Assignment:
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Reject_Time_Expression (Field6)
   --
   -- Only for Iir_Kind_Concurrent_Conditional_Signal_Assignment:
   --   Get/Set_Conditional_Waveform_Chain (Field7)
   --
   -- Only for Iir_Kind_Concurrent_Selected_Signal_Assignment:
   --   Get/Set_Selected_Waveform_Chain (Field7)
   --
   -- If the assignment is guarded, then get_guard must return the
   -- declaration of the signal guard, otherwise, null_iir.
   -- If the guard signal decl is not known, as a kludge and only to mark this
   -- assignment guarded, the guard can be this assignment.
   --   Get/Set_Guard (Field8)
   --
   --   Get/Set_Delay_Mechanism (Field12)
   --
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   -- True if the target of the assignment is guarded
   --   Get_Guarded_Target_State (State3)

   -- Iir_Kind_Sensitized_Process_Statement (Medium)
   -- Iir_Kind_Process_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Sequential_Statement_Chain (Field5)
   --
   -- Only for Iir_Kind_Sensitized_Process_Statement:
   --   Get_Sensitivity_List (Field6)
   --
   --   Get/Set_Callees_List (Field7)
   --
   --   Get/Set_Extra_Info (Field8)
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

   -- Iir_Kind_Concurrent_Assertion_Statement (Medium)
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Severity_Expression (Field5)
   --
   --   Get/Set_Report_Expression (Field6)
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
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)

   -- Iir_Kind_Psl_Assert_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Psl_Property (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Severity_Expression (Field5)
   --
   --   Get/Set_Report_Expression (Field6)
   --
   --   Get/Set_PSL_Clock (Field7)
   --
   --   Get/Set_PSL_NFA (Field8)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Component_Instantiation_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   -- Unit instantiated.
   -- Parse: a name, a entity_aspect_entity or a entity_aspect_configuration
   -- Sem: the component declaration or the design unit.
   --   Get/Set_Instantiated_Unit (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Default_Binding_Indication (Field5)
   --
   --   Get/Set_Generic_Map_Aspect_Chain (Field8)
   --
   --   Get/Set_Port_Map_Aspect_Chain (Field9)
   --
   -- Configuration:
   -- In case of a configuration specification, the node is put into
   -- default configuration.  In the absence of a specification, the
   -- default entity aspect, if any; if none, this field is null_iir.
   --   Get/Set_Configuration_Specification (Field7)
   --
   -- During Sem and elaboration, the configuration field can be filled by
   -- a component configuration declaration.
   --
   -- Configuration for this component.
   -- FIXME: must be get/set_binding_indication.
   --   Get/Set_Component_Configuration (Field6)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Block_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field5)
   --
   --   Get/Set_Block_Block_Configuration (Field6)
   --
   --   Get/Set_Block_Header (Field7)
   --
   -- get/set_guard_decl is used for semantic analysis, in order to add
   -- a signal declaration.
   --   Get/Set_Guard_Decl (Field8)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)

   -- Iir_Kind_Generate_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get_Declaration_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Concurrent_Statement_Chain (Field5)
   --
   -- The generation scheme.
   -- A (boolean) expression for a conditionnal elaboration (if).
   -- A (iterator) declaration for an iterative elaboration (for).
   --   Get/Set_Generation_Scheme (Field6)
   --
   -- The block configuration for this statement.
   --   Get/Set_Generate_Block_Configuration (Field7)
   --
   --   Get/Set_Visible_Flag (Flag4)

   ---------------------------
   -- sequential statements --
   ---------------------------

   -- Iir_Kind_If_Statement (Medium)
   -- Iir_Kind_Elsif (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   -- May be NULL only for an iir_kind_elsif node, and then means the else
   -- clause.
   --   Get/Set_Condition (Field1)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Chain (Field2)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Label (Field3)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Identifier (Alias Field3)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Sequential_Statement_Chain (Field5)
   --
   -- Must be an Iir_kind_elsif node, or NULL for no more elsif clauses.
   --   Get/Set_Else_Clause (Field6)
   --
   -- Only for Iir_Kind_If_Statement:
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_For_Loop_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Iterator_Scheme (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Sequential_Statement_Chain (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   --   Get/Set_Is_Within_Flag (Flag5)

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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Sequential_Statement_Chain (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Exit_Statement (Short)
   -- Iir_Kind_Next_Statement (Short)
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   -- Label identifier after parse.
   --   Get/Set_Loop (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Signal_Assignment_Statement (Medium)
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   -- The waveform.
   -- If the waveform_chain is null_iir, then the signal assignment is a
   -- disconnection statement, ie TARGET <= null_iir after disconection_time,
   -- where disconnection_time is specified by a disconnection specification.
   --   Get/Set_Waveform_Chain (Field5)
   --
   --   Get/Set_Reject_Time_Expression (Field6)
   --
   --   Get/Set_Delay_Mechanism (Field12)
   --
   --   Get/Set_Visible_Flag (Flag4)
   --
   -- True if the target of the assignment is guarded
   --   Get_Guarded_Target_State (State3)

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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Assertion_Statement (Medium)
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Severity_Expression (Field5)
   --
   --   Get/Set_Report_Expression (Field6)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Report_Statement (Medium)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Severity_Expression (Field5)
   --
   --   Get/Set_Report_Expression (Field6)
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Condition_Clause (Field5)
   --
   --   Get/Set_Sensitivity_List (Field6)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Return_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   -- Type of the return value of the function.  This is a copy of return_type.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Case_Statement (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   -- Chain is compose of Iir_Kind_Choice_By_XXX.
   --   Get/Set_Case_Statement_Alternative_Chain (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Label (Field3)
   --   Get/Set_Identifier (Alias Field3)
   --
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Visible_Flag (Flag4)

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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   -- Only for Iir_Kind_Concurrent_Procedure_Call_Statement:
   --   Get/Set_Postponed_Flag (Flag3)
   --
   --   Get/Set_Visible_Flag (Flag4)

   -- Iir_Kind_Procedure_Call (Short)
   --
   --   Get/Set_Parent (Field0)
   --
   --   Get/Set_Parameter_Association_Chain (Field2)
   --
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
   --   Get/Set_Attribute_Value_Chain (Field4)
   --
   --   Get/Set_Visible_Flag (Flag4)

   ---------------
   -- operators --
   ---------------

   -- Iir_Kinds_Monadic_Operator (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Operand (Field2)
   --
   -- Function declaration corresponding to the function to call.
   --   Get/Set_Implementation (Field3)
   --
   -- Expr_staticness is defined by §7.4
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kinds_Dyadic_Operator (Short)
   --
   --   Get/Set_Type (Field1)
   --
   -- Left and Right operands.
   --   Get/Set_Left (Field2)
   --
   -- Function declaration corresponding to the function to call.
   --   Get/Set_Implementation (Field3)
   --
   --   Get/Set_Right (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Function_Call (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Parameter_Association_Chain (Field2)
   --
   -- Function declaration corresponding to the function to call.
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
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Aggregate_Info (Field2)
   --
   --   Get/Set_Association_Choices_Chain (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Value_Staticness (State2)

   -- Iir_Kind_Aggregate_Info (Short)
   --
   -- Get info for the next dimension.  NULL_IIR terminated.
   --   Get/Set_Sub_Aggregate_Info (Field1)
   --
   -- For array aggregate only:
   -- If TRUE, the aggregate bounds are not locally static.
   -- This flag is only valid when the array aggregate is constrained, ie
   -- has no 'others' choice.
   --   Get/Set_Aggr_Dynamic_Flag (Flag3)
   --
   -- If TRUE, the aggregate is named, else it is positionnal.
   --   Get/Set_Aggr_Named_Flag (Flag4)
   --
   -- The following three fields are used to check bounds of an array
   -- aggregate.
   -- For named aggregate, low and high bounds are computed, for positionnal
   -- aggregate, the (minimum) number of elements is computed.
   -- Note there may be elements beyond the bounds, due to other choice.
   -- These fields may apply for the aggregate or for the aggregate and its
   -- brothers if the node is for a sub-aggregate.
   --
   -- The low and high index choice, if any.
   --   Get/Set_Aggr_Low_Limit (Field2)
   --
   --   Get/Set_Aggr_High_Limit (Field3)
   --
   -- The maximum number of elements, if any.
   --   Get/Set_Aggr_Max_Length (Field4)
   --
   -- True if the choice list has an 'others' choice.
   --   Get/Set_Aggr_Others_Flag (Flag2)

   -- Iir_Kind_Qualified_Expression (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Type_Conversion (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Type_Mark (Field2)
   --
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Allocator_By_Expression (Short)
   -- Iir_Kind_Allocator_By_Subtype (Short)
   --
   --   Get/Set_Type (Field1)
   --
   -- Contains the expression for a by expression allocator or the
   -- subtype indication for a by subtype allocator.
   --   Get/Set_Expression (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -----------
   -- names --
   -----------

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
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Selected_Name (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Suffix_Identifier (Field2)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Selected_By_All_Name (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Operator_Symbol (Short)
   --
   --   Get/Set_Identifier (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Base_Name (Field5)

   -- Iir_Kind_Indexed_Name (Short)
   -- Select the element designed with the INDEX_LIST from array PREFIX.
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Index_List (Field2)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Slice_Name (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Suffix (Field2)
   --
   --   Get/Set_Base_Name (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)

   -- Iir_Kind_Parenthesis_Name (Short)
   -- Created by the parser, and mutated into the correct iir node: it can be
   -- either a function call, an indexed array, a type conversion or a slice
   -- name.
   --
   -- Always returns null_iir.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Association_Chain (Field2)

   ----------------
   -- attributes --
   ----------------

   -- Iir_Kind_Attribute_Name (Short)
   --
   --   Get/Set_Attribute_Identifier (Field2)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Named_Entity (Field4)
   --
   --   Get/Set_Signature (Field5)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Base_Attribute (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)

   -- Iir_Kind_Left_Type_Attribute (Short)
   -- Iir_Kind_Right_Type_Attribute (Short)
   -- Iir_Kind_High_Type_Attribute (Short)
   -- Iir_Kind_Low_Type_Attribute (Short)
   -- Iir_Kind_Ascending_Type_Attribute (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Range_Array_Attribute (Short)
   -- Iir_Kind_Reverse_Range_Array_Attribute (Short)
   -- Iir_Kind_Left_Array_Attribute (Short)
   -- Iir_Kind_Right_Array_Attribute (Short)
   -- Iir_Kind_High_Array_Attribute (Short)
   -- Iir_Kind_Low_Array_Attribute (Short)
   -- Iir_Kind_Ascending_Array_Attribute (Short)
   -- Iir_Kind_Length_Array_Attribute (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Index_Subtype (Field2)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Stable_Attribute (Short)
   -- Iir_Kind_Delayed_Attribute (Short)
   -- Iir_Kind_Quiet_Attribute (Short)
   -- Iir_Kind_Transaction_Attribute (Short)
   -- (Iir_Kinds_Signal_Attribute)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Prefix (Field3)
   --
   -- Not used by Iir_Kind_Transaction_Attribute
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Has_Active_Flag (Flag2)
   --
   --   Get/Set_Expr_Staticness (State1)
   --
   --   Get/Set_Name_Staticness (State2)
   --
   --   Get/Set_Base_Name (Field5)

   -- Iir_Kind_Event_Attribute (Short)
   -- Iir_Kind_Last_Event_Attribute (Short)
   -- Iir_Kind_Last_Value_Attribute (Short)
   -- Iir_Kind_Active_Attribute (Short)
   -- Iir_Kind_Last_Active_Attribute (Short)
   -- Iir_Kind_Driving_Attribute (Short)
   -- Iir_Kind_Driving_Value_Attribute (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
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
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Image_Attribute (Short)
   -- Iir_Kind_Value_Attribute (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   --   Get/Set_Parameter (Field4)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Simple_Name_Attribute (Short)
   -- Iir_Kind_Instance_Name_Attribute (Short)
   -- Iir_Kind_Path_Name_Attribute (Short)
   --
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Prefix (Field3)
   --
   -- Only for Iir_Kind_Simple_Name_Attribute:
   --   Get/Set_Simple_Name_Identifier (Field2)
   --
   --   Get/Set_Expr_Staticness (State1)

   -- Iir_Kind_Behavior_Attribute (Short)
   -- Iir_Kind_Structure_Attribute (Short)
   -- FIXME: to describe (Short)

   -- Iir_Kind_Error (Short)
   -- Can be used instead of an expression or a type.
   --   Get/Set_Type (Field1)
   --
   --   Get/Set_Error_Origin (Field2)
   --
   --   Get/Set_Type_Declarator (Field3)
   --
   --   Get/Set_Base_Type (Field4)
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


   -- End of Iir_Kind.


   type Iir_Kind is
      (
   -- Erroneous IIR.
       Iir_Kind_Error,

       Iir_Kind_Design_File,
       Iir_Kind_Design_Unit,
       Iir_Kind_Library_Clause,
       Iir_Kind_Use_Clause,

   -- Literals.
       Iir_Kind_Character_Literal,
       Iir_Kind_Integer_Literal,
       Iir_Kind_Floating_Point_Literal,
       Iir_Kind_Null_Literal,
       Iir_Kind_String_Literal,
       Iir_Kind_Physical_Int_Literal,
       Iir_Kind_Physical_Fp_Literal,
       Iir_Kind_Bit_String_Literal,
       Iir_Kind_Simple_Aggregate,

   -- Tuple,
       Iir_Kind_Proxy,
       Iir_Kind_Waveform_Element,
       Iir_Kind_Conditional_Waveform,
       Iir_Kind_Association_Element_By_Expression,
       Iir_Kind_Association_Element_By_Individual,
       Iir_Kind_Association_Element_Open,
       Iir_Kind_Choice_By_Others,
       Iir_Kind_Choice_By_Expression,
       Iir_Kind_Choice_By_Range,
       Iir_Kind_Choice_By_None,
       Iir_Kind_Choice_By_Name,
       Iir_Kind_Entity_Aspect_Entity,
       Iir_Kind_Entity_Aspect_Configuration,
       Iir_Kind_Entity_Aspect_Open,
       Iir_Kind_Block_Configuration,
       Iir_Kind_Block_Header,
       Iir_Kind_Component_Configuration,
       Iir_Kind_Binding_Indication,
       Iir_Kind_Entity_Class,
       Iir_Kind_Attribute_Value,
       Iir_Kind_Signature,
       Iir_Kind_Aggregate_Info,
       Iir_Kind_Procedure_Call,
       Iir_Kind_Operator_Symbol,
       Iir_Kind_Record_Element_Constraint,

       Iir_Kind_Attribute_Specification,
       Iir_Kind_Disconnection_Specification,
       Iir_Kind_Configuration_Specification,

   -- Type definitions.
   -- iir_kinds_type_and_subtype_definition
   -- kinds: disc: discrete, st: subtype.
       Iir_Kind_Access_Type_Definition,
       Iir_Kind_Incomplete_Type_Definition,
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
       Iir_Kind_Subtype_Definition,  -- temporary (must not appear after sem).

   -- Lists.
       Iir_Kind_Overload_List,  -- used internally by sem_expr.

   -- Declarations.
       Iir_Kind_Type_Declaration,
       Iir_Kind_Anonymous_Type_Declaration,
       Iir_Kind_Subtype_Declaration,
       Iir_Kind_Configuration_Declaration,
       Iir_Kind_Entity_Declaration,
       Iir_Kind_Package_Declaration,
       Iir_Kind_Package_Body,
       Iir_Kind_Architecture_Declaration,
       Iir_Kind_Unit_Declaration,
       Iir_Kind_Library_Declaration,
       Iir_Kind_Component_Declaration,
       Iir_Kind_Attribute_Declaration,
       Iir_Kind_Group_Template_Declaration,
       Iir_Kind_Group_Declaration,
       Iir_Kind_Element_Declaration,
       Iir_Kind_Non_Object_Alias_Declaration,

       Iir_Kind_Psl_Declaration,

       Iir_Kind_Function_Body,
       Iir_Kind_Function_Declaration,
       Iir_Kind_Implicit_Function_Declaration,
       Iir_Kind_Implicit_Procedure_Declaration,
       Iir_Kind_Procedure_Declaration,
       Iir_Kind_Procedure_Body,
       Iir_Kind_Enumeration_Literal,

       Iir_Kind_Object_Alias_Declaration,       -- object
       Iir_Kind_File_Declaration,               -- object
       Iir_Kind_Guard_Signal_Declaration,       -- object
       Iir_Kind_Signal_Declaration,             -- object
       Iir_Kind_Variable_Declaration,           -- object
       Iir_Kind_Constant_Declaration,           -- object
       Iir_Kind_Iterator_Declaration,           -- object
       Iir_Kind_Constant_Interface_Declaration, -- object, interface
       Iir_Kind_Variable_Interface_Declaration, -- object, interface
       Iir_Kind_Signal_Interface_Declaration,   -- object, interface
       Iir_Kind_File_Interface_Declaration,     -- object, interface

   -- Expressions.
       Iir_Kind_Identity_Operator,
       Iir_Kind_Negation_Operator,
       Iir_Kind_Absolute_Operator,
       Iir_Kind_Not_Operator,
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
       Iir_Kind_Qualified_Expression,
       Iir_Kind_Type_Conversion,
       Iir_Kind_Allocator_By_Expression,
       Iir_Kind_Allocator_By_Subtype,
       Iir_Kind_Selected_Element,
       Iir_Kind_Dereference,
       Iir_Kind_Implicit_Dereference,
       Iir_Kind_Psl_Expression,

   -- Concurrent statements.
       Iir_Kind_Sensitized_Process_Statement,
       Iir_Kind_Process_Statement,
       Iir_Kind_Concurrent_Conditional_Signal_Assignment,
       Iir_Kind_Concurrent_Selected_Signal_Assignment,
       Iir_Kind_Concurrent_Assertion_Statement,
       Iir_Kind_Psl_Default_Clock,
       Iir_Kind_Psl_Assert_Statement,
       Iir_Kind_Concurrent_Procedure_Call_Statement,
       Iir_Kind_Block_Statement,
       Iir_Kind_Generate_Statement,
       Iir_Kind_Component_Instantiation_Statement,

   -- Iir_Kind_Sequential_Statement
       Iir_Kind_Signal_Assignment_Statement,
       Iir_Kind_Null_Statement,
       Iir_Kind_Assertion_Statement,
       Iir_Kind_Report_Statement,
       Iir_Kind_Wait_Statement,
       Iir_Kind_Variable_Assignment_Statement,
       Iir_Kind_Return_Statement,
       Iir_Kind_For_Loop_Statement,
       Iir_Kind_While_Loop_Statement,
       Iir_Kind_Next_Statement,
       Iir_Kind_Exit_Statement,
       Iir_Kind_Case_Statement,
       Iir_Kind_Procedure_Call_Statement,
       Iir_Kind_If_Statement,
       Iir_Kind_Elsif,

   -- Names
       Iir_Kind_Simple_Name,
       Iir_Kind_Slice_Name,
       Iir_Kind_Indexed_Name,
       Iir_Kind_Selected_Name,
       Iir_Kind_Selected_By_All_Name,
       Iir_Kind_Parenthesis_Name,

   -- Attributes
       Iir_Kind_Base_Attribute,
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

   type Iir_Signal_Kind is
      (
       Iir_No_Signal_Kind,
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

   type Iir_Delay_Mechanism is (Iir_Inertial_Delay, Iir_Transport_Delay);

   type Iir_Direction is (Iir_To, Iir_Downto);

   --  Iir_Lexical_Layout_type describe the lexical token used to describe
   --  an interface declaration.  This has no semantics meaning, but it is
   --  necessary to keep how lexically an interface was declared due to
   --  LRM93 2.7 (conformance rules).
   --  To keep this simple, the layout is stored as a bit-string.
   --  Fields are:
   --  Has_type: set if the interface is the last of a list.
   --  has_mode: set if mode is explicit
   --  has_class: set if class (constant, signal, variable or file) is explicit
   --
   --  Exemple:
   --  procedure P (A,B: integer;
   --               C: in constant bit;
   --               D: inout bit;
   --               E: variable bit;
   --               F, G: in bit;
   --               H, I: constant bit;
   --               J, K: in constant bit);
   --  A:
   --  B:                         has_type
   --  C, K: has_mode, has_class, has_type
   --  D:    has_mode,            has_type
   --  E, I:           has_class, has_type
   --  F:    has_mode
   --  G:    has_mode,            has_type
   --  H:              has_class
   --  J:    has_mode, has_class
   type Iir_Lexical_Layout_Type is mod 2 ** 3;
   Iir_Lexical_Has_Mode  : constant Iir_Lexical_Layout_Type := 2 ** 0;
   Iir_Lexical_Has_Class : constant Iir_Lexical_Layout_Type := 2 ** 1;
   Iir_Lexical_Has_Type  : constant Iir_Lexical_Layout_Type := 2 ** 2;

   --  List of predefined operators and functions.
   type Iir_Predefined_Functions is
      (
       Iir_Predefined_Error,

   --  Predefined operators for BOOLEAN type.
       Iir_Predefined_Boolean_And,
       Iir_Predefined_Boolean_Or,
       Iir_Predefined_Boolean_Nand,
       Iir_Predefined_Boolean_Nor,
       Iir_Predefined_Boolean_Xor,
       Iir_Predefined_Boolean_Xnor,
       Iir_Predefined_Boolean_Not,

   --  Predefined operators for any enumeration type.
       Iir_Predefined_Enum_Equality,
       Iir_Predefined_Enum_Inequality,
       Iir_Predefined_Enum_Less,
       Iir_Predefined_Enum_Less_Equal,
       Iir_Predefined_Enum_Greater,
       Iir_Predefined_Enum_Greater_Equal,

   --  Predefined operators for BIT type.
       Iir_Predefined_Bit_And,
       Iir_Predefined_Bit_Or,
       Iir_Predefined_Bit_Nand,
       Iir_Predefined_Bit_Nor,
       Iir_Predefined_Bit_Xor,
       Iir_Predefined_Bit_Xnor,
       Iir_Predefined_Bit_Not,

   --  Predefined operators for any integer type.
       Iir_Predefined_Integer_Equality,
       Iir_Predefined_Integer_Inequality,
       Iir_Predefined_Integer_Less,
       Iir_Predefined_Integer_Less_Equal,
       Iir_Predefined_Integer_Greater,
       Iir_Predefined_Integer_Greater_Equal,

       Iir_Predefined_Integer_Identity,
       Iir_Predefined_Integer_Negation,
       Iir_Predefined_Integer_Absolute,

       Iir_Predefined_Integer_Plus,
       Iir_Predefined_Integer_Minus,
       Iir_Predefined_Integer_Mul,
       Iir_Predefined_Integer_Div,
       Iir_Predefined_Integer_Mod,
       Iir_Predefined_Integer_Rem,

       Iir_Predefined_Integer_Exp,

   --  Predefined operators for any floating type.
       Iir_Predefined_Floating_Equality,
       Iir_Predefined_Floating_Inequality,
       Iir_Predefined_Floating_Less,
       Iir_Predefined_Floating_Less_Equal,
       Iir_Predefined_Floating_Greater,
       Iir_Predefined_Floating_Greater_Equal,

       Iir_Predefined_Floating_Identity,
       Iir_Predefined_Floating_Negation,
       Iir_Predefined_Floating_Absolute,

       Iir_Predefined_Floating_Plus,
       Iir_Predefined_Floating_Minus,
       Iir_Predefined_Floating_Mul,
       Iir_Predefined_Floating_Div,

       Iir_Predefined_Floating_Exp,

   --  Predefined operator for universal types.
       Iir_Predefined_Universal_R_I_Mul,
       Iir_Predefined_Universal_I_R_Mul,
       Iir_Predefined_Universal_R_I_Div,

   --  Predefined operators for physical types.
       Iir_Predefined_Physical_Equality,
       Iir_Predefined_Physical_Inequality,
       Iir_Predefined_Physical_Less,
       Iir_Predefined_Physical_Less_Equal,
       Iir_Predefined_Physical_Greater,
       Iir_Predefined_Physical_Greater_Equal,

       Iir_Predefined_Physical_Identity,
       Iir_Predefined_Physical_Negation,
       Iir_Predefined_Physical_Absolute,

       Iir_Predefined_Physical_Plus,
       Iir_Predefined_Physical_Minus,

       Iir_Predefined_Physical_Integer_Mul,
       Iir_Predefined_Physical_Real_Mul,
       Iir_Predefined_Integer_Physical_Mul,
       Iir_Predefined_Real_Physical_Mul,
       Iir_Predefined_Physical_Integer_Div,
       Iir_Predefined_Physical_Real_Div,
       Iir_Predefined_Physical_Physical_Div,

   --  Predefined operators for access.
       Iir_Predefined_Access_Equality,
       Iir_Predefined_Access_Inequality,

   --  Predefined operators for record.
       Iir_Predefined_Record_Equality,
       Iir_Predefined_Record_Inequality,

   --  Predefined operators for array.
       Iir_Predefined_Array_Equality,
       Iir_Predefined_Array_Inequality,
       Iir_Predefined_Array_Less,
       Iir_Predefined_Array_Less_Equal,
       Iir_Predefined_Array_Greater,
       Iir_Predefined_Array_Greater_Equal,

       Iir_Predefined_Array_Array_Concat,
       Iir_Predefined_Array_Element_Concat,
       Iir_Predefined_Element_Array_Concat,
       Iir_Predefined_Element_Element_Concat,

   --  Predefined shift operators.
       Iir_Predefined_Array_Sll,
       Iir_Predefined_Array_Srl,
       Iir_Predefined_Array_Sla,
       Iir_Predefined_Array_Sra,
       Iir_Predefined_Array_Rol,
       Iir_Predefined_Array_Ror,

   --  Predefined operators for one dimensional array
       Iir_Predefined_Bit_Array_And,
       Iir_Predefined_Bit_Array_Or,
       Iir_Predefined_Bit_Array_Nand,
       Iir_Predefined_Bit_Array_Nor,
       Iir_Predefined_Bit_Array_Xor,
       Iir_Predefined_Bit_Array_Xnor,
       Iir_Predefined_Bit_Array_Not,

       Iir_Predefined_Boolean_Array_And,
       Iir_Predefined_Boolean_Array_Or,
       Iir_Predefined_Boolean_Array_Nand,
       Iir_Predefined_Boolean_Array_Nor,
       Iir_Predefined_Boolean_Array_Xor,
       Iir_Predefined_Boolean_Array_Xnor,
       Iir_Predefined_Boolean_Array_Not,

   --  Predefined attribute functions.
       Iir_Predefined_Attribute_Image,
       Iir_Predefined_Attribute_Value,
       Iir_Predefined_Attribute_Pos,
       Iir_Predefined_Attribute_Val,
       Iir_Predefined_Attribute_Succ,
       Iir_Predefined_Attribute_Pred,
       Iir_Predefined_Attribute_Leftof,
       Iir_Predefined_Attribute_Rightof,
       Iir_Predefined_Attribute_Left,
       Iir_Predefined_Attribute_Right,
       Iir_Predefined_Attribute_Event,
       Iir_Predefined_Attribute_Active,
       Iir_Predefined_Attribute_Last_Event,
       Iir_Predefined_Attribute_Last_Active,
       Iir_Predefined_Attribute_Last_Value,
       Iir_Predefined_Attribute_Driving,
       Iir_Predefined_Attribute_Driving_Value,

   --  Access procedure
       Iir_Predefined_Deallocate,

   --  file function / procedures.
       Iir_Predefined_File_Open,
       Iir_Predefined_File_Open_Status,
       Iir_Predefined_File_Close,
       Iir_Predefined_Read,
       Iir_Predefined_Read_Length,
       Iir_Predefined_Flush,
       Iir_Predefined_Write,
       Iir_Predefined_Endfile,

   --  To_String
       Iir_Predefined_Array_To_String,

   --  Predefined function.
       Iir_Predefined_Now_Function
       );

   --  Return TRUE iff FUNC is a short-cut predefined function.
   function Iir_Predefined_Shortcut_P (Func : Iir_Predefined_Functions)
     return Boolean;

   subtype Iir_Predefined_Pure_Functions is Iir_Predefined_Functions range
     Iir_Predefined_Boolean_And .. Iir_Predefined_Attribute_Driving_Value;

   subtype Iir_Predefined_Dyadic_Bit_Array_Functions
   is Iir_Predefined_Functions range
     Iir_Predefined_Bit_Array_And ..
   --Iir_Predefined_Bit_Array_Or
   --Iir_Predefined_Bit_Array_Nand
   --Iir_Predefined_Bit_Array_Nor
   --Iir_Predefined_Bit_Array_Xor
     Iir_Predefined_Bit_Array_Xnor;

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

   -- Staticness as defined by LRM93 §6.1 and §7.4
   type Iir_Staticness is (Unknown, None, Globally, Locally);

   -- Staticness as defined by LRM93 §6.1 and §7.4
   function Min (L,R: Iir_Staticness) return Iir_Staticness renames
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
     (Unconstrained, Partially_Constrained, Fully_Constrained);

   ---------------
   -- subranges --
   ---------------
   -- These subtypes are used for ranges, for `case' statments or for the `in'
   -- operator.

   -- In order to be correctly parsed by check_iir, the declaration must
   -- follow these rules:
   -- * the first line must be "subtype Iir_Kinds_NAME is Iir_Kind_range"
   -- * the second line must be the lowest bound of the range, followed by "..
   -- * comments line
   -- * the last line must be the highest bound of the range, followed by ";"

--   subtype Iir_Kinds_List is Iir_Kind range
--     Iir_Kind_List ..
--     Iir_Kind_Callees_List;

   subtype Iir_Kinds_Library_Unit_Declaration is Iir_Kind range
     Iir_Kind_Configuration_Declaration ..
   --Iir_Kind_Entity_Declaration
   --Iir_Kind_Package_Declaration
   --Iir_Kind_Package_Body
     Iir_Kind_Architecture_Declaration;

   --  Note: does not include iir_kind_enumeration_literal since it is
   --  considered as a declaration.
   subtype Iir_Kinds_Literal is Iir_Kind range
     Iir_Kind_Character_Literal ..
   --Iir_Kind_Integer_Literal
   --Iir_Kind_Floating_Point_Literal
   --Iir_Kind_Null_Literal
   --Iir_Kind_String_Literal
   --Iir_Kind_Physical_Int_Literal
   --Iir_Kind_Physical_Fp_Literal
     Iir_Kind_Bit_String_Literal;

   subtype Iir_Kinds_Array_Type_Definition is Iir_Kind range
     Iir_Kind_Array_Type_Definition ..
     Iir_Kind_Array_Subtype_Definition;

   subtype Iir_Kinds_Type_And_Subtype_Definition is Iir_Kind range
     Iir_Kind_Access_Type_Definition ..
   --Iir_Kind_Incomplete_Type_Definition
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

   subtype Iir_Kinds_Scalar_Type_Definition is Iir_Kind range
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

   subtype Iir_Kinds_Type_Declaration is Iir_Kind range
     Iir_Kind_Type_Declaration ..
   --Iir_Kind_Anonymous_Type_Declaration
     Iir_Kind_Subtype_Declaration;

   subtype Iir_Kinds_Nonoverloadable_Declaration is Iir_Kind range
     Iir_Kind_Type_Declaration ..
     Iir_Kind_Element_Declaration;

   subtype Iir_Kinds_Monadic_Operator is Iir_Kind range
     Iir_Kind_Identity_Operator ..
   --Iir_Kind_Negation_Operator
   --Iir_Kind_Absolute_Operator
     Iir_Kind_Not_Operator;

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

   subtype Iir_Kinds_Function_Declaration is Iir_Kind range
     Iir_Kind_Function_Declaration ..
     Iir_Kind_Implicit_Function_Declaration;

   subtype Iir_Kinds_Procedure_Declaration is Iir_Kind range
     Iir_Kind_Implicit_Procedure_Declaration ..
     Iir_Kind_Procedure_Declaration;

   subtype Iir_Kinds_Subprogram_Declaration is Iir_Kind range
     Iir_Kind_Function_Declaration ..
   --Iir_Kind_Implicit_Function_Declaration
   --Iir_Kind_Implicit_Procedure_Declaration
     Iir_Kind_Procedure_Declaration;

   subtype Iir_Kinds_Process_Statement is Iir_Kind range
     Iir_Kind_Sensitized_Process_Statement ..
     Iir_Kind_Process_Statement;

   subtype Iir_Kinds_Interface_Declaration is Iir_Kind range
     Iir_Kind_Constant_Interface_Declaration ..
   --Iir_Kind_Variable_Interface_Declaration
   --Iir_Kind_Signal_Interface_Declaration
     Iir_Kind_File_Interface_Declaration;

   subtype Iir_Kinds_Object_Declaration is Iir_Kind range
     Iir_Kind_Object_Alias_Declaration ..
   --Iir_Kind_File_Declaration
   --Iir_Kind_Guard_Signal_Declaration
   --Iir_Kind_Signal_Declaration
   --Iir_Kind_Variable_Declaration
   --Iir_Kind_Constant_Declaration
   --Iir_Kind_Iterator_Declaration
   --Iir_Kind_Constant_Interface_Declaration
   --Iir_Kind_Variable_Interface_Declaration
   --Iir_Kind_Signal_Interface_Declaration
     Iir_Kind_File_Interface_Declaration;

   subtype Iir_Kinds_Non_Alias_Object_Declaration is Iir_Kind range
     Iir_Kind_File_Declaration ..
   --Iir_Kind_Guard_Signal_Declaration
   --Iir_Kind_Signal_Declaration
   --Iir_Kind_Variable_Declaration
   --Iir_Kind_Constant_Declaration
   --Iir_Kind_Iterator_Declaration
   --Iir_Kind_Constant_Interface_Declaration
   --Iir_Kind_Variable_Interface_Declaration
   --Iir_Kind_Signal_Interface_Declaration
     Iir_Kind_File_Interface_Declaration;

   subtype Iir_Kinds_Association_Element is Iir_Kind range
     Iir_Kind_Association_Element_By_Expression ..
   --Iir_Kind_Association_Element_By_Individual
     Iir_Kind_Association_Element_Open;

   subtype Iir_Kinds_Choice is Iir_Kind range
     Iir_Kind_Choice_By_Others ..
   --Iir_Kind_Choice_By_Expression
   --Iir_Kind_Choice_By_Range
   --Iir_Kind_Choice_By_None
     Iir_Kind_Choice_By_Name;

   subtype Iir_Kinds_Name is Iir_Kind range
     Iir_Kind_Simple_Name ..
   --Iir_Kind_Slice_Name
   --Iir_Kind_Indexed_Name
   --Iir_Kind_Selected_Name
   --Iir_Kind_Selected_By_All_Name
     Iir_Kind_Parenthesis_Name;

   subtype Iir_Kinds_Dereference is Iir_Kind range
     Iir_Kind_Dereference ..
     Iir_Kind_Implicit_Dereference;

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


   subtype Iir_Kinds_Attribute is Iir_Kind range
     Iir_Kind_Base_Attribute ..
     Iir_Kind_Path_Name_Attribute;

   subtype Iir_Kinds_Type_Attribute is Iir_Kind range
     Iir_Kind_Left_Type_Attribute ..
   --Iir_Kind_Right_Type_Attribute
   --Iir_Kind_High_Type_Attribute
   --Iir_Kind_Low_Type_Attribute
     Iir_Kind_Ascending_Type_Attribute;

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
   --Iir_Kind_Concurrent_Conditional_Signal_Assignment
   --Iir_Kind_Concurrent_Selected_Signal_Assignment
   --Iir_Kind_Concurrent_Assertion_Statement
   --Iir_Kind_Psl_Default_Clock
   --Iir_Kind_Psl_Assert_Statement
   --Iir_Kind_Concurrent_Procedure_Call_Statement
   --Iir_Kind_Block_Statement
   --Iir_Kind_Generate_Statement
     Iir_Kind_Component_Instantiation_Statement;

   subtype Iir_Kinds_Concurrent_Signal_Assignment is Iir_Kind range
     Iir_Kind_Concurrent_Conditional_Signal_Assignment ..
     Iir_Kind_Concurrent_Selected_Signal_Assignment;

   subtype Iir_Kinds_Sequential_Statement is Iir_Kind range
     Iir_Kind_Signal_Assignment_Statement ..
   --Iir_Kind_Null_Statement
   --Iir_Kind_Assertion_Statement
   --Iir_Kind_Report_Statement
   --Iir_Kind_Wait_Statement
   --Iir_Kind_Variable_Assignment_Statement
   --Iir_Kind_Return_Statement
   --Iir_Kind_For_Loop_Statement
   --Iir_Kind_While_Loop_Statement
   --Iir_Kind_Next_Statement
   --Iir_Kind_Exit_Statement
   --Iir_Kind_Case_Statement
   --Iir_Kind_Procedure_Call_Statement
     Iir_Kind_If_Statement;

   subtype Iir_Kinds_Allocator is Iir_Kind range
     Iir_Kind_Allocator_By_Expression ..
     Iir_Kind_Allocator_By_Subtype;

   subtype Iir_Kinds_Clause is Iir_Kind range
     Iir_Kind_Library_Clause ..
     Iir_Kind_Use_Clause;

   subtype Iir_Kinds_Specification is Iir_Kind range
     Iir_Kind_Attribute_Specification ..
   --Iir_Kind_Disconnection_Specification
     Iir_Kind_Configuration_Specification;

   subtype Iir_Kinds_Declaration is Iir_Kind range
     Iir_Kind_Type_Declaration ..
   --Iir_Kind_Anonymous_Type_Declaration
   --Iir_Kind_Subtype_Declaration
   --Iir_Kind_Configuration_Declaration
   --Iir_Kind_Entity_Declaration
   --Iir_Kind_Package_Declaration
   --Iir_Kind_Package_Body
   --Iir_Kind_Architecture_Declaration
   --Iir_Kind_Unit_Declaration
   --Iir_Kind_Library_Declaration
   --Iir_Kind_Component_Declaration
   --Iir_Kind_Attribute_Declaration
   --Iir_Kind_Group_Template_Declaration
   --Iir_Kind_Group_Declaration
   --Iir_Kind_Element_Declaration
   --Iir_Kind_Non_Object_Alias_Declaration
   --Iir_Kind_Psl_Declaration
   --Iir_Kind_Function_Body
   --Iir_Kind_Function_Declaration
   --Iir_Kind_Implicit_Function_Declaration
   --Iir_Kind_Implicit_Procedure_Declaration
   --Iir_Kind_Procedure_Declaration
   --Iir_Kind_Procedure_Body
   --Iir_Kind_Enumeration_Literal
   --Iir_Kind_Object_Alias_Declaration
   --Iir_Kind_File_Declaration
   --Iir_Kind_Guard_Signal_Declaration
   --Iir_Kind_Signal_Declaration
   --Iir_Kind_Variable_Declaration
   --Iir_Kind_Constant_Declaration
   --Iir_Kind_Iterator_Declaration
   --Iir_Kind_Constant_Interface_Declaration
   --Iir_Kind_Variable_Interface_Declaration
   --Iir_Kind_Signal_Interface_Declaration
     Iir_Kind_File_Interface_Declaration;

   -------------------------------------
   -- Types and subtypes declarations --
   -------------------------------------

   -- Level 1 base class.
   subtype Iir is Nodes.Node_Type;
   subtype Iir_List is Lists.List_Type;
   Null_Iir_List : constant Iir_List := Lists.Null_List;
   Iir_List_All : constant Iir_List := Lists.List_All;
   Iir_List_Others : constant Iir_List := Lists.List_Others;
   subtype Iir_Lists_All_Others is Iir_List
     range Iir_List_Others .. Iir_List_All;

   Null_Iir : constant Iir := Nodes.Null_Node;

   function Is_Null (Node : Iir) return Boolean;
   pragma Inline (Is_Null);

   function Is_Null_List (Node : Iir_List) return Boolean;
   pragma Inline (Is_Null_List);

   function "=" (L, R : Iir) return Boolean renames Nodes."=";

   function Get_Last_Node return Iir renames Nodes.Get_Last_Node;

   function Create_Iir_List return Iir_List
     renames Lists.Create_List;
   function Get_Nth_Element (L : Iir_List; N : Natural) return Iir
     renames Lists.Get_Nth_Element;
   procedure Replace_Nth_Element (L : Iir_List; N : Natural; El : Iir)
     renames Lists.Replace_Nth_Element;
   procedure Append_Element (L : Iir_List; E : Iir)
     renames Lists.Append_Element;
   procedure Add_Element (L : Iir_List; E : Iir)
     renames Lists.Add_Element;
   procedure Destroy_Iir_List (L : in out Iir_List)
     renames Lists.Destroy_List;
   function Get_Nbr_Elements (L : Iir_List) return Natural
     renames Lists.Get_Nbr_Elements;
   procedure Set_Nbr_Elements (L : Iir_List; Nbr : Natural)
     renames Lists.Set_Nbr_Elements;
   function Get_First_Element (L : Iir_List) return Iir
     renames Lists.Get_First_Element;
   function Get_Last_Element (L : Iir_List) return Iir
     renames Lists.Get_Last_Element;
   function "=" (L, R : Iir_List) return Boolean renames Lists."=";

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
      --  All the informations come from the library file.
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
   --  The unit is obseleted (ie replaced) by a more recently analyzed design
   --  unit.another design unit.
   --  If another design unit depends (directly or not) on an obseleted design
   --  unit, it is also obselete, and cannot be defined.
   Date_Obsolete      : constant Date_Type := 0;
   --  The unit was not analyzed.
   Date_Not_Analyzed  : constant Date_Type := 1;
   --  The unit has been analyzed but it has bad dependences.
   Date_Bad_Analyze   : constant Date_Type := 2;
   --  The unit has been parsed but not analyzed.
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

   type Base_Type is (Base_2, Base_8, Base_16);

   -- design file
   subtype Iir_Design_File is Iir;

   subtype Iir_Design_Unit is Iir;

   subtype Iir_Library_Clause is Iir;

   -- Literals.
   --subtype Iir_Text_Literal is Iir;

   subtype Iir_Character_Literal is Iir;

   subtype Iir_Integer_Literal is Iir;

   subtype Iir_Floating_Point_Literal is Iir;

   subtype Iir_String_Literal is Iir;

   subtype Iir_Bit_String_Literal is Iir;

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

   -- Tuples.
   subtype Iir_Proxy is Iir;

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

   subtype Iir_Index_List is Iir_List;

   subtype Iir_Design_Unit_List is Iir_List;

   subtype Iir_Enumeration_Literal_List is Iir_List;

   subtype Iir_Designator_List is Iir_List;

   subtype Iir_Attribute_Value_Chain is Iir_List;

   subtype Iir_Overload_List is Iir;

   subtype Iir_Group_Constituent_List is Iir_List;

   subtype Iir_Callees_List is Iir_List;

   -- Declaration and children.
   subtype Iir_Entity_Declaration is Iir;

   subtype Iir_Signal_Interface_Declaration is Iir;

   subtype Iir_Architecture_Declaration is Iir;

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

   subtype Iir_Implicit_Function_Declaration is Iir;

   subtype Iir_Implicit_Procedure_Declaration is Iir;

   subtype Iir_Use_Clause is Iir;

   subtype Iir_Constant_Declaration is Iir;

   subtype Iir_Iterator_Declaration is Iir;

   subtype Iir_Constant_Interface_Declaration is Iir;

   subtype Iir_Variable_Interface_Declaration is Iir;

   subtype Iir_File_Interface_Declaration is Iir;

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
   function Get_Kind (An_Iir: Iir) return Iir_Kind;
   pragma Inline (Get_Kind);

   --  Create a new IIR of kind NEW_KIND, and copy fields from SRC to this
   --  iir.  Src fields are cleaned.
   --function Clone_Iir (Src: Iir; New_Kind : Iir_Kind) return Iir;

   procedure Set_Location (Target: Iir; Location: Location_Type)
     renames Nodes.Set_Location;
   function Get_Location (Target: Iir) return Location_Type
     renames Nodes.Get_Location;

   procedure Location_Copy (Target: Iir; Src: Iir);

   function Create_Iir (Kind: Iir_Kind) return Iir;
   function Create_Iir_Error return Iir;
   procedure Free_Iir (Target: Iir) renames Nodes.Free_Node;

   --  Disp statistics about node usage.
   procedure Disp_Stats;

   --  Design units contained in a design file.
   --  Field: Field5
   function Get_First_Design_Unit (Design : Iir) return Iir;
   procedure Set_First_Design_Unit (Design : Iir; Chain : Iir);

   --  Field: Field6
   function Get_Last_Design_Unit (Design : Iir) return Iir;
   procedure Set_Last_Design_Unit (Design : Iir; Chain : Iir);

   --  Library declaration of a library clause.
   --  Field: Field1
   function Get_Library_Declaration (Design : Iir) return Iir;
   procedure Set_Library_Declaration (Design : Iir; Library : Iir);

   -- File time stamp is the system time of the file last modification.
   --  Field: Field4 (uc)
   function Get_File_Time_Stamp (Design : Iir) return Time_Stamp_Id;
   procedure Set_File_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id);

   -- Time stamp of the last analysis system time.
   --  Field: Field3 (uc)
   function Get_Analysis_Time_Stamp (Design : Iir) return Time_Stamp_Id;
   procedure Set_Analysis_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id);

   --  The library which FILE belongs to.
   --  Field: Field0
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
   --  Field: Field0
   function Get_Design_File (Unit : Iir_Design_Unit) return Iir_Design_File;
   procedure Set_Design_File (Unit : Iir_Design_Unit; File : Iir_Design_File);

   --  Design files of a library.
   --  Field: Field1
   function Get_Design_File_Chain (Library : Iir) return Iir_Design_File;
   procedure Set_Design_File_Chain (Library : Iir; Chain : Iir_Design_File);

   --  System directory where the library is stored.
   --  Field: Field11 (pos)
   function Get_Library_Directory (Library : Iir) return Name_Id;
   procedure Set_Library_Directory (Library : Iir; Dir : Name_Id);

   -- Symbolic date, used to order design units in a library.
   --  Field: Field10 (pos)
   function Get_Date (Target : Iir) return Date_Type;
   procedure Set_Date (Target : Iir; Date : Date_Type);

   --  Chain of context clauses.
   --  Field: Field1
   function Get_Context_Items (Design_Unit : Iir) return Iir;
   procedure Set_Context_Items (Design_Unit : Iir; Items_Chain : Iir);

   --  List of design units on which the design unit depends.
   --  Field: Field8 (uc)
   function Get_Dependence_List (Unit : Iir) return Iir_List;
   procedure Set_Dependence_List (Unit : Iir; List : Iir_List);

   --  List of functions or sensitized processes whose analysis checks are not
   --  complete.
   --  These elements have direct or indirect calls to procedure whose body is
   --  not yet analyzed.  Therefore, purity or wait checks are not complete.
   --  Field: Field9 (uc)
   function Get_Analysis_Checks_List (Unit : Iir) return Iir_List;
   procedure Set_Analysis_Checks_List (Unit : Iir; List : Iir_List);

   --  Wether the unit is on disk, parsed or analyzed.
   --  Field: State1 (pos)
   function Get_Date_State (Unit : Iir_Design_Unit) return Date_State_Type;
   procedure Set_Date_State (Unit : Iir_Design_Unit; State : Date_State_Type);

   --  If TRUE, the target of the signal assignment is guarded.
   --  If FALSE, the target is not guarded.
   --  This is determined during sem by examining the declaration(s) of the
   --  target (there may be severals declarations in the case of a aggregate
   --  target).
   --  If UNKNOWN, this is not determined at compile time but at run-time.
   --  This is the case for formal signal interfaces of subprograms.
   --  Field: State3 (pos)
   function Get_Guarded_Target_State (Stmt : Iir) return Tri_State_Type;
   procedure Set_Guarded_Target_State (Stmt : Iir; State : Tri_State_Type);

   --  Library unit of a design unit.
   --  Field: Field5
   function Get_Library_Unit (Design_Unit : Iir_Design_Unit) return Iir;
   procedure Set_Library_Unit (Design_Unit : Iir_Design_Unit; Lib_Unit : Iir);
   pragma Inline (Get_Library_Unit);

   --  Every design unit is put in an hash table to find quickly found by its
   --  name.  This field is a single chain for collisions.
   --  Field: Field7
   function Get_Hash_Chain (Design_Unit : Iir_Design_Unit) return Iir;
   procedure Set_Hash_Chain (Design_Unit : Iir_Design_Unit; Chain : Iir);

   -- Set the line and the offset in the line, only for the library manager.
   -- This is valid until the file is really loaded in memory.  On loading,
   -- location will contain all this informations.
   -- Field: Field1
   -- Field: Field6
   -- Field: Field7
   procedure Set_Pos_Line_Off (Design_Unit: Iir_Design_Unit;
                               Pos : Source_Ptr; Line, Off: Natural);
   procedure Get_Pos_Line_Off (Design_Unit: Iir_Design_Unit;
                               Pos : out Source_Ptr; Line, Off: out Natural);


   --  literals.

   --  Value of an integer/physical literal.
   --  Field: Int64
   function Get_Value (Lit : Iir) return Iir_Int64;
   procedure Set_Value (Lit : Iir; Val : Iir_Int64);

   --  Position (same as lit_type'pos) of an enumeration literal.
   --  Field: Field10 (pos)
   function Get_Enum_Pos (Lit : Iir) return Iir_Int32;
   procedure Set_Enum_Pos (Lit : Iir; Val : Iir_Int32);

   --  Field: Field6
   function Get_Physical_Literal (Unit : Iir) return Iir;
   procedure Set_Physical_Literal (Unit : Iir; Lit : Iir);

   --  Value of a physical unit declaration.
   --  Field: Field7
   function Get_Physical_Unit_Value (Unit : Iir) return Iir;
   procedure Set_Physical_Unit_Value (Unit : Iir; Lit : Iir);

   --  Value of a floating point literal.
   --  Field: Fp64
   function Get_Fp_Value (Lit : Iir) return Iir_Fp64;
   procedure Set_Fp_Value (Lit : Iir; Val : Iir_Fp64);

   --  Declaration of the literal.
   --  This is used to retrieve the genuine enumeration literal for literals
   --  created from static expression.
   --  Field: Field6
   function Get_Enumeration_Decl (Target : Iir) return Iir;
   procedure Set_Enumeration_Decl (Target : Iir; Lit : Iir);

   --  List of elements of a simple aggregate.
   --  Field: Field3 (uc)
   function Get_Simple_Aggregate_List (Target : Iir) return Iir_List;
   procedure Set_Simple_Aggregate_List (Target : Iir; List : Iir_List);

   -- The logarithm of the base (1, 3 or 4) of a bit string.
   --  Field: Field11 (pos)
   function Get_Bit_String_Base (Lit : Iir) return Base_Type;
   procedure Set_Bit_String_Base (Lit : Iir; Base : Base_Type);

   --  The enumeration literal which defines the '0' and '1' value.
   --  Field: Field4
   function Get_Bit_String_0 (Lit : Iir) return Iir_Enumeration_Literal;
   procedure Set_Bit_String_0 (Lit : Iir; El : Iir_Enumeration_Literal);

   --  Field: Field5
   function Get_Bit_String_1 (Lit : Iir) return Iir_Enumeration_Literal;
   procedure Set_Bit_String_1 (Lit : Iir; El : Iir_Enumeration_Literal);

   --  The origin of a literal can be null_iir for a literal generated by the
   --  parser, or a node which was statically evaluated to this literal.
   --  Such nodes are created by eval_expr.
   --  Field: Field2
   function Get_Literal_Origin (Lit : Iir) return Iir;
   procedure Set_Literal_Origin (Lit : Iir; Orig : Iir);

   --  tuples.

   function Create_Proxy (Proxy: Iir) return Iir_Proxy;

   --  Field: Field1
   function Get_Proxy (Target : Iir_Proxy) return Iir;
   procedure Set_Proxy (Target : Iir_Proxy; Proxy : Iir);

   --  Field: Field3 (uc)
   function Get_Entity_Class (Target : Iir) return Token_Type;
   procedure Set_Entity_Class (Target : Iir; Kind : Token_Type);

   --  Field: Field1 (uc)
   function Get_Entity_Name_List (Target : Iir) return Iir_List;
   procedure Set_Entity_Name_List (Target : Iir; Names : Iir_List);

   --  Field: Field6
   function Get_Attribute_Designator (Target : Iir) return Iir;
   procedure Set_Attribute_Designator (Target : Iir; Designator : Iir);

   --  Chain of attribute specifications.  This is used only during sem, to
   --  check that no named entity of a given class appear after an attr. spec.
   --  with the entity name list OTHERS or ALL.
   --  Field: Field7
   function Get_Attribute_Specification_Chain (Target : Iir) return Iir;
   procedure Set_Attribute_Specification_Chain (Target : Iir; Chain : Iir);

   --  Field: Field4
   function Get_Attribute_Specification (Val : Iir) return Iir;
   procedure Set_Attribute_Specification (Val : Iir; Attr : Iir);

   --  Field: Field4 (uc)
   function Get_Signal_List (Target : Iir) return Iir_List;
   procedure Set_Signal_List (Target : Iir; List : Iir_List);

   --  Field: Field3
   function Get_Designated_Entity (Val : Iir_Attribute_Value) return Iir;
   procedure Set_Designated_Entity (Val : Iir_Attribute_Value; Entity : Iir);

   --  Field: Field1
   function Get_Formal (Target : Iir) return Iir;
   procedure Set_Formal (Target : Iir; Formal : Iir);

   --  Field: Field3
   function Get_Actual (Target : Iir) return Iir;
   procedure Set_Actual (Target : Iir; Actual : Iir);

   --  Field: Field4
   function Get_In_Conversion (Target : Iir) return Iir;
   procedure Set_In_Conversion (Target : Iir; Conv : Iir);

   --  Field: Field5
   function Get_Out_Conversion (Target : Iir) return Iir;
   procedure Set_Out_Conversion (Target : Iir; Conv : Iir);

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
   --  Field: Flag3
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
   --  Field: Field1
   function Get_Associated (Target : Iir) return Iir;
   procedure Set_Associated (Target : Iir; Associated : Iir);

   --  Set when a choice belongs to the same alternative as the previous one.
   --  Field: Flag1
   function Get_Same_Alternative_Flag (Target : Iir) return Boolean;
   procedure Set_Same_Alternative_Flag (Target : Iir; Val : Boolean);

   --  Field: Field2
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
   --  Field: Field4
   function Get_Prev_Block_Configuration (Target : Iir) return Iir;
   procedure Set_Prev_Block_Configuration (Target : Iir; Block : Iir);

   --  Field: Field3
   function Get_Configuration_Item_Chain (Target : Iir) return Iir;
   procedure Set_Configuration_Item_Chain (Target : Iir; Chain : Iir);

   --  Chain of attribute values for a named entity.
   --  To be used with Get/Set_Chain.
   --  There is no order, therefore, a new attribute value may be always
   --  prepended.
   --  Field: Field4
   function Get_Attribute_Value_Chain (Target : Iir) return Iir;
   procedure Set_Attribute_Value_Chain (Target : Iir; Chain : Iir);

   --  Next attribute value in the attribute specification chain (of attribute
   --  value).
   --  Field: Field0
   function Get_Spec_Chain (Target : Iir) return Iir;
   procedure Set_Spec_Chain (Target : Iir; Chain : Iir);

   --  Chain of attribute values for attribute specification.
   --  To be used with Get/Set_Spec_Chain.
   --  Field: Field4
   function Get_Attribute_Value_Spec_Chain (Target : Iir) return Iir;
   procedure Set_Attribute_Value_Spec_Chain (Target : Iir; Chain : Iir);

   --  Field: Field4
   function Get_Entity (Decl : Iir) return Iir;
   procedure Set_Entity (Decl : Iir; Entity : Iir);

   --  The package declaration corresponding to the body.
   --  Field: Field4
   function Get_Package (Package_Body : Iir) return Iir_Package_Declaration;
   procedure Set_Package (Package_Body : Iir; Decl : Iir_Package_Declaration);

   --  The package body corresponding to the package declaration.
   --  Field: Field4
   function Get_Package_Body (Pkg : Iir) return Iir_Package_Body;
   procedure Set_Package_Body (Pkg : Iir; Decl : Iir_Package_Body);

   --  If true, the package need a body.
   --  Field: Flag1
   function Get_Need_Body (Decl : Iir_Package_Declaration) return Boolean;
   procedure Set_Need_Body (Decl : Iir_Package_Declaration; Flag : Boolean);

   --  Field: Field5
   function Get_Block_Configuration (Target : Iir) return Iir;
   procedure Set_Block_Configuration (Target : Iir; Block : Iir);

   --  Field: Field5
   function Get_Concurrent_Statement_Chain (Target : Iir) return Iir;
   procedure Set_Concurrent_Statement_Chain (Target : Iir; First : Iir);

   --  Field: Field2
   function Get_Chain (Target : Iir) return Iir;
   procedure Set_Chain (Target : Iir; Chain : Iir);
   pragma Inline (Get_Chain);

   --  Field: Field7
   function Get_Port_Chain (Target : Iir) return Iir;
   procedure Set_Port_Chain (Target : Iir; Chain : Iir);

   --  Field: Field6
   function Get_Generic_Chain (Target : Iir) return Iir;
   procedure Set_Generic_Chain (Target : Iir; Generics : Iir);

   --  Field: Field1
   function Get_Type (Target : Iir) return Iir;
   procedure Set_Type (Target : Iir; Atype : Iir);
   pragma Inline (Get_Type);

   --  The subtype definition associated with the type declaration (if any).
   --  Field: Field4
   function Get_Subtype_Definition (Target : Iir) return Iir;
   procedure Set_Subtype_Definition (Target : Iir; Def : Iir);

   --  Mode of interfaces or file (v87).
   --  Field: Odigit1 (pos)
   function Get_Mode (Target : Iir) return Iir_Mode;
   procedure Set_Mode (Target : Iir; Mode : Iir_Mode);

   --  Field: State3 (pos)
   function Get_Signal_Kind (Target : Iir) return Iir_Signal_Kind;
   procedure Set_Signal_Kind (Target : Iir; Signal_Kind : Iir_Signal_Kind);

   --  The base name of a name is the node at the origin of the name.
   --  The base name is a declaration (signal, object, constant or interface),
   --  a selected_by_all name, an implicit_dereference name.
   --  Field: Field5
   function Get_Base_Name (Target : Iir) return Iir;
   procedure Set_Base_Name (Target : Iir; Name : Iir);
   pragma Inline (Get_Base_Name);

   --  Field: Field5
   function Get_Interface_Declaration_Chain (Target : Iir) return Iir;
   procedure Set_Interface_Declaration_Chain (Target : Iir; Chain : Iir);
   pragma Inline (Get_Interface_Declaration_Chain);

   --  Field: Field4
   function Get_Subprogram_Specification (Target : Iir) return Iir;
   procedure Set_Subprogram_Specification (Target : Iir; Spec : Iir);

   --  Field: Field5
   function Get_Sequential_Statement_Chain (Target : Iir) return Iir;
   procedure Set_Sequential_Statement_Chain (Target : Iir; Chain : Iir);

   --  Field: Field6
   function Get_Subprogram_Body (Target : Iir) return Iir;
   procedure Set_Subprogram_Body (Target : Iir; A_Body : Iir);

   --  Several subprograms in a declarative region may have the same
   --  identifier.  If the overload number is not 0, it is the rank of the
   --  subprogram.  If the overload number is 0, then the identifier is not
   --  overloaded in the declarative region.
   --  Field: Field9 (pos)
   function Get_Overload_Number (Target : Iir) return Iir_Int32;
   procedure Set_Overload_Number (Target : Iir; Val : Iir_Int32);

   --  Depth of a subprogram.
   --  For a subprogram declared immediatly within an entity, architecture,
   --  package, process, block, generate, the depth is 0.
   --  For a subprogram declared immediatly within a subprogram of level N,
   --  the depth is N + 1.
   --  Depth is used with depth of impure objects to check purity rules.
   --  Field: Field10 (pos)
   function Get_Subprogram_Depth (Target : Iir) return Iir_Int32;
   procedure Set_Subprogram_Depth (Target : Iir; Depth : Iir_Int32);

   --  Hash of a subprogram profile.
   --  This is used to speed up subprogram profile comparaison, which is very
   --  often used by overload.
   --  Field: Field11 (pos)
   function Get_Subprogram_Hash (Target : Iir) return Iir_Int32;
   procedure Set_Subprogram_Hash (Target : Iir; Val : Iir_Int32);
   pragma Inline (Get_Subprogram_Hash);

   --  Index for extra infos.
   --  Subprograms and processes need a lot of field in their nodes.
   --  Unfortunatly, the size of the nodes is limited and these infos are
   --  only used for optimization.
   --  This is an index into a separate table.
   --  Field: Field8 (pos)
   function Get_Extra_Info (Target : Iir) return Iir_Int32;
   procedure Set_Extra_Info (Target : Iir; Info : Iir_Int32);

   --  Depth of the deepest impure object.
   --  Field: Field3 (uc)
   function Get_Impure_Depth (Target : Iir) return Iir_Int32;
   procedure Set_Impure_Depth (Target : Iir; Depth : Iir_Int32);

   --  Field: Field1
   function Get_Return_Type (Target : Iir) return Iir;
   procedure Set_Return_Type (Target : Iir; Decl : Iir);
   pragma Inline (Get_Return_Type);

   --  Code of an implicit subprogram definition.
   --  Field: Field6 (pos)
   function Get_Implicit_Definition (D : Iir) return Iir_Predefined_Functions;
   procedure Set_Implicit_Definition (D : Iir; Def : Iir_Predefined_Functions);

   --  For an implicit subprogram, the type_reference is the type declaration
   --  for which the implicit subprogram was defined.
   --  Field: Field10
   function Get_Type_Reference (Target : Iir) return Iir;
   procedure Set_Type_Reference (Target : Iir; Decl : Iir);

   --  Get the default value of an object declaration.
   --  Null_iir if no default value.
   --  Field: Field6
   function Get_Default_Value (Target : Iir) return Iir;
   procedure Set_Default_Value (Target : Iir; Value : Iir);

   --  The deferred_declaration field points to the deferred constant
   --  declaration for a full constant declaration, or is null_iir for a
   --  usual or deferred constant declaration.
   --  Set only during sem.
   --  Field: Field7
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
   function Get_Design_Unit (Target : Iir) return Iir_Design_Unit;
   procedure Set_Design_Unit (Target : Iir; Unit : Iir_Design_Unit);

   --  Field: Field7
   function Get_Block_Statement (Target : Iir) return Iir_Block_Statement;
   procedure Set_Block_Statement (Target : Iir; Block : Iir_Block_Statement);

   --  For a non-resolved signal: null_iir if the signal has no driver, or
   --  a process/concurrent_statement for which the signal should have a
   --  driver.  This is used to catch at analyse time unresolved signals with
   --  several drivers.
   --  Field: Field7
   function Get_Signal_Driver (Target : Iir_Signal_Declaration) return Iir;
   procedure Set_Signal_Driver (Target : Iir_Signal_Declaration; Driver : Iir);

   --  Field: Field1
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

   --  Field: Field2
   function Get_Element_Declaration (Target : Iir) return Iir;
   procedure Set_Element_Declaration (Target : Iir; El : Iir);

   --  Field: Field2
   function Get_Selected_Element (Target : Iir) return Iir;
   procedure Set_Selected_Element (Target : Iir; El : Iir);

   --  Field: Field2 (uc)
   function Get_Suffix_Identifier (Target : Iir) return Name_Id;
   procedure Set_Suffix_Identifier (Target : Iir; Ident : Name_Id);

   --  Field: Field2 (uc)
   function Get_Attribute_Identifier (Target : Iir) return Name_Id;
   procedure Set_Attribute_Identifier (Target : Iir; Ident : Name_Id);

   --  Selected names of an use_clause are chained.
   --  Field: Field3
   function Get_Use_Clause_Chain (Target : Iir) return Iir;
   procedure Set_Use_Clause_Chain (Target : Iir; Chain : Iir);

   --  Selected name of an use_clause.
   --  Field: Field1
   function Get_Selected_Name (Target : Iir_Use_Clause) return Iir;
   procedure Set_Selected_Name (Target : Iir_Use_Clause; Name : Iir);

   --  The type declarator which declares the type definition TARGET.
   --  Field: Field3
   function Get_Type_Declarator (Target : Iir) return Iir;
   procedure Set_Type_Declarator (Target : Iir; Decl : Iir);

   --  Field: Field2 (uc)
   function Get_Enumeration_Literal_List (Target : Iir) return Iir_List;
   procedure Set_Enumeration_Literal_List (Target : Iir; List : Iir_List);

   --  Field: Field1
   function Get_Entity_Class_Entry_Chain (Target : Iir) return Iir;
   procedure Set_Entity_Class_Entry_Chain (Target : Iir; Chain : Iir);

   --  Field: Field1 (uc)
   function Get_Group_Constituent_List (Group : Iir) return Iir_List;
   procedure Set_Group_Constituent_List (Group : Iir; List : Iir_List);

   --  Chain of physical type units.
   --  The first unit is the primary unit.  If you really need the primary
   --  unit (and not the chain), you'd better to use Get_Primary_Unit.
   --  Field: Field1
   function Get_Unit_Chain (Target : Iir) return Iir;
   procedure Set_Unit_Chain (Target : Iir; Chain : Iir);

   --  Alias of Get_Unit_Chain.
   --  Return the primary unit of a physical type.
   --  Field: Field1
   function Get_Primary_Unit (Target : Iir) return Iir;

   --  Get/Set the identifier of a declaration.
   --  Can also be used instead of get/set_label.
   --  Field: Field3 (uc)
   function Get_Identifier (Target : Iir) return Name_Id;
   procedure Set_Identifier (Target : Iir; Identifier : Name_Id);
   pragma Inline (Get_Identifier);

   --  Field: Field3 (uc)
   function Get_Label (Target : Iir) return Name_Id;
   procedure Set_Label (Target : Iir; Label : Name_Id);

   --  Get/Set the visible flag of a declaration.
   --  The visible flag is true to make invalid the use of the identifier
   --  during its declaration.  It is set to false when the identifier is added
   --  to the name table, and set to true when the declaration is finished.
   --  Field: Flag4
   function Get_Visible_Flag (Target : Iir) return Boolean;
   procedure Set_Visible_Flag (Target : Iir; Flag : Boolean);

   --  Field: Field1
   function Get_Range_Constraint (Target : Iir) return Iir;
   procedure Set_Range_Constraint (Target : Iir; Constraint : Iir);

   --  Field: State2 (pos)
   function Get_Direction (Decl : Iir) return Iir_Direction;
   procedure Set_Direction (Decl : Iir; Dir : Iir_Direction);

   --  Field: Field2
   function Get_Left_Limit (Decl : Iir_Range_Expression) return Iir;
   procedure Set_Left_Limit (Decl : Iir_Range_Expression; Limit : Iir);

   --  Field: Field3
   function Get_Right_Limit (Decl : Iir_Range_Expression) return Iir;
   procedure Set_Right_Limit (Decl : Iir_Range_Expression; Limit : Iir);

   --  Field: Field4
   function Get_Base_Type (Decl : Iir) return Iir;
   procedure Set_Base_Type (Decl : Iir; Base_Type : Iir);
   pragma Inline (Get_Base_Type);

   --  Field: Field5
   function Get_Resolution_Function (Decl : Iir) return Iir;
   procedure Set_Resolution_Function (Decl : Iir; Func : Iir);

   --  True if ATYPE defines std.textio.text file type.
   --  Field: Flag4
   function Get_Text_File_Flag (Atype : Iir) return Boolean;
   procedure Set_Text_File_Flag (Atype : Iir; Flag : Boolean);

   --  True if enumeration type ATYPE has only character literals.
   --  Field: Flag4
   function Get_Only_Characters_Flag (Atype : Iir) return Boolean;
   procedure Set_Only_Characters_Flag (Atype : Iir; Flag : Boolean);

   --  Field: State1 (pos)
   function Get_Type_Staticness (Atype : Iir) return Iir_Staticness;
   procedure Set_Type_Staticness (Atype : Iir; Static : Iir_Staticness);

   --  Field: State2 (pos)
   function Get_Constraint_State (Atype : Iir) return Iir_Constraint;
   procedure Set_Constraint_State (Atype : Iir; State : Iir_Constraint);

   --  Field: Field6 (uc)
   function Get_Index_Subtype_List (Decl : Iir) return Iir_List;
   procedure Set_Index_Subtype_List (Decl : Iir; List : Iir_List);

   --  Field: Field2 (uc)
   function Get_Index_List (Decl : Iir) return Iir_List;
   procedure Set_Index_List (Decl : Iir; List : Iir_List);

   --  Field: Field1
   function Get_Element_Subtype (Decl : Iir) return Iir;
   procedure Set_Element_Subtype (Decl : Iir; Sub_Type : Iir);

   --  Chains of elements of a record.
   --  Field: Field1 (uc)
   function Get_Elements_Declaration_List (Decl : Iir) return Iir_List;
   procedure Set_Elements_Declaration_List (Decl : Iir; List : Iir_List);

   --  Field: Field2
   function Get_Designated_Type (Target : Iir) return Iir;
   procedure Set_Designated_Type (Target : Iir; Dtype : Iir);

   --  Field: Field1
   function Get_Target (Target : Iir) return Iir;
   procedure Set_Target (Target : Iir; Atarget : Iir);

   --  Field: Field5
   function Get_Waveform_Chain (Target : Iir) return Iir_Waveform_Element;
   procedure Set_Waveform_Chain (Target : Iir; Chain : Iir_Waveform_Element);

   --  Field: Field8
   function Get_Guard (Target : Iir) return Iir;
   procedure Set_Guard (Target : Iir; Guard : Iir);

   --  Field: Field12 (pos)
   function Get_Delay_Mechanism (Target : Iir) return Iir_Delay_Mechanism;
   procedure Set_Delay_Mechanism (Target : Iir; Kind : Iir_Delay_Mechanism);

   --  Field: Field6
   function Get_Reject_Time_Expression (Target : Iir) return Iir;
   procedure Set_Reject_Time_Expression (Target : Iir; Expr : Iir);

   --  Field: Field6 (uc)
   function Get_Sensitivity_List (Wait : Iir) return Iir_List;
   procedure Set_Sensitivity_List (Wait : Iir; List : Iir_List);

   --  Field: Field5
   function Get_Condition_Clause (Wait : Iir_Wait_Statement) return Iir;
   procedure Set_Condition_Clause (Wait : Iir_Wait_Statement; Cond : Iir);

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
   --  Field: Field7 (uc)
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
   --  Field: Flag7
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

   --  Get/Set wether the subprogram may be called by a sensitized process
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
   --  TRUE if the declaration was decored with the std.foreign attribute.
   --  Field: Flag3
   function Get_Foreign_Flag (Decl : Iir) return Boolean;
   procedure Set_Foreign_Flag (Decl : Iir; Flag : Boolean);

   --  Get/Set the resolved flag of a subtype definition.
   --  A subtype definition may be resolved either because a
   --  resolution_function_name is present in the subtype_indication, or
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

   --  Set on an array_subtype if there is an index constraint.
   --  If not set, the subtype is unconstrained.
   --  Field: Flag4
   function Get_Index_Constraint_Flag (Atype : Iir) return Boolean;
   procedure Set_Index_Constraint_Flag (Atype : Iir; Flag : Boolean);

   --  Condition of an assertion.
   --  Field: Field1
   function Get_Assertion_Condition (Target : Iir) return Iir;
   procedure Set_Assertion_Condition (Target : Iir; Cond : Iir);

   --  Report expression of an assertion or report statement.
   --  Field: Field6
   function Get_Report_Expression (Target : Iir) return Iir;
   procedure Set_Report_Expression (Target : Iir; Expr : Iir);

   --  Severity expression of an assertion or report statement.
   --  Field: Field5
   function Get_Severity_Expression (Target : Iir) return Iir;
   procedure Set_Severity_Expression (Target : Iir; Expr : Iir);

   --  Instantiated unit of a component instantiation statement.
   --  Field: Field1
   function Get_Instantiated_Unit (Target : Iir) return Iir;
   procedure Set_Instantiated_Unit (Target : Iir; Unit : Iir);

   --  Generic map aspect list.
   --  Field: Field8
   function Get_Generic_Map_Aspect_Chain (Target : Iir) return Iir;
   procedure Set_Generic_Map_Aspect_Chain (Target : Iir; Generics : Iir);

   --  Port map aspect list.
   --  Field: Field9
   function Get_Port_Map_Aspect_Chain (Target : Iir) return Iir;
   procedure Set_Port_Map_Aspect_Chain (Target : Iir; Port : Iir);

   --  Configuration of an entity_aspect_configuration.
   --  Field: Field1
   function Get_Configuration (Target : Iir) return Iir;
   procedure Set_Configuration (Target : Iir; Conf : Iir);

   --  Component configuration for a component_instantiation_statement.
   --  Field: Field6
   function Get_Component_Configuration (Target : Iir) return Iir;
   procedure Set_Component_Configuration (Target : Iir; Conf : Iir);

   --  Configuration specification for a component_instantiation_statement.
   --  Field: Field7
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

   --  Field: Field7
   function Get_Selected_Waveform_Chain (Target : Iir) return Iir;
   procedure Set_Selected_Waveform_Chain (Target : Iir; Chain : Iir);

   --  Field: Field7
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
   --  Field: Field6 (uc)
   function Get_Guard_Sensitivity_List (Guard : Iir) return Iir_List;
   procedure Set_Guard_Sensitivity_List (Guard : Iir; List : Iir_List);

   --  Block_Configuration that applies to this block statement.
   --  Field: Field6
   function Get_Block_Block_Configuration (Block : Iir) return Iir;
   procedure Set_Block_Block_Configuration (Block : Iir; Conf : Iir);

   --  Field: Field7
   function Get_Block_Header (Target : Iir) return Iir;
   procedure Set_Block_Header (Target : Iir; Header : Iir);

   --  Get/Set the block_configuration (there may be several
   --  block_configuration through the use of prev_configuration singly linked
   --  list) that apply to this generate statement.
   --  Field: Field7
   function Get_Generate_Block_Configuration (Target : Iir) return Iir;
   procedure Set_Generate_Block_Configuration (Target : Iir; Conf : Iir);

   --  Field: Field6
   function Get_Generation_Scheme (Target : Iir) return Iir;
   procedure Set_Generation_Scheme (Target : Iir; Scheme : Iir);

   --  Condition of a conditionam_waveform, if_statement, elsif,
   --  while_loop_statement, next_statement or exit_statement.
   --  Field: Field1
   function Get_Condition (Target : Iir) return Iir;
   procedure Set_Condition (Target : Iir; Condition : Iir);

   --  Field: Field6
   function Get_Else_Clause (Target : Iir) return Iir_Elsif;
   procedure Set_Else_Clause (Target : Iir; Clause : Iir_Elsif);

   --  Iterator of a for_loop_statement.
   --  Field: Field1
   function Get_Iterator_Scheme (Target : Iir) return Iir;
   procedure Set_Iterator_Scheme (Target : Iir; Iterator : Iir);

   --  Get/Set the statement in which TARGET appears.  This is used to check
   --  if next/exit is in a loop.
   --  Field: Field0
   function Get_Parent (Target : Iir) return Iir;
   procedure Set_Parent (Target : Iir; Parent : Iir);

   --  Loop label for an exit_statement or next_statement.
   --  Field: Field5
   function Get_Loop (Target : Iir) return Iir;
   procedure Set_Loop (Target : Iir; Stmt : Iir);

   --  Component name for a component_configuration or
   --  a configuration_specification.
   --  Field: Field4
   function Get_Component_Name (Target : Iir) return Iir;
   procedure Set_Component_Name (Target : Iir; Name : Iir);

   --  Field: Field1 (uc)
   function Get_Instantiation_List (Target : Iir) return Iir_List;
   procedure Set_Instantiation_List (Target : Iir; List : Iir_List);

   --  Field: Field3
   function Get_Entity_Aspect (Target : Iir_Binding_Indication) return Iir;
   procedure Set_Entity_Aspect (Target : Iir_Binding_Indication; Entity : Iir);

   --  Field: Field1
   function Get_Default_Entity_Aspect (Target : Iir) return Iir;
   procedure Set_Default_Entity_Aspect (Target : Iir; Aspect : Iir);

   --  Field: Field6
   function Get_Default_Generic_Map_Aspect_Chain (Target : Iir) return Iir;
   procedure Set_Default_Generic_Map_Aspect_Chain (Target : Iir; Chain : Iir);

   --  Field: Field7
   function Get_Default_Port_Map_Aspect_Chain (Target : Iir) return Iir;
   procedure Set_Default_Port_Map_Aspect_Chain (Target : Iir; Chain : Iir);

   --  Field: Field3
   function Get_Binding_Indication (Target : Iir) return Iir;
   procedure Set_Binding_Indication (Target : Iir; Binding : Iir);

   --  The named entity designated by a name.
   --  Field: Field4
   function Get_Named_Entity (Target : Iir) return Iir;
   procedure Set_Named_Entity (Target : Iir; Val : Iir);

   --  Expression staticness, defined by rules of LRM 7.4
   --  Field: State1 (pos)
   function Get_Expr_Staticness (Target : Iir) return Iir_Staticness;
   procedure Set_Expr_Staticness (Target : Iir; Static : Iir_Staticness);

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
   --  Field: Field3
   function Get_Prefix (Target : Iir) return Iir;
   procedure Set_Prefix (Target : Iir; Prefix : Iir);

   --  Suffix of a slice or attribute.
   --  Field: Field2
   function Get_Suffix (Target : Iir) return Iir;
   procedure Set_Suffix (Target : Iir; Suffix : Iir);

   --  Set the designated index subtype of an array attribute.
   --  Field: Field2
   function Get_Index_Subtype (Attr : Iir) return Iir;
   procedure Set_Index_Subtype (Attr : Iir; St : Iir);

   --  Parameter of an attribute.
   --  Field: Field4
   function Get_Parameter (Target : Iir) return Iir;
   procedure Set_Parameter (Target : Iir; Param : Iir);

   --  Type of the actual for an association by individual.
   --  Unless the formal is an unconstrained array type, this is the same as
   --  the formal type.
   --  Field: Field3
   function Get_Actual_Type (Target : Iir) return Iir;
   procedure Set_Actual_Type (Target : Iir; Atype : Iir);

   --  List of individual associations for association_element_by_individual.
   --  Associations for parenthesis_name.
   --  Field: Field2
   function Get_Association_Chain (Target : Iir) return Iir;
   procedure Set_Association_Chain (Target : Iir; Chain : Iir);

   --  List of individual associations for association_element_by_individual.
   --  Field: Field4
   function Get_Individual_Association_Chain (Target : Iir) return Iir;
   procedure Set_Individual_Association_Chain (Target : Iir; Chain : Iir);

   --  Get/Set info for the aggregate.
   --  There is one aggregate_info for for each dimension.
   --  Field: Field2
   function Get_Aggregate_Info (Target : Iir) return Iir_Aggregate_Info;
   procedure Set_Aggregate_Info (Target : Iir; Info : Iir_Aggregate_Info);

   --  Get/Set the info node for the next dimension.
   --  Field: Field1
   function Get_Sub_Aggregate_Info (Target : Iir) return Iir_Aggregate_Info;
   procedure Set_Sub_Aggregate_Info (Target : Iir; Info : Iir_Aggregate_Info);

   --  TRUE when the length of the aggregate is not locally static.
   --  Field: Flag3
   function Get_Aggr_Dynamic_Flag (Target : Iir) return Boolean;
   procedure Set_Aggr_Dynamic_Flag (Target : Iir; Val : Boolean);

   --  Get/Set the maximum number of elements for the lowest dimension of
   --  the aggregate or for the current dimension of a sub-aggregate.
   --  The real number of elements may be greater than this number if there
   --  is an 'other' choice.
   --  Field: Field4 (uc)
   function Get_Aggr_Max_Length (Info : Iir_Aggregate_Info) return Iir_Int32;
   procedure Set_Aggr_Max_Length (Info : Iir_Aggregate_Info; Nbr : Iir_Int32);

   --  Highest index choice, if any.
   --  Field: Field2
   function Get_Aggr_Low_Limit (Target : Iir_Aggregate_Info) return Iir;
   procedure Set_Aggr_Low_Limit (Target : Iir_Aggregate_Info; Limit : Iir);

   --  Highest index choice, if any.
   --  Field: Field3
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

   --  Staticness of the expressions in an aggregate.
   --  We can't use expr_staticness for this purpose, since the staticness
   --  of an aggregate is at most globally.
   --  Field: State2 (pos)
   function Get_Value_Staticness (Target : Iir) return Iir_Staticness;
   procedure Set_Value_Staticness (Target : Iir; Staticness : Iir_Staticness);

   --  Chain of choices.
   --  Field: Field4
   function Get_Association_Choices_Chain (Target : Iir) return Iir;
   procedure Set_Association_Choices_Chain (Target : Iir; Chain : Iir);

   --  Chain of choices.
   --  Field: Field1
   function Get_Case_Statement_Alternative_Chain (Target : Iir) return Iir;
   procedure Set_Case_Statement_Alternative_Chain (Target : Iir; Chain : Iir);

   --  Staticness of the choice.
   --  Field: State2 (pos)
   function Get_Choice_Staticness (Target : Iir) return Iir_Staticness;
   procedure Set_Choice_Staticness (Target : Iir; Staticness : Iir_Staticness);

   --  Field: Field1
   function Get_Procedure_Call (Stmt : Iir) return Iir;
   procedure Set_Procedure_Call (Stmt : Iir; Call : Iir);

   --  Subprogram to be called by a procedure, function call or operator.
   --  Field: Field3
   function Get_Implementation (Target : Iir) return Iir;
   procedure Set_Implementation (Target : Iir; Decl : Iir);

   --  Paramater associations for procedure and function call.
   --  Field: Field2
   function Get_Parameter_Association_Chain (Target : Iir) return Iir;
   procedure Set_Parameter_Association_Chain (Target : Iir; Chain : Iir);

   --  Object of a method call.  NULL_IIR if the subprogram is not a method.
   --  Field: Field4
   function Get_Method_Object (Target : Iir) return Iir;
   procedure Set_Method_Object (Target : Iir; Object : Iir);

   --  The type_mark that appeared in the subtype indication.
   --  May be null_iir if there is no type mark (as in an iterator).
   --  May differ from base_type, if the type_mark is a subtype_name.
   --  Field: Field2
   function Get_Type_Mark (Target : Iir) return Iir;
   procedure Set_Type_Mark (Target : Iir; Mark : Iir);

   --  Get/set the lexical layout of an interface.
   --  Field: Odigit2 (pos)
   function Get_Lexical_Layout (Decl : Iir) return Iir_Lexical_Layout_Type;
   procedure Set_Lexical_Layout (Decl : Iir; Lay : Iir_Lexical_Layout_Type);

   --  List of use (designated type of access types) of an incomplete type
   --  definition.  The purpose is to complete the uses with the full type
   --  definition.
   --  Field: Field2 (uc)
   function Get_Incomplete_Type_List (Target : Iir) return Iir_List;
   procedure Set_Incomplete_Type_List (Target : Iir; List : Iir_List);

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
   function Get_Type_Marks_List (Target : Iir) return Iir_List;
   procedure Set_Type_Marks_List (Target : Iir; List : Iir_List);

   --  Field: Field5
   function Get_Signature (Target : Iir) return Iir;
   procedure Set_Signature (Target : Iir; Value : Iir);

   --  Field: Field1 (uc)
   function Get_Overload_List (Target : Iir) return Iir_List;
   procedure Set_Overload_List (Target : Iir; List : Iir_List);

   --  Identifier of the simple_name attribute.
   --  Field: Field2 (uc)
   function Get_Simple_Name_Identifier (Target : Iir) return Name_Id;
   procedure Set_Simple_Name_Identifier (Target : Iir; Ident : Name_Id);

   --  Body of a protected type declaration.
   --  Field: Field2
   function Get_Protected_Type_Body (Target : Iir) return Iir;
   procedure Set_Protected_Type_Body (Target : Iir; Bod : Iir);

   --  Corresponsing protected type declaration of a protected type body.
   --  Field: Field4
   function Get_Protected_Type_Declaration (Target : Iir) return Iir;
   procedure Set_Protected_Type_Declaration (Target : Iir; Decl : Iir);

   --  Location of the 'end' token.
   --  Field: Field6 (uc)
   function Get_End_Location (Target : Iir) return Location_Type;
   procedure Set_End_Location (Target : Iir; Loc : Location_Type);

   --  For a string literal: the string identifier.
   --  Field: Field3 (uc)
   function Get_String_Id (Lit : Iir) return String_Id;
   procedure Set_String_Id (Lit : Iir; Id : String_Id);

   --  For a string literal: the string length.
   --  Field: Field0 (uc)
   function Get_String_Length (Lit : Iir) return Int32;
   procedure Set_String_Length (Lit : Iir; Len : Int32);

   --  For a declaration: true if the declaration is used somewhere.
   --  Field: Flag6
   function Get_Use_Flag (Decl : Iir) return Boolean;
   procedure Set_Use_Flag (Decl : Iir; Val : Boolean);

   --  Field: Field1 (uc)
   function Get_Psl_Property (Decl : Iir) return PSL_Node;
   procedure Set_Psl_Property (Decl : Iir; Prop : PSL_Node);

   --  Field: Field1 (uc)
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
end Iirs;
