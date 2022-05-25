--  Common operations on nodes.
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
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;
with PSL.Types; use PSL.Types;

package Vhdl.Utils is
   --  Get identifier of NODE as a string.
   function Image_Identifier (Node : Iir) return String;
   function Image_String_Lit (Str : Iir) return String;

   --  Return True iff N is an error node.
   function Is_Error (N : Iir) return Boolean;
   pragma Inline (Is_Error);

   --  Return True iff N is an overflow_literal node.
   function Is_Overflow_Literal (N : Iir) return Boolean;
   pragma Inline (Is_Overflow_Literal);

   --  If N is a literal and has a literal origin, return the literal origin.
   --  Otherwise return N.
   --  In other words, return the node as it was.
   function Strip_Literal_Origin (N : Iir) return Iir;

   --  Find LIT in the list of identifiers or characters LIST.
   --  Return the literal (whose name is LIT) or null_iir if not found.
   function Find_Name_In_Chain (Chain: Iir; Lit: Name_Id) return Iir;
   function Find_Name_In_Flist (List : Iir_Flist; Lit: Name_Id) return Iir;

   --  Return TRUE if EL in an element of chain CHAIN.
   function Is_In_Chain (Chain : Iir; El : Iir) return Boolean;

   --  Convert a list L to an Flist, and free L.
   function List_To_Flist (L : Iir_List) return Iir_Flist;

   --  Return a copy of the LEN first elements of L.  L is destroyed.
   function Truncate_Flist (L : Iir_Flist; Len : Natural) return Iir_Flist;

   --  Convert an operator node to a name.
   function Get_Operator_Name (Op : Iir) return Name_Id;

   -- Get the longest static prefix of EXPR.
   -- See LRM93 8.1
   function Get_Longest_Static_Prefix (Expr: Iir) return Iir;

   --  Get the prefix of NAME, ie the declaration at the base of NAME.
   --  Return NAME itself if NAME is not an object or a subelement of
   --  an object.  If WITH_ALIAS is true, continue with the alias name when an
   --  alias is found, else return the alias.
   --  FIXME: clarify when NAME is returned.
   function Get_Object_Prefix (Name: Iir; With_Alias : Boolean := True)
                              return Iir;


   --  Return TRUE if NAME is a name that designate an object (ie a constant,
   --  a variable, a signal or a file).
   function Is_Object_Name (Name : Iir) return Boolean;

   --  Return an object node if NAME designates an object (ie either is an
   --  object or a name for an object).
   --  Otherwise, returns NULL_IIR.
   --  For the definition of an object, see LRM08 6.4 Objects.
   function Name_To_Object (Name : Iir) return Iir;

   --  Return the value designated by NAME.  This is often an object, but can
   --  also be an expression like a function call or an attribute.
   function Name_To_Value (Name : Iir) return Iir;

   --  Return TRUE iff EXPR is a signal name.
   function Is_Signal_Name (Expr : Iir) return Boolean;

   --  Return TRUE iff EXPR is a quantity name.
   function Is_Quantity_Name (Expr : Iir) return Boolean;

   --  Get the interface corresponding to the formal name FORMAL.  This is
   --  always an interface, even if the formal is a name.
   function Get_Interface_Of_Formal (Formal : Iir) return Iir;

   --  Get the corresponding interface of an association while walking on
   --  associations.  ASSOC and INTER are the current association and
   --  interface (initialized to the association chain and interface chain).
   --  The function Get_Association_Interface return the interface associated
   --  to ASSOC,and Next_Association_Interface updates ASSOC and INTER.
   function Get_Association_Interface (Assoc : Iir; Inter : Iir) return Iir;
   procedure Next_Association_Interface
     (Assoc : in out Iir; Inter : in out Iir);

   --  Return the formal of ASSOC as a named entity (either an interface
   --  declaration or indexed/sliced/selected name of it).  If there is no
   --  formal in ASSOC, return the corresponding interface INTER.
   function Get_Association_Formal (Assoc : Iir; Inter : Iir) return Iir;

   --  Return the first association in ASSOC_CHAIN for interface INTER.  This
   --  is the first in case of individual association.
   --  Return NULL_IIR if not found (not present).
   function Find_First_Association_For_Interface
     (Assoc_Chain : Iir; Inter_Chain : Iir; Inter : Iir) return Iir;

   --  Return True iff interface INTER is a (subprogram) parameter.
   function Is_Parameter (Inter : Iir) return Boolean;

   --  Return True iff parameter INTER should be copied back (for out/inout
   --  variable).
   function Is_Copyback_Parameter (Inter : Iir) return Boolean;

   --  Duplicate enumeration literal LIT.
   function Copy_Enumeration_Literal (Lit : Iir) return Iir;

   --  True if EXPR can be built statically.  This is the case of literals
   --  (except overflow), and the case of some aggregates.
   --  This is different from locally static expression, particularly for
   --  aggregate: the analyzer may choose to dynamically create a locally
   --  static aggregate if it is sparse.
   function Is_Static_Construct (Expr : Iir) return Boolean;

   --  Make TARGETS depends on UNIT.
   --  UNIT must be either a design unit or a entity_aspect_entity.
   procedure Add_Dependence (Target: Iir_Design_Unit; Unit: Iir);

   --  Get the design_unit from dependency DEP.  DEP must be an element of
   --  a dependencies list.
   function Get_Unit_From_Dependence (Dep : Iir) return Iir;

   --  Clear configuration field of all component instantiation of
   --  the concurrent statements of PARENT.
   procedure Clear_Instantiation_Configuration (Parent : Iir);

   --  Free Node and its prefixes, if any.
   procedure Free_Name (Node : Iir);

   --  Free NODE and its sub-nodes.
   procedure Free_Recursive (Node : Iir; Free_List : Boolean := False);

   --  Free nodes in LIST.
   procedure Free_Recursive_List (List : Iir_List);

   --  Name of FUNC.
   function Get_Predefined_Function_Name (Func : Iir_Predefined_Functions)
     return String;

   --  Create the range_constraint node for an enumeration type.
   procedure Create_Range_Constraint_For_Enumeration_Type
     (Def : Iir_Enumeration_Type_Definition);

   --  Return the node containing the Callees_List (ie the subprogram body if
   --  SUBPRG is a subprogram spec, SUBPRG if SUBPRG is a process).
   function Get_Callees_List_Holder (Subprg : Iir) return Iir;

   --  Clear flag of TOP and all of its callees.
   procedure Clear_Seen_Flag (Top : Iir);

   --  Return the base type of ATYPE.  Will always return ATYPE if ATYPE is
   --  a proper type (and not a subtype).
   function Get_Base_Type (Atype : Iir) return Iir;

   --  Return TRUE iff DEF is an anonymous type (or subtype) definition.
   --  Note: DEF is required to be a type (or subtype) definition.
   --  Note: type (and not subtype) are never anonymous.
   function Is_Anonymous_Type_Definition (Def : Iir) return Boolean;
   pragma Inline (Is_Anonymous_Type_Definition);

   --  Likewise but for natures.
   function Is_Anonymous_Nature_Definition (Def : Iir) return Boolean;
   pragma Inline (Is_Anonymous_Nature_Definition);

   --  Return TRUE iff DEF is a fully constrained type (or subtype) definition.
   function Is_Fully_Constrained_Type (Def : Iir) return Boolean;

   --  Return TRUE iff DEF is an array type (or subtype) definition.
   function Is_Array_Type (Def : Iir) return Boolean;

   --  Return True iff OBJ can be the target of an aggregate with an others
   --  choice (cf LRM08 9.3.3.3).
   --  Return True iff object or member of it is declared to be a fully
   --  constrained subtype.
   function Is_Object_Fully_Constrained (Decl : Iir) return Boolean;
   function Is_Object_Name_Fully_Constrained (Obj : Iir) return Boolean;

   --  Return the type definition/subtype indication of NAME if NAME denotes
   --  a type or subtype name.  Otherwise, return Null_Iir;
   function Is_Type_Name (Name : Iir) return Iir;

   --  Return TRUE iff SPEC is the subprogram specification of a subprogram
   --  body which was previously declared.  In that case, the only use of SPEC
   --  is to match the body with its declaration.
   function Is_Second_Subprogram_Specification (Spec : Iir) return Boolean;

   --  Return True iif SPEC is the specification of an implicit subprogram.
   --  False for explicit subprograms.
   function Is_Implicit_Subprogram (Spec : Iir) return Boolean;
   pragma Inline (Is_Implicit_Subprogram);

   --  Return True if N is a function_declaration or an
   --  interface_function_declaration.
   function Is_Function_Declaration (N : Iir) return Boolean;
   pragma Inline (Is_Function_Declaration);

   --  Return True if N is a procedure_declaration or an
   --  interface_procedure_declaration.
   function Is_Procedure_Declaration (N : Iir) return Boolean;
   pragma Inline (Is_Procedure_Declaration);

   --  If NAME is a simple or an expanded name, return the denoted declaration.
   --  Otherwise, return NAME.
   function Strip_Denoting_Name (Name : Iir) return Iir;

   --  Build a simple name node whose named entity is REF and location LOC.
   function Build_Simple_Name (Ref : Iir; Loc : Location_Type) return Iir;
   function Build_Simple_Name (Ref : Iir; Loc : Iir) return Iir;

   --  Create a name that referenced the same named entity as NAME.
   --
   --  This is mainly used by canon, when there is a need to reference an
   --  existing name.  In some cases, it is not possible to use the name,
   --  because it is already owned.
   function Build_Reference_Name (Name : Iir) return Iir;

   --  If N is a reference_name, return the corresponding node, otherwise
   --  return N.
   function Strip_Reference_Name (N : Iir) return Iir;

   --  If SUBTYP has a resolution indication that is a function name, returns
   --  the function declaration (not the name).
   function Has_Resolution_Function (Subtyp : Iir) return Iir;

   --  Get the type of any node representing a subtype indication.  This simply
   --  skip over denoting names.
   function Get_Type_Of_Subtype_Indication (Ind : Iir) return Iir;

   --  Get the type of an index_subtype_definition or of a discrete_range from
   --  an index_constraint.
   function Get_Index_Type (Index_Type : Iir) return Iir
     renames Get_Type_Of_Subtype_Indication;

   --  Get the nature from a subnature indication.
   function Get_Nature_Of_Subnature_Indication (Ind : Iir) return Iir;

   --  Return the IDX-th index type for index subtype definition list or
   --  index_constraint INDEXES.  Return Null_Iir if IDX is out of dimension
   --  bounds, so that this function can be used to iterator over indexes of
   --  a type (or subtype).  Note that IDX starts at 0.
   function Get_Index_Type (Indexes : Iir_Flist; Idx : Natural) return Iir;

   --  Likewise but for array type or subtype ARRAY_TYPE.
   function Get_Index_Type (Array_Type : Iir; Idx : Natural) return Iir;

   --  Number of dimensions (1..n) for ARRAY_TYPE.
   function Get_Nbr_Dimensions (Array_Type : Iir) return Natural;

   --  Return True iff the all indexes of ARRAY_TYPE are locally static.
   function Are_Array_Indexes_Locally_Static (Array_Type : Iir) return Boolean;

   --  Return true if array/record bounds are locally static.  Only fully
   --  constrained records or arrays are allowed.
   --  It is possible to have non-locally static types with locally bounds (eg:
   --  a constrained array of type).
   function Are_Bounds_Locally_Static (Def : Iir) return Boolean;

   --  Return the type or subtype definition of the SUBTYP type mark.
   function Get_Denoted_Type_Mark (Subtyp : Iir) return Iir;

   --  From element declaration or element constraint EL, get the corresponding
   --  element declaration in the base record type.
   function Get_Base_Element_Declaration (El : Iir) return Iir;

   --  Append EL to the chain of owned elements of REC_TYPE.  Used only when
   --  a record_element_constraint is built.
   procedure Append_Owned_Element_Constraint (Rec_Type : Iir; El : Iir);

   --  Return true iff L and R have the same profile.
   --  L and R must be subprograms specification (or spec_body).
   function Is_Same_Profile (L, R: Iir) return Boolean;

   --  Return true iff FUNC is an operation for ATYPE.
   --
   --  LRM08 5.1 Types
   --  The set of operations of a type includes the explicitly declared
   --  subprograms that have a parameter or result of the type. The remaining
   --  operations of a type are the base operations and the predefined
   --  operations.
   function Is_Operation_For_Type (Subprg : Iir; Atype : Iir) return Boolean;

   --  From a block_specification, returns the block.
   --  Roughly speaking, this get prefix of indexed and sliced name.
   function Get_Block_From_Block_Specification (Block_Spec : Iir)
     return Iir;

   --  Wrapper around Get_Entity_Name: return the entity declaration of the
   --  entity name of DECL, or Null_Iir in case of error.
   function Get_Entity (Decl : Iir) return Iir;

   --  Wrapper around get_Configuration_Name: return the configuration
   --  declaration of ASPECT.
   function Get_Configuration (Aspect : Iir) return Iir;

   --  Return the identifier of the entity for architecture ARCH.
   function Get_Entity_Identifier_Of_Architecture (Arch : Iir) return Name_Id;

   --  Return True is component instantiation statement INST instantiate a
   --  component.
   function Is_Component_Instantiation
     (Inst : Iir_Component_Instantiation_Statement) return Boolean;

   --  Return True is component instantiation statement INST instantiate a
   --  design entity.
   function Is_Entity_Instantiation
     (Inst : Iir_Component_Instantiation_Statement) return Boolean;

   --  Get the expression of the attribute specification corresponding to the
   --  attribute name NAME.  Meaningful only for static values.
   function Get_Attribute_Name_Expression (Name : Iir) return Iir;

   --  Return the bound type of a string type, ie the type of the (first)
   --  dimension of a one-dimensional array type.
   function Get_String_Type_Bound_Type (Sub_Type : Iir) return Iir;

   --  Return left or right limit according to the direction.
   procedure Get_Low_High_Limit (Arange : Iir_Range_Expression;
                                 Low, High : out Iir);
   function Get_Low_Limit (Arange : Iir_Range_Expression) return Iir;
   function Get_High_Limit (Arange : Iir_Range_Expression) return Iir;

   --  Return TRUE iff type/subtype definition A_TYPE is an undim array.
   function Is_One_Dimensional_Array_Type (A_Type : Iir) return Boolean;

   --  Return TRUE iff unanalyzed EXPR is a range attribute.
   function Is_Range_Attribute_Name (Expr : Iir) return Boolean;

   --  Return range_expression or a range attribute from discrete range RNG.
   function Get_Range_From_Discrete_Range (Rng : Iir) return Iir;

   --  Create an array subtype from array_type or array_subtype ARR_TYPE.
   --  All fields of the returned node are filled, except the index_list.
   --  The type_staticness is set with the type staticness of the element
   --  subtype and therefore must be updated.
   --  The type_declarator field is set to null_iir.
   function Create_Array_Subtype (Arr_Type : Iir; Loc : Location_Type)
                                 return Iir_Array_Subtype_Definition;

   --  Return TRUE iff SPEC is declared inside a protected type or a protected
   --  body.
   function Is_Subprogram_Method (Spec : Iir) return Boolean;

   --  Return the protected type for method SPEC.
   function Get_Method_Type (Spec : Iir) return Iir;

   --  For Association_Element_By_Expression: return the actual.
   --  For Association_Element_Open: return the default value of the
   --    interface INTER.
   function Get_Actual_Or_Default (Assoc : Iir; Inter : Iir) return Iir;

   --  Create an error node for node ORIG.
   function Create_Error (Orig : Iir) return Iir;

   --  Create an error node for node ORIG, and set its type to ATYPE.
   --  Set its staticness to locally.
   function Create_Error_Expr (Orig : Iir; Atype : Iir) return Iir;

   --  Create an error node for node ORIG, which is supposed to be a type.
   function Create_Error_Type (Orig : Iir) return Iir;

   --  Create an error node for a name.
   function Create_Error_Name (Orig : Iir) return Iir;

   --  Extract the entity from ASPECT.
   --  Note: if ASPECT is a component declaration, returns ASPECT.
   --        if ASPECT is open, return Null_Iir;
   function Get_Entity_From_Entity_Aspect (Aspect : Iir) return Iir;

   --  Return the corresponding entity declaration from top-level configuration
   --  design unit CONFIG.
   function Get_Entity_From_Configuration (Config : Iir) return Iir;

   --  Definition from LRM08 4.8 Package bodies
   --  True if PKG (a package declaration or a package body) is not a library
   --  unit.  Can be true only for vhdl08.
   function Is_Nested_Package (Pkg : Iir) return Boolean;

   --  Definitions from LRM08 4.7 Package declarations.
   --  PKG must denote a package declaration.
   function Is_Simple_Package (Pkg : Iir) return Boolean;
   function Is_Uninstantiated_Package (Pkg : Iir) return Boolean;
   function Is_Generic_Mapped_Package (Pkg : Iir) return Boolean;

   function Is_Uninstantiated_Subprogram (Subprg : Iir) return Boolean;

   --  Return TRUE if the base name of NAME is a signal object.
   function Is_Signal_Object (Name: Iir) return Boolean;

   --  Return True IFF kind of N is K1 or K2.
   function Kind_In (N : Iir; K1, K2 : Iir_Kind) return Boolean;
   function Kind_In (N : Iir; K1, K2, K3 : Iir_Kind) return Boolean;
   pragma Inline (Kind_In);

   subtype Parameter_Index is Natural range 1 .. 4;

   --  Get/Set attribute parameter by index (for AMS attributes).
   procedure Set_Attribute_Parameter
     (Attr : Iir; N : Parameter_Index; Param : Iir);
   function Get_Attribute_Parameter
     (Attr : Iir; N : Parameter_Index) return Iir;

   --  Return the expected signature length that will be used by
   --  Get_File_Signature.
   function Get_File_Signature_Length (Def : Iir) return Natural;

   --  Store in RES the file signature for type DEF.
   --  Set the length of the buffer to OFF.
   --  Parameters are 'in out' as they are updated, so you should call this
   --  procedure with OFF = RES'First.
   procedure Get_File_Signature (Def : Iir;
                                 Res : in out String;
                                 Off : in out Natural);

   --  Like Get_Identifier but return a Name_Id for the same casing as it
   --  appears in the source file.
   --  Not useful for analysis as VHDL is case insensitive, but could be
   --  useful for error messages or tooling.
   function Get_Source_Identifier (Decl : Iir) return Name_Id;

   --  IIR wrapper around Get_HDL_Node/Set_HDL_Node.
   function Get_HDL_Node (N : PSL_Node) return Iir;
   procedure Set_HDL_Node (N : PSL_Node; Expr : Iir);
end Vhdl.Utils;
