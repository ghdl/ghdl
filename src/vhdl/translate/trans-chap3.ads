--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

package Trans.Chap3 is
   --  Translate the subtype of an object, since an object can define
   --  a subtype.
   --  This can be done only for a declaration.
   --  DECL must have an identifier and a type.
   procedure Translate_Object_Subtype
     (Decl : Iir; With_Vars : Boolean := True);
   procedure Elab_Object_Subtype (Def : Iir);

   --  Translate the subtype of a literal.
   --  This can be done not at declaration time, ie no variables are created
   --  for this subtype.
   --procedure Translate_Literal_Subtype (Def : Iir);

   --  Translation of a type definition or subtype indication.
   --  1. Create corresponding Ortho type.
   --  2. Create bounds type
   --  3. Create bounds declaration
   --  4. Create bounds constructor
   --  5. Create type descriptor declaration
   --  6. Create type descriptor constructor
   procedure Translate_Type_Definition
     (Def : Iir; With_Vars : Boolean := True);

   procedure Translate_Named_Type_Definition (Def : Iir; Id : Name_Id);
   procedure Translate_Anonymous_Type_Definition (Def : Iir);

   --  Translate subprograms for types.
   procedure Translate_Type_Subprograms (Decl : Iir);

   procedure Create_Type_Definition_Type_Range (Def : Iir);
   function Create_Static_Array_Subtype_Bounds
     (Def : Iir_Array_Subtype_Definition) return O_Cnode;

   --  Same as Translate_type_definition only for std.standard.boolean and
   --  std.standard.bit.
   procedure Translate_Bool_Type_Definition (Def : Iir);

   --  Call lock or unlock on a protected object.
   procedure Call_Ghdl_Protected_Procedure (Type_Def : Iir; Proc : O_Dnode);

   procedure Translate_Protected_Type_Body (Bod : Iir);
   procedure Translate_Protected_Type_Body_Subprograms (Bod : Iir);

   --  Translate_type_definition_Elab do 4 and 6.
   --  It generates code to do type elaboration.
   procedure Elab_Type_Declaration (Decl : Iir);
   procedure Elab_Subtype_Declaration (Decl : Iir_Subtype_Declaration);

   --  Builders.
   --  A complex type is a type whose size is not locally static.
   --
   --  The most simple example is an unidimensionnl array whose range
   --  depends on generics.
   --
   --  We call first order complex type any array whose bounds are not
   --  locally static and whose sub-element size is locally static.
   --
   --  First order complex type objects are represented by a pointer to an
   --  array of sub-element, and the storage area for the array is
   --  allocated at run-time.
   --
   --  Since a sub-element type may be a complex type, a type may be
   --  complex because one of its sub-element type is complex.
   --  EG, a record type whose one element is a complex array.
   --
   --  A type may be complex either because it is a first order complex
   --  type (ie an array whose bounds are not locally static) or because
   --  one of its sub-element type is such a type (this is recursive).
   --
   --  We call second order complex type a complex type that is not of first
   --  order.
   --  We call third order complex type a second order complex type which is
   --  an array whose bounds are not locally static.
   --
   --  In a complex type, sub-element of first order complex type are
   --  represented by a pointer.
   --  Any complex type object (constant, signal, variable, port, generic)
   --  is represented by a pointer.
   --
   --  Creation of a second or third order complex type object consists in
   --  allocating the memory and building the object.
   --  Building a object consists in setting internal pointers.
   --
   --  A complex type has always a non-null INFO.C, and its size is computed
   --  during elaboration.
   --
   --  For a second or third order complex type, INFO.C.BUILDER_NEED_FUNC
   --  is set to TRUE.

   --  Call builder for variable pointed VAR of type VAR_TYPE.
   procedure Gen_Call_Type_Builder (Var : Mnode; Var_Type : Iir);

   --  Functions for fat array.
   --  Fat array are array whose size is not known at compilation time.
   --  This corresponds to an unconstrained array or a non locally static
   --  constrained array.
   --  A fat array is a structure containing 2 fields:
   --  * base: a pointer to the data of the array.
   --  * bounds: a pointer to a structure containing as many fields as
   --    number of dimensions; these fields are a structure describing the
   --    range of the dimension.

   --  Index array BASE of type ATYPE with INDEX.
   --  INDEX must be of type ghdl_index_type, thus no bounds checks are
   --  performed.
   function Index_Base (Base : Mnode; Atype : Iir; Index : O_Enode)
                           return Mnode;

   --  Same for for slicing.
   function Slice_Base (Base : Mnode; Atype : Iir; Index : O_Enode)
                           return Mnode;

   --  Get the length of the array (the number of elements).
   function Get_Array_Length (Arr : Mnode; Atype : Iir) return O_Enode;

   --  Get the number of elements for bounds BOUNDS.  BOUNDS are
   --  automatically stabilized if necessary.
   function Get_Bounds_Length (Bounds : Mnode; Atype : Iir) return O_Enode;

   --  Get the number of elements in array ATYPE.
   function Get_Array_Type_Length (Atype : Iir) return O_Enode;

   --  Get the base of array ARR.
   function Get_Array_Base (Arr : Mnode) return Mnode;

   --  Get the bounds of array ARR.
   function Get_Array_Bounds (Arr : Mnode) return Mnode;

   --  Get the range ot ATYPE.
   function Type_To_Range (Atype : Iir) return Mnode;

   --  Get length of range R.
   function Range_To_Length (R : Mnode) return Mnode;

   --  Get direction of range R.
   function Range_To_Dir (R : Mnode) return Mnode;

   --  Get left/right bounds for range R.
   function Range_To_Left (R : Mnode) return Mnode;
   function Range_To_Right (R : Mnode) return Mnode;

   --  Get range for dimension DIM (1 based) of array bounds B or type
   --  ATYPE.
   function Bounds_To_Range (B : Mnode; Atype : Iir; Dim : Positive)
                                return Mnode;

   --  Get the range of dimension DIM (1 based) of array ARR of type ATYPE.
   function Get_Array_Range (Arr : Mnode; Atype : Iir; Dim : Positive)
                                return Mnode;

   --  Get array bounds for type ATYPE.
   function Get_Array_Type_Bounds (Atype : Iir) return Mnode;

   --  Return a pointer to the base from bounds_acc ACC.
   function Get_Bounds_Acc_Base
     (Acc : O_Enode; D_Type : Iir) return O_Enode;

   --  Deallocate OBJ.
   procedure Gen_Deallocate (Obj : O_Enode);

   --  Performs deallocation of PARAM (the parameter of a deallocate call).
   procedure Translate_Object_Deallocation (Param : Iir);

   --  Copy bounds from SRC to DEST.
   procedure Copy_Bounds (Dest : O_Enode; Src : O_Enode; Obj_Type : Iir);
   procedure Copy_Bounds (Dest : Mnode; Src : Mnode; Obj_Type : Iir);

   --  Allocate an object of type OBJ_TYPE and set RES.
   --  RES must be a stable access of type ortho_ptr_type.
   --  For an unconstrained array, BOUNDS is a pointer to the boundaries of
   --  the object, which are copied.
   procedure Translate_Object_Allocation
     (Res        : in out Mnode;
      Alloc_Kind : Allocation_Kind;
      Obj_Type   : Iir;
      Bounds     : Mnode);

   --  Low level copy of SRC to DEST.  Both have the same type, OBJ_TYPE.
   --  There is no length check, so arrays must be of the same length.
   procedure Translate_Object_Copy
     (Dest : Mnode; Src : O_Enode; Obj_Type : Iir);

   --  Get size (in bytes with type ghdl_index_type) of subtype ATYPE.
   --  For an unconstrained array, BOUNDS must be set, otherwise it may be a
   --  null_mnode.
   function Get_Subtype_Size
     (Atype : Iir; Bounds : Mnode; Kind : Object_Kind_Type) return O_Enode;

   --  Get size (in bytes with type ghdl_index_type) of object OBJ.
   --  For an unconstrained array, OBJ must be really an object, otherwise,
   --  it may be the result of T2M.
   function Get_Object_Size (Obj : Mnode; Obj_Type : Iir) return O_Enode;

   --  If needed call the procedure to build OBJ.
   procedure Maybe_Call_Type_Builder (Obj : Mnode; Obj_Type : Iir);

   --  Allocate the base of a fat array, whose length is determined from
   --  the bounds.
   --  RES_PTR is a pointer to the fat pointer (must be a variable that
   --  can be referenced several times).
   --  ARR_TYPE is the type of the array.
   procedure Allocate_Fat_Array_Base (Alloc_Kind : Allocation_Kind;
                                      Res        : Mnode;
                                      Arr_Type   : Iir);

   --  Create the bounds for SUB_TYPE.
   --  SUB_TYPE is expected to be a non-static, anonymous array type.
   procedure Create_Array_Subtype (Sub_Type : Iir);

   --  Return TRUE if VALUE is not is the range specified by ATYPE.
   --  VALUE must be stable.
   function Not_In_Range (Value : O_Dnode; Atype : Iir) return O_Enode;

   --  Return TRUE if base type of ATYPE is larger than its bounds, ie
   --  if a value of type ATYPE may be out of range.
   function Need_Range_Check (Expr : Iir; Atype : Iir) return Boolean;

   --  Generate an error if VALUE (computed from EXPR which may be NULL_IIR
   --  if not from a tree) is not in range specified by ATYPE.
   procedure Check_Range
     (Value : O_Dnode; Expr : Iir; Atype : Iir; Loc : Iir);

   --  Insert a scalar check for VALUE of type ATYPE.  EXPR may be NULL_IIR.
   function Insert_Scalar_Check
     (Value : O_Enode; Expr : Iir; Atype : Iir; Loc : Iir) return O_Enode;

   --  The base type of EXPR and the base type of ATYPE must be the same.
   --  If the type is a scalar type, and if a range check is needed, this
   --  function inserts the check.  Otherwise, it returns VALUE.
   function Maybe_Insert_Scalar_Check
     (Value : O_Enode; Expr : Iir; Atype : Iir) return O_Enode;

   --  Return True iff all indexes of L_TYPE and R_TYPE have the same
   --  length.  They must be locally static.
   function Locally_Array_Match (L_Type, R_Type : Iir) return Boolean;

   --  Check bounds length of L match bounds length of R.
   --  If L_TYPE (resp. R_TYPE) is not a thin array, then L_NODE
   --    (resp. R_NODE) are not used (and may be Mnode_Null).
   --  If L_TYPE (resp. T_TYPE) is a fat array, then L_NODE (resp. R_NODE)
   --    must designate the array.
   procedure Check_Array_Match
     (L_Type : Iir; L_Node : Mnode; R_Type : Iir; R_Node : Mnode; Loc : Iir);

   --  Create a subtype range to be stored into RES from length LENGTH, which
   --  is of type INDEX_TYPE.
   --  This is done according to rules 7.2.4 of LRM93, ie:
   --  direction and left bound of the range is the same of INDEX_TYPE.
   --  LENGTH is a variable. LOC is the location in case of error.
   procedure Create_Range_From_Length
     (Index_Type : Iir; Length : O_Dnode; Res : Mnode; Loc : Iir);

end Trans.Chap3;
