--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Chap3 is
   --  Translate the subtype of an object, since an object can define
   --  a subtype.
   --  This can be done only for a declaration.
   --  DECL must have an identifier and a type.
   procedure Translate_Object_Subtype_Indication
     (Decl : Iir; With_Vars : Boolean := True);
   procedure Elab_Object_Subtype_Indication (Decl : Iir);

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
   procedure Translate_Type_Definition (Def : Iir);

   --  Translate subprograms for types.
   procedure Translate_Type_Subprograms
     (Decl : Iir; Kind : Subprg_Translate_Kind);

   function Create_Static_Composite_Subtype_Layout (Def : Iir) return O_Cnode;

   --  Same as Translate_type_definition only for std.standard.boolean and
   --  std.standard.bit.
   procedure Translate_Bool_Type_Definition (Def : Iir);

   --  Call lock or unlock on a protected object.
   procedure Call_Ghdl_Protected_Procedure (Type_Def : Iir; Proc : O_Dnode);

   procedure Translate_Protected_Type_Body (Bod : Iir);
   procedure Translate_Protected_Type_Body_Subprograms_Spec (Bod : Iir);
   procedure Translate_Protected_Type_Body_Subprograms_Body (Bod : Iir);

   procedure Translate_Subtype_Definition
     (Def : Iir; With_Vars : Boolean := True);

   --  Translate a proper subtype indication.
   procedure Translate_Subtype_Indication (Def : Iir; With_Vars : Boolean);

   procedure Translate_Named_Subtype_Definition (Def : Iir; Id : Name_Id);

   --  When there is no name for the subtype (eg: the subtype of a string or
   --  of an aggregate).  There is also no type mark.
   procedure Translate_Anonymous_Subtype_Definition
     (Def : Iir; With_Vars : Boolean);

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

   --  Functions for fat array.
   --  Fat array are array whose size is not known at compilation time.
   --  This corresponds to an unconstrained array or a non locally static
   --  constrained array.
   --  A fat array is a structure containing 2 fields:
   --  * base: a pointer to the data of the array.
   --  * bounds: a pointer to a structure containing as many fields as
   --    number of dimensions; these fields are a structure describing the
   --    range of the dimension.

   procedure Gen_Call_Type_Builder
     (Layout : Mnode; Var_Type : Iir; Kind : Object_Kind_Type);

   --  If the element subtype of ARR_TYPE is unbounded, create a fat pointer,
   --  set the bounds of it (from ARR), and return it.
   --  Otherwise, return a null mnode.
   --  Used to build a var for a subelement of ARR.
   --  This is used by foreach_non_composite to factorize fat-pointer
   --  building.
   function Create_Maybe_Fat_Array_Element (Arr : Mnode; Arr_Type : Iir)
                                           return Mnode;

   --  If the element subtype of the array is unbounded, set the base of VAR
   --  from EL, and return it.
   --  Otherwise directly return EL (VAR must be null).
   function Assign_Maybe_Fat_Array_Element (Var : Mnode; El : Mnode)
                                           return Mnode;

   --  Index array BASE of type ATYPE with INDEX.
   --  INDEX must be of type ghdl_index_type, thus no bounds checks are
   --  performed.
   function Index_Base (Base : Mnode;
                        Atype : Iir;
                        Index : O_Enode;
                        Stride : O_Enode := O_Enode_Null) return Mnode;

   --  Index array ARR of type ATYPE with INDEX.
   --  Return the base.
   function Index_Array (Arr : Mnode; Atype : Iir; Index : O_Enode)
                        return Mnode;

   --  Same for for slicing.
   function Slice_Base
     (Base : Mnode; Atype : Iir; Index : O_Enode; Stride : O_Enode)
     return Mnode;

   --  Get the length of the array (the number of elements).
   function Get_Array_Length (Arr : Mnode; Atype : Iir) return O_Enode;

   --  Get the number of elements for bounds BOUNDS.  BOUNDS are
   --  automatically stabilized if necessary.
   function Get_Bounds_Length (Bounds : Mnode; Atype : Iir) return O_Enode;

   --  Return the number of elements for statically bounded array ATYPE.
   function Get_Static_Array_Length (Atype : Iir) return Int64;

   --  Get the number of elements in array ATYPE.
   function Get_Array_Type_Length (Atype : Iir) return O_Enode;

   --  Return the allocation kind used for layout variable of type INFO.
   function Get_Composite_Type_Layout_Alloc (Info : Type_Info_Acc)
                                             return Allocation_Kind;

   --  Get the base of array or record OBJ.  If OBJ is already constrained,
   --  return it.
   function Get_Composite_Base (Obj : Mnode) return Mnode;

   --  The base returned by Get_Composite_Base is always the least
   --  constrained array base.  But the subtype may be more constrained than
   --  the base.  In that case the base must be converted to the subtype.
   function Convert_Array_Base (Arr : Mnode) return Mnode;

   --  Get the base of array or record OBJ; but if OBJ is already constrained,
   --  convert it to the base of an unbounded object (so this unboxes the
   --  records).
   function Get_Composite_Unbounded_Base (Obj : Mnode) return Mnode;

   --  Get the bounds of composite ARR (an array or an unbounded record).
   function Get_Composite_Bounds (Obj : Mnode) return Mnode;

   --  Get the range ot ATYPE.
   function Type_To_Range (Atype : Iir) return Mnode;

   --  Get length of range R.
   function Range_To_Length (R : Mnode) return Mnode;

   --  Get direction of range R.
   function Range_To_Dir (R : Mnode) return Mnode;

   --  Get left/right bounds for range R.
   function Range_To_Left (R : Mnode) return Mnode;
   function Range_To_Right (R : Mnode) return Mnode;

   --  Get range for dimension DIM (1 based) of array bounds B of type
   --  ATYPE.
   function Bounds_To_Range (B : Mnode; Atype : Iir; Dim : Positive)
                                return Mnode;

   --  Get the range of dimension DIM (1 based) of array ARR of type ATYPE.
   function Get_Array_Range (Arr : Mnode; Atype : Iir; Dim : Positive)
                                return Mnode;

   --  Get array/record bounds for type ATYPE.
   function Get_Composite_Type_Bounds (Atype : Iir) return Mnode;

   --  Return a pointer to the base from bounds_acc ACC.
   function Get_Bounds_Acc_Base
     (Acc : O_Enode; D_Type : Iir) return O_Enode;

   --  Return bounds from layout B.
   function Layout_To_Bounds (B : Mnode) return Mnode;

   function Layout_To_Size (Layout : Mnode; Kind : Object_Kind_Type)
                           return O_Lnode;

   --  From a record layout B, return the layout of element EL.  EL must be
   --  an unbounded element.
   function Record_Layout_To_Element_Layout (B : Mnode; El : Iir) return Mnode;

   --  From an unbounded record bounds B, get the bounds for (unbounded)
   --  element EL.
   function Record_Bounds_To_Element_Bounds (B : Mnode; El : Iir) return Mnode;

   --  Return the offset for field EL in record B.
   function Record_Layout_To_Element_Offset
     (B : Mnode; El : Iir; Kind : Object_Kind_Type) return O_Lnode;

   --  From an unbounded array bounds B, get the bounds for the (unbounded)
   --  element.
   function Array_Bounds_To_Element_Bounds (B : Mnode; Arr_Type : Iir)
                                           return Mnode;

   --  From unbounded array bounds B, get the layout of the unbounded element.
   function Array_Bounds_To_Element_Layout (B : Mnode; Arr_Type : Iir)
                                           return Mnode;

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
     (Dest : Mnode; Src : Mnode; Obj_Type : Iir);

   --  Get size (in bytes with type ghdl_index_type) of subtype ATYPE.
   --  For an unconstrained array, BOUNDS must be set, otherwise it may be a
   --  null_mnode.
   function Get_Subtype_Size
     (Atype : Iir; Bounds : Mnode; Kind : Object_Kind_Type) return O_Enode;

   --  Get size (in bytes with type ghdl_index_type) of object OBJ.
   --  For an unconstrained array, OBJ must be really an object, otherwise,
   --  it may be the result of T2M.
   function Get_Object_Size (Obj : Mnode; Obj_Type : Iir) return O_Enode;

   --  Allocate the base of an unbounded composite, whose length is
   --  determined from the bounds (already set).
   --  RES_PTR is a pointer to the fat pointer (must be a stable variable: it
   --  can be referenced several times).
   --  ARR_TYPE is the type of the composite.
   procedure Allocate_Unbounded_Composite_Base (Alloc_Kind : Allocation_Kind;
                                                Res        : Mnode;
                                                Arr_Type   : Iir);

   --  Allocate the bounds for type OBJ_TYPE and assign it to unbounded object
   --  RES.  Then copy bounds from BOUNDS to the object.
   procedure Allocate_Unbounded_Composite_Bounds
     (Alloc_Kind : Allocation_Kind;
      Res        : Mnode;
      Obj_Type   : Iir);

   --  Used for alias: create the vars for the subtype of the name (when the
   --  name is a slice).  The identifier prefix must have been set.
   --
   --  Slices are special because they create new bounds variables.  These
   --  variables are expected to be transcient.  But in some cases (like
   --  aliases), they have a longer life.
   procedure Translate_Array_Subtype (Arr_Type : Iir);
   procedure Elab_Array_Subtype (Arr_Type : Iir);

   --  Return the element subtype to be used for getting type info.
   --  Follow the translation for array subtypes, as a type for the element is
   --  not created if the array is not also constrained with locally static
   --  indexes.
   function Get_Element_Subtype_For_Info (Arr_Def : Iir) return Iir;

   --  Create the bounds for SUB_TYPE.
   --  SUB_TYPE is expected to be a non-static, anonymous array or record
   --  subtype.
   procedure Create_Composite_Subtype (Sub_Type : Iir; Elab : Boolean := True);

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
   --  length.  They must be constrained.
   function Locally_Types_Match (L_Type : Iir; R_Type : Iir)
                                return Tri_State_Type;

   --  Check bounds of L match bounds of R.
   --  If L_TYPE (resp. R_TYPE) is not a thin composite type, then L_NODE
   --    (resp. R_NODE) are not used (and may be Mnode_Null).
   --  If L_TYPE (resp. T_TYPE) is a fat type, then L_NODE (resp. R_NODE)
   --    must designate the object.
   procedure Check_Composite_Match
     (L_Type : Iir; L_Node : Mnode; R_Type : Iir; R_Node : Mnode; Loc : Iir);

   --  Create a subtype range to be stored into RES from length LENGTH, which
   --  is of type INDEX_TYPE.
   --  This is done according to rules 7.2.4 of LRM93, ie:
   --  direction and left bound of the range is the same of INDEX_TYPE.
   --  LENGTH is a variable. LOC is the location in case of error.
   procedure Create_Range_From_Length
     (Index_Type : Iir; Length : O_Dnode; Res : Mnode; Loc : Iir);

end Trans.Chap3;
