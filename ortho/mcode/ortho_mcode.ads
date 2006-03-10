--  Mcode back-end for ortho.
--  Copyright (C) 2006 Tristan Gingold
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
with Interfaces; use Interfaces;
with Ortho_Code; use Ortho_Code;
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.Consts; use Ortho_Code.Consts;
with Ortho_Code.Decls; use Ortho_Code.Decls;
with Ortho_Code.Exprs; use Ortho_Code.Exprs;
with Ortho_Code.Abi;

--  Interface to create nodes.
package Ortho_Mcode is
   --- PUBLIC DECLARATIONS
   subtype O_Tnode is Ortho_Code.O_Tnode;
   subtype O_Cnode is Ortho_Code.O_Cnode;
   subtype O_Dnode is Ortho_Code.O_Dnode;
   subtype O_Enode is Ortho_Code.O_Enode;
   subtype O_Fnode is Ortho_Code.O_Fnode;
   subtype O_Lnode is Ortho_Code.O_Lnode;
   subtype O_Snode is Ortho_Code.Exprs.O_Snode;

   O_Lnode_Null : constant O_Lnode := Ortho_Code.O_Lnode_Null;
   O_Cnode_Null : constant O_Cnode := Ortho_Code.O_Cnode_Null;
   O_Dnode_Null : constant O_Dnode := Ortho_Code.O_Dnode_Null;
   O_Enode_Null : constant O_Enode := Ortho_Code.O_Enode_Null;
   O_Fnode_Null : constant O_Fnode := Ortho_Code.O_Fnode_Null;
   O_Snode_Null : O_Snode renames Ortho_Code.Exprs.O_Snode_Null;
   O_Tnode_Null : constant O_Tnode := Ortho_Code.O_Tnode_Null;
   function "=" (L, R : O_Tnode) return Boolean renames Ortho_Code."=";
   function "=" (L, R : O_Cnode) return Boolean renames Ortho_Code."=";
   function "=" (L, R : O_Snode) return Boolean renames Ortho_Code.Exprs."=";
   function "=" (L, R : O_Dnode) return Boolean renames Ortho_Code."=";
   function "=" (L, R : O_Enode) return Boolean renames Ortho_Code."=";
   function "=" (L, R : O_Fnode) return Boolean renames Ortho_Code."=";
   function "=" (L, R : O_Lnode) return Boolean renames Ortho_Code."=";

   --  Initialize nodes.
   procedure Init;
   procedure Finish;

   procedure Free_All;

   subtype O_Element_List is Ortho_Code.Types.O_Element_List;

   --  Build a record type.
   procedure Start_Record_Type (Elements : out O_Element_List)
     renames Ortho_Code.Types.Start_Record_Type;

   --  Add a field in the record; not constrained array are prohibited, since
   --  its size is unlimited.
   procedure New_Record_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode)
     renames Ortho_Code.Types.New_Record_Field;

   --  Finish the record type.
   procedure Finish_Record_Type
     (Elements : in out O_Element_List; Res : out O_Tnode)
     renames Ortho_Code.Types.Finish_Record_Type;

   -- Build an uncomplete record type:
   -- First call NEW_UNCOMPLETE_RECORD_TYPE, which returns a record type.
   -- This type can be declared or used to define access types on it.
   -- Then, complete (if necessary) the record type, by calling
   -- START_UNCOMPLETE_RECORD_TYPE, NEW_RECORD_FIELD and FINISH_RECORD_TYPE.
   procedure New_Uncomplete_Record_Type (Res : out O_Tnode)
     renames Ortho_Code.Types.New_Uncomplete_Record_Type;
   procedure Start_Uncomplete_Record_Type (Res : O_Tnode;
                                           Elements : out O_Element_List)
     renames Ortho_Code.Types.Start_Uncomplete_Record_Type;

   --  Build an union type.
   procedure Start_Union_Type (Elements : out O_Element_List)
     renames Ortho_Code.Types.Start_Union_Type;
   procedure New_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident;
      Etype : O_Tnode)
     renames Ortho_Code.Types.New_Union_Field;
   procedure Finish_Union_Type
     (Elements : in out O_Element_List; Res : out O_Tnode)
     renames Ortho_Code.Types.Finish_Union_Type;

   --  Build an access type.
   --  DTYPE may be O_tnode_null in order to build an incomplete access type.
   --  It is completed with finish_access_type.
   function New_Access_Type (Dtype : O_Tnode) return O_Tnode
     renames Ortho_Code.Types.New_Access_Type;
   procedure Finish_Access_Type (Atype : O_Tnode; Dtype : O_Tnode)
     renames Ortho_Code.Types.Finish_Access_Type;

   --  Build an array type.
   --  The array is not constrained and unidimensional.
   function New_Array_Type (El_Type : O_Tnode; Index_Type : O_Tnode)
                           return O_Tnode
     renames Ortho_Code.Types.New_Array_Type;

   --  Build a constrained array type.
   function New_Constrained_Array_Type (Atype : O_Tnode; Length : O_Cnode)
                                       return O_Tnode;

   --  Build a scalar type; size may be 8, 16, 32 or 64.
   function New_Unsigned_Type (Size : Natural) return O_Tnode
     renames Ortho_Code.Types.New_Unsigned_Type;
   function New_Signed_Type (Size : Natural) return O_Tnode
     renames Ortho_Code.Types.New_Signed_Type;

   --  Build a float type.
   function New_Float_Type return O_Tnode
     renames Ortho_Code.Types.New_Float_Type;

   --  Build a boolean type.
   procedure New_Boolean_Type (Res : out O_Tnode;
                               False_Id : O_Ident;
                               False_E : out O_Cnode;
                               True_Id : O_Ident;
                               True_E : out O_Cnode)
     renames Ortho_Code.Types.New_Boolean_Type;

   --  Create an enumeration
   subtype O_Enum_List is Ortho_Code.Types.O_Enum_List;

   --  Elements are declared in order, the first is ordered from 0.
   procedure Start_Enum_Type (List : out O_Enum_List; Size : Natural)
     renames Ortho_Code.Types.Start_Enum_Type;
   procedure New_Enum_Literal (List : in out O_Enum_List;
                               Ident : O_Ident; Res : out O_Cnode)
     renames Ortho_Code.Types.New_Enum_Literal;
   procedure Finish_Enum_Type (List : in out O_Enum_List; Res : out O_Tnode)
     renames Ortho_Code.Types.Finish_Enum_Type;

   -------------------
   --  Expressions  --
   -------------------

   subtype ON_Op_Kind is Ortho_Code.ON_Op_Kind;
   function "=" (L, R : ON_Op_Kind) return Boolean renames Ortho_Code."=";

   ON_Nil : constant ON_Op_Kind := Ortho_Code.ON_Nil;

   ON_Add_Ov : constant ON_Op_Kind := Ortho_Code.ON_Add_Ov;
   ON_Sub_Ov : constant ON_Op_Kind := Ortho_Code.ON_Sub_Ov;
   ON_Mul_Ov : constant ON_Op_Kind := Ortho_Code.ON_Mul_Ov;
   ON_Div_Ov : constant ON_Op_Kind := Ortho_Code.ON_Div_Ov;
   ON_Rem_Ov : constant ON_Op_Kind := Ortho_Code.ON_Rem_Ov;
   ON_Mod_Ov : constant ON_Op_Kind := Ortho_Code.ON_Mod_Ov;

   ON_And : constant ON_Op_Kind := Ortho_Code.ON_And;
   ON_Or : constant ON_Op_Kind := Ortho_Code.ON_Or;
   ON_Xor : constant ON_Op_Kind := Ortho_Code.ON_Xor;

   --  Monadic operations.
   ON_Not : constant ON_Op_Kind := Ortho_Code.ON_Not;
   ON_Neg_Ov : constant ON_Op_Kind := Ortho_Code.ON_Neg_Ov;
   ON_Abs_Ov : constant ON_Op_Kind := Ortho_Code.ON_Abs_Ov;

   --  Comparaisons
   ON_Eq : constant ON_Op_Kind := Ortho_Code.ON_Eq;
   ON_Neq : constant ON_Op_Kind := Ortho_Code.ON_Neq;
   ON_Le : constant ON_Op_Kind := Ortho_Code.ON_Le;
   ON_Lt : constant ON_Op_Kind := Ortho_Code.ON_Lt;
   ON_Ge : constant ON_Op_Kind := Ortho_Code.ON_Ge;
   ON_Gt : constant ON_Op_Kind := Ortho_Code.ON_Gt;

   subtype ON_Dyadic_Op_Kind is ON_Op_Kind range ON_Add_Ov .. ON_Xor;
   subtype ON_Monadic_Op_Kind is ON_Op_Kind range ON_Not .. ON_Abs_Ov;
   subtype ON_Compare_Op_Kind is ON_Op_Kind range ON_Eq .. ON_Gt;

   subtype O_Storage is Ortho_Code.O_Storage;
   O_Storage_Private : constant O_Storage := Ortho_Code.O_Storage_Private;
   O_Storage_Local : constant O_Storage := Ortho_Code.O_Storage_Local;
   O_Storage_Public : constant O_Storage := Ortho_Code.O_Storage_Public;
   O_Storage_External : constant O_Storage := Ortho_Code.O_Storage_External;
   function "=" (L, R : O_Storage) return Boolean renames Ortho_Code."=";

   Type_Error : exception;
   Syntax_Error : exception;

   function New_Lit (Lit : O_Cnode) return O_Enode
     renames Ortho_Code.Exprs.New_Lit;

   --  Create a dyadic operation.
   --  Left and right nodes must have the same type.
   --  Binary operation is allowed only on boolean types.
   --  The result is of the type of the operands.
   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
                          return O_Enode
     renames Ortho_Code.Exprs.New_Dyadic_Op;

   --  Create a monadic operation.
   --  Result is of the type of operand.
   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
                           return O_Enode
     renames Ortho_Code.Exprs.New_Monadic_Op;

   --  Create a comparaison operator.
   --  NTYPE is the type of the result and must be a boolean type.
   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind; Left, Right : O_Enode; Ntype : O_Tnode)
     return O_Enode
     renames Ortho_Code.Exprs.New_Compare_Op;

   --  Create a literal from an integer.
   function New_Signed_Literal (Ltype : O_Tnode; Value : Integer_64)
                               return O_Cnode
     renames Ortho_Code.Consts.New_Signed_Literal;
   function New_Unsigned_Literal (Ltype : O_Tnode; Value : Unsigned_64)
                                 return O_Cnode
     renames Ortho_Code.Consts.New_Unsigned_Literal;
   function New_Float_Literal (Ltype : O_Tnode; Value : IEEE_Float_64)
                              return O_Cnode
     renames Ortho_Code.Consts.New_Float_Literal;

   --  Create a null access literal.
   function New_Null_Access (Ltype : O_Tnode) return O_Cnode
     renames Ortho_Code.Consts.New_Null_Access;

   subtype O_Inter_List is Ortho_Code.Decls.O_Inter_List;
   subtype O_Record_Aggr_List is Ortho_Code.Consts.O_Record_Aggr_List;
   subtype O_Array_Aggr_List is Ortho_Code.Consts.O_Array_Aggr_List;
   subtype O_Assoc_List is Ortho_Code.Exprs.O_Assoc_List;
   subtype O_If_Block is Ortho_Code.Exprs.O_If_Block;
   subtype O_Case_Block is Ortho_Code.Exprs.O_Case_Block;


   --  Build a record/array aggregate.
   --  The aggregate is constant, and therefore can be only used to initialize
   --  constant declaration.
   --  ATYPE must be either a record type or an array subtype.
   --  Elements must be added in the order, and must be literals or aggregates.
   procedure Start_Record_Aggr (List : out O_Record_Aggr_List;
                                Atype : O_Tnode)
     renames Ortho_Code.Consts.Start_Record_Aggr;
   procedure New_Record_Aggr_El (List : in out O_Record_Aggr_List;
                                 Value : O_Cnode)
     renames Ortho_Code.Consts.New_Record_Aggr_El;
   procedure Finish_Record_Aggr (List : in out O_Record_Aggr_List;
                                 Res : out O_Cnode)
     renames Ortho_Code.Consts.Finish_Record_Aggr;

   procedure Start_Array_Aggr (List : out O_Array_Aggr_List; Atype : O_Tnode)
     renames Ortho_Code.Consts.Start_Array_Aggr;
   procedure New_Array_Aggr_El (List : in out O_Array_Aggr_List;
                                Value : O_Cnode)
     renames Ortho_Code.Consts.New_Array_Aggr_El;
   procedure Finish_Array_Aggr (List : in out O_Array_Aggr_List;
                                Res : out O_Cnode)
     renames Ortho_Code.Consts.Finish_Array_Aggr;

   --  Build an union aggregate.
   function New_Union_Aggr (Atype : O_Tnode; Field : O_Fnode; Value : O_Cnode)
                           return O_Cnode
     renames Ortho_Code.Consts.New_Union_Aggr;

   --  Returns the size in bytes of ATYPE.  The result is a literal of
   --  unsigned type RTYPE
   --  ATYPE cannot be an unconstrained array type.
   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
     renames Ortho_Code.Consts.New_Sizeof;

   --  Returns the offset of FIELD in its record.  The result is a literal
   --  of unsigned type RTYPE.
   function New_Offsetof (Field : O_Fnode; Rtype : O_Tnode) return O_Cnode
     renames Ortho_Code.Consts.New_Offsetof;

   --  Get an element of an array.
   --  INDEX must be of the type of the array index.
   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode)
                                return O_Lnode
     renames Ortho_Code.Exprs.New_Indexed_Element;

   --  Get a slice of an array; this is equivalent to a conversion between
   --  an array or an array subtype and an array subtype.
   --  RES_TYPE must be an array_sub_type whose base type is the same as the
   --  base type of ARR.
   --  INDEX must be of the type of the array index.
   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
                      return O_Lnode
     renames Ortho_Code.Exprs.New_Slice;

   --  Get an element of a record.
   --  Type of REC must be a record type.
   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
                                 return O_Lnode
     renames Ortho_Code.Exprs.New_Selected_Element;

   --  Reference an access.
   --  Type of ACC must be an access type.
   function New_Access_Element (Acc : O_Enode) return O_Lnode
     renames Ortho_Code.Exprs.New_Access_Element;

   --  Do a conversion.
   --  Allowed conversions are:
   --  FIXME: to write.
   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode
     renames Ortho_Code.Exprs.New_Convert_Ov;

   --  Get the address of LVALUE.
   --  ATYPE must be a type access whose designated type is the type of LVALUE.
   --  FIXME: what about arrays.
   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode) return O_Enode
     renames Ortho_Code.Exprs.New_Address;
   function New_Global_Address (Decl : O_Dnode; Atype : O_Tnode)
                               return O_Cnode
     renames Ortho_Code.Consts.New_Global_Address;

   --  Same as New_Address but without any restriction.
   function New_Unchecked_Address (Lvalue : O_Lnode; Atype : O_Tnode)
                                  return O_Enode
     renames Ortho_Code.Exprs.New_Unchecked_Address;
   function New_Global_Unchecked_Address (Decl : O_Dnode; Atype : O_Tnode)
                                         return O_Cnode
     renames Ortho_Code.Consts.New_Global_Unchecked_Address;

   --  Get the address of a subprogram.
   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
                                   return O_Cnode
     renames Ortho_Code.Consts.New_Subprogram_Address;

   --  Get the value of an Lvalue.
   function New_Value (Lvalue : O_Lnode) return O_Enode
     renames Ortho_Code.Exprs.New_Value;

   --  Get the value of object OBJ.
   function New_Obj_Value (Obj : O_Dnode) return O_Enode;

   --  Return a pointer of type RTPE to SIZE bytes allocated on the stack.
   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode
     renames Ortho_Code.Exprs.New_Alloca;

   ---------------------
   --  Declarations.  --
   ---------------------

   --  Following lines applies to FILENAME.
   procedure New_Debug_Filename_Decl (Filename : String)
     renames Ortho_Code.Abi.New_Debug_Filename_Decl;

   --  Line number of the next declaration.
   procedure New_Debug_Line_Decl (Line : Natural);

   --  Add a comment in the declarative region.
   procedure New_Debug_Comment_Decl (Comment : String);

   --  Declare a type.
   --  This simply gives a name to a type.
   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode)
     renames Ortho_Code.Decls.New_Type_Decl;

   --  Declare a constant.
   --  This simply gives a name to a constant value or aggregate.
   --  A constant cannot be modified and its storage cannot be local.
   --  ATYPE must be constrained.
   procedure New_Const_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
     renames Ortho_Code.Decls.New_Const_Decl;

   --  Set the value of a non-external constant.
   procedure Start_Const_Value (Const : in out O_Dnode);
   procedure Finish_Const_Value (Const : in out O_Dnode; Val : O_Cnode);

   --  Create a variable declaration.
   --  A variable can be local only inside a function.
   --  ATYPE must be constrained.
   procedure New_Var_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
     renames Ortho_Code.Decls.New_Var_Decl;

   function New_Obj (Decl : O_Dnode) return O_Lnode
     renames Ortho_Code.Exprs.New_Obj;

   --  Start a subprogram declaration.
   --  Note: nested subprograms are allowed, ie o_storage_local subprograms can
   --   be declared inside a subprograms.  It is not allowed to declare
   --   o_storage_external subprograms inside a subprograms.
   --  Return type and interfaces cannot be a composite type.
   procedure Start_Function_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode)
     renames Ortho_Code.Decls.Start_Function_Decl;
   --  For a subprogram without return value.
   procedure Start_Procedure_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage)
     renames Ortho_Code.Decls.Start_Procedure_Decl;

   --  Add an interface declaration to INTERFACES.
   procedure New_Interface_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode;
      Ident : O_Ident;
      Atype : O_Tnode)
     renames Ortho_Code.Decls.New_Interface_Decl;
   --  Finish the function declaration, get the node and a statement list.
   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List; Res : out O_Dnode)
     renames Ortho_Code.Decls.Finish_Subprogram_Decl;
   --  Start a subprogram body.
   --  Note: the declaration may have an external storage, in this case it
   --  becomes public.
   procedure Start_Subprogram_Body (Func : O_Dnode)
     renames Ortho_Code.Exprs.Start_Subprogram_Body;
   --  Finish a subprogram body.
   procedure Finish_Subprogram_Body
     renames Ortho_Code.Exprs.Finish_Subprogram_Body;


   -------------------
   --  Statements.  --
   -------------------

   --  Add a line number as a statement.
   procedure New_Debug_Line_Stmt (Line : Natural)
     renames Ortho_Code.Exprs.New_Debug_Line_Stmt;

   --  Add a comment as a statement.
   procedure New_Debug_Comment_Stmt (Comment : String);

   --  Start a declarative region.
   procedure Start_Declare_Stmt;
   procedure Finish_Declare_Stmt;

   --  Create a function call or a procedure call.
   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode)
     renames Ortho_Code.Exprs.Start_Association;
   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode)
     renames Ortho_Code.Exprs.New_Association;
   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode
     renames Ortho_Code.Exprs.New_Function_Call;
   procedure New_Procedure_Call (Assocs : in out O_Assoc_List)
     renames Ortho_Code.Exprs.New_Procedure_Call;

   --  Assign VALUE to TARGET, type must be the same or compatible.
   --  FIXME: what about slice assignment?
   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode)
     renames Ortho_Code.Exprs.New_Assign_Stmt;

   --  Exit from the subprogram and return VALUE.
   procedure New_Return_Stmt (Value : O_Enode)
     renames Ortho_Code.Exprs.New_Return_Stmt;
   --  Exit from the subprogram, which doesn't return value.
   procedure New_Return_Stmt
     renames Ortho_Code.Exprs.New_Return_Stmt;

   --  Build an IF statement.
   procedure Start_If_Stmt (Block : out O_If_Block; Cond : O_Enode)
     renames Ortho_Code.Exprs.Start_If_Stmt;
   --  COND is NULL for the final else statement.
   procedure New_Elsif_Stmt (Block : in out O_If_Block; Cond : O_Enode)
     renames Ortho_Code.Exprs.New_Elsif_Stmt;
   procedure New_Else_Stmt (Block : in out O_If_Block)
     renames Ortho_Code.Exprs.New_Else_Stmt;
   procedure Finish_If_Stmt (Block : in out O_If_Block)
     renames Ortho_Code.Exprs.Finish_If_Stmt;

   --  Create a infinite loop statement.
   procedure Start_Loop_Stmt (Label : out O_Snode)
     renames Ortho_Code.Exprs.Start_Loop_Stmt;
   procedure Finish_Loop_Stmt (Label : in out O_Snode)
     renames Ortho_Code.Exprs.Finish_Loop_Stmt;

   --  Exit from a loop stmt or from a for stmt.
   procedure New_Exit_Stmt (L : O_Snode)
     renames Ortho_Code.Exprs.New_Exit_Stmt;
   --  Go to the start of a loop stmt or of a for stmt.
   --  Loops/Fors between L and the current points are exited.
   procedure New_Next_Stmt (L : O_Snode)
     renames Ortho_Code.Exprs.New_Next_Stmt;

   --  Case statement.
   --  VALUE is the selector and must be a discrete type.
   procedure Start_Case_Stmt (Block : out O_Case_Block; Value : O_Enode)
     renames Ortho_Code.Exprs.Start_Case_Stmt;

   --  Start a branch before the choices.
   procedure Start_Choice (Block : in out O_Case_Block)
     renames Ortho_Code.Exprs.Start_Choice;
   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode)
     renames Ortho_Code.Exprs.New_Expr_Choice;
   procedure New_Range_Choice (Block : in out O_Case_Block;
                               Low, High : O_Cnode)
     renames Ortho_Code.Exprs.New_Range_Choice;
   procedure New_Default_Choice (Block : in out O_Case_Block)
     renames Ortho_Code.Exprs.New_Default_Choice;
   --  Finish a branch after a choice, allow regular statements.
   procedure Finish_Choice (Block : in out O_Case_Block)
     renames Ortho_Code.Exprs.Finish_Choice;
   procedure Finish_Case_Stmt (Block : in out O_Case_Block)
     renames Ortho_Code.Exprs.Finish_Case_Stmt;
end Ortho_Mcode;
