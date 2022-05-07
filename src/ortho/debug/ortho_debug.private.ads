--  Ortho debug back-end declarations.
--  Copyright (C) 2005-2014 Tristan Gingold
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

with Interfaces; use Interfaces;
with Ortho_Ident;
use Ortho_Ident;

--  Interface to create nodes.
package Ortho_Debug is
   procedure Init;
   procedure Finish;

private
   --  This back-end supports nested subprograms.
   Has_Nested_Subprograms : constant Boolean := True;

   --  Return the type of elements of array type/subtype ATYPE.
   function Get_Array_El_Type (Atype : O_Tnode) return O_Tnode;

   --  Return the elements of a record or sub-record.
   function Get_Record_Elements (Atype : O_Tnode) return O_Fnode;

   --  Return the base type of T.
   --  function Get_Base_Type (T : O_Tnode) return O_Tnode;

   --  A node for a type.
   type O_Tnode_Type (<>);
   type O_Tnode is access O_Tnode_Type;

   --  A node for a statement.
   type O_Snode_Type (<>);
   type O_Snode is access O_Snode_Type;

   Top : O_Snode;

   type Str_Acc is access String;

   type Decl_Scope_Type;
   type Decl_Scope_Acc is access Decl_Scope_Type;

   type On_Decl_Kind is
     (ON_Type_Decl, ON_Completed_Type_Decl,
      ON_Const_Decl, ON_Var_Decl, ON_Interface_Decl,
      ON_Function_Decl, ON_Function_Body,
      ON_Init_Value,
      ON_Debug_Line_Decl, ON_Debug_Comment_Decl, ON_Debug_Filename_Decl);

   type O_Dnode_Type (<>);
   type O_Dnode is access O_Dnode_Type;

   O_Dnode_Null : constant O_Dnode := null;

   type O_Dnode_Type (Kind : On_Decl_Kind) is record
      Next : O_Dnode;
      Name : O_Ident;
      Dtype : O_Tnode;
      Storage : O_Storage;
      --  Declare statement in which the declaration appears.
      Scope : O_Snode;
      --  Line number, for regen.
      Lineno : Natural;
      case Kind is
         when ON_Type_Decl =>
            null;
         when ON_Completed_Type_Decl =>
            null;
         when ON_Const_Decl
           | ON_Var_Decl =>
            --  Corresponding declaration for initial value (if any).
            Value_Decl : O_Dnode;
         when ON_Init_Value =>
            --  Corresponding declaration of the object.
            Init_Decl : O_Dnode;
            Value : O_Cnode;
         when ON_Function_Decl =>
            Interfaces : O_Dnode;
            Func_Body : O_Dnode;
            Alive : Boolean;
         when ON_Function_Body =>
            Func_Decl : O_Dnode;
            Func_Stmt : O_Snode;
         when ON_Interface_Decl =>
            Func_Scope : O_Dnode;
         when ON_Debug_Line_Decl =>
            Line : Natural;
         when ON_Debug_Comment_Decl =>
            Comment : Str_Acc;
         when ON_Debug_Filename_Decl =>
            Filename : Str_Acc;
      end case;
   end record;

   --  A node for a record element.
   type O_Fnode_Type;
   type O_Fnode is access O_Fnode_Type;

   O_Fnode_Null : constant O_Fnode := null;

   type O_Fnode_Type is record
      --  Record type.
      Parent : O_Tnode;
      --  Next field in the record.
      Next : O_Fnode;
      --  Name of the record field.
      Ident : O_Ident;
      --  Type of the record field.
      Ftype : O_Tnode;
   end record;

   type O_Anode_Type;
   type O_Anode is access O_Anode_Type;
   type O_Anode_Type is record
      Next : O_Anode;
      Formal : O_Dnode;
      Actual : O_Enode;
   end record;

   type OC_Kind is
     (
      OC_Boolean_Lit,
      OC_Unsigned_Lit,
      OC_Signed_Lit,
      OC_Float_Lit,
      OC_Enum_Lit,
      OC_Null_Lit,
      OC_Sizeof_Lit,
      OC_Record_Sizeof_Lit,
      OC_Alignof_Lit,
      OC_Offsetof_Lit,
      OC_Default_Lit,
      OC_Array_Aggregate,
      OC_Record_Aggregate,
      OC_Aggr_Element,
      OC_Union_Aggr,
      OC_Address,
      OC_Unchecked_Address,
      OC_Subprogram_Address
     );
   type O_Cnode_Type (Kind : OC_Kind) is record
      --  Type of the constant.
      Ctype : O_Tnode;
      --  True if referenced.
      Ref : Boolean;
      case Kind is
         when OC_Unsigned_Lit =>
            U_Val : Unsigned_64;
         when OC_Signed_Lit =>
            S_Val : Integer_64;
         when OC_Float_Lit =>
            F_Val : IEEE_Float_64;
         when OC_Boolean_Lit =>
            B_Val : Boolean;
            B_Id : O_Ident;
         when OC_Enum_Lit =>
            E_Val : Integer;
            E_Next : O_Cnode;
            E_Name : O_Ident;
         when OC_Null_Lit =>
            null;
         when OC_Default_Lit =>
            null;
         when OC_Sizeof_Lit
            | OC_Record_Sizeof_Lit
            | OC_Alignof_Lit =>
            S_Type : O_Tnode;
         when OC_Offsetof_Lit =>
            Off_Field : O_Fnode;
         when OC_Array_Aggregate =>
            Arr_Len : Unsigned_32;
            Arr_Els : O_Cnode;
         when OC_Record_Aggregate =>
            Rec_Els : O_Cnode;
         when OC_Union_Aggr =>
            Uaggr_Field : O_Fnode;
            Uaggr_Value : O_Cnode;
         when OC_Aggr_Element =>
            Aggr_Value : O_Cnode;
            Aggr_Next : O_Cnode;
         when OC_Address
           | OC_Unchecked_Address =>
            Addr_Global : O_Gnode;
         when OC_Subprogram_Address =>
            Addr_Decl : O_Dnode;
      end case;
   end record;

   type O_Cnode is access O_Cnode_Type;
   O_Cnode_Null : constant O_Cnode := null;

   type OE_Kind is
     (
      --  Literals.
      OE_Lit,

      --  Dyadic operations.
      OE_Add_Ov,                --  OE_Dyadic_Op_Kind
      OE_Sub_Ov,                --  OE_Dyadic_Op_Kind
      OE_Mul_Ov,                --  OE_Dyadic_Op_Kind
      OE_Div_Ov,                --  OE_Dyadic_Op_Kind
      OE_Rem_Ov,                --  OE_Dyadic_Op_Kind
      OE_Mod_Ov,                --  OE_Dyadic_Op_Kind
      OE_Exp_Ov,                --  OE_Dyadic_Op_Kind

      --  Binary operations.
      OE_And,                   --  OE_Dyadic_Op_Kind
      OE_Or,                    --  OE_Dyadic_Op_Kind
      OE_Xor,                   --  OE_Dyadic_Op_Kind

      --  Monadic operations.
      OE_Not,                   --  OE_Monadic_Op_Kind
      OE_Neg_Ov,                --  OE_Monadic_Op_Kind
      OE_Abs_Ov,                --  OE_Monadic_Op_Kind

      --  Comparaisons
      OE_Eq,                    --  OE_Compare_Op_Kind
      OE_Neq,                   --  OE_Compare_Op_Kind
      OE_Le,                    --  OE_Compare_Op_Kind
      OE_Lt,                    --  OE_Compare_Op_Kind
      OE_Ge,                    --  OE_Compare_Op_Kind
      OE_Gt,                    --  OE_Compare_Op_Kind

      --  Misc.
      OE_Convert_Ov,
      OE_Convert,
      OE_Address,
      OE_Unchecked_Address,
      OE_Alloca,
      OE_Function_Call,

      OE_Value,
      OE_Nil
      );

   subtype OE_Dyadic_Expr_Kind is OE_Kind range OE_Add_Ov .. OE_Xor;
   subtype OE_Monadic_Expr_Kind is OE_Kind range OE_Not .. OE_Abs_Ov;
   subtype OE_Compare_Expr_Kind is OE_Kind range OE_Eq .. OE_Gt;

   type O_Enode_Type (Kind : OE_Kind);
   type O_Enode is access O_Enode_Type;
   O_Enode_Null : constant O_Enode := null;

   type O_Enode_Type (Kind : OE_Kind) is record
      --  Type of the result.
      Rtype : O_Tnode;
      --  True if referenced.
      Ref : Boolean;
      case Kind is
         when OE_Dyadic_Expr_Kind
           | OE_Compare_Expr_Kind =>
            Left : O_Enode;
            Right : O_Enode;
         when OE_Monadic_Expr_Kind =>
            Operand : O_Enode;
         when OE_Lit =>
            Lit : O_Cnode;
         when OE_Address
           | OE_Unchecked_Address =>
            Lvalue : O_Lnode;
         when OE_Convert_Ov
            | OE_Convert =>
            Conv : O_Enode;
         when OE_Function_Call =>
            Func : O_Dnode;
            Assoc : O_Anode;
         when OE_Value =>
            Value : O_Lnode;
         when OE_Alloca =>
            A_Size : O_Enode;
         when OE_Nil =>
            null;
      end case;
   end record;
   type O_Enode_Array is array (Natural range <>) of O_Enode;
   type O_Enode_Array_Acc is access O_Enode_Array;

   type OL_Kind is
     (
      --  Name.
      OL_Obj,
      OL_Indexed_Element,
      OL_Slice,
      OL_Selected_Element,
      OL_Access_Element
      );

   type O_Lnode_Type (Kind : OL_Kind);
   type O_Lnode is access O_Lnode_Type;
   O_Lnode_Null : constant O_Lnode := null;

   type O_Lnode_Type (Kind : OL_Kind) is record
      --  Type of the result.
      Rtype : O_Tnode;
      --  True if referenced.
      Ref : Boolean;
      case Kind is
         when OL_Obj =>
            Obj : O_Dnode;
         when OL_Indexed_Element =>
            Array_Base : O_Lnode;
            Index : O_Enode;
         when OL_Slice =>
            Slice_Base : O_Lnode;
            Slice_Index : O_Enode;
         when OL_Selected_Element =>
            Rec_Base : O_Lnode;
            Rec_El : O_Fnode;
         when OL_Access_Element =>
            Acc_Base : O_Enode;
      end case;
   end record;

   type OG_Kind is
     (
      OG_Decl,
      OG_Selected_Element
     );

   type O_Gnode_Type (Kind : OG_Kind);
   type O_Gnode is access O_Gnode_Type;
   O_Gnode_Null : constant O_Gnode := null;

   type O_Gnode_Type (Kind : OG_Kind) is record
      --  Type of the result.
      Rtype : O_Tnode;
      --  True if referenced.
      Ref : Boolean;
      case Kind is
         when OG_Decl =>
            Decl : O_Dnode;
         when OG_Selected_Element =>
            Rec_Base : O_Gnode;
            Rec_El : O_Fnode;
      end case;
   end record;

   O_Tnode_Null : constant O_Tnode := null;
   type ON_Type_Kind is
     (ON_Boolean_Type, ON_Enum_Type,
      ON_Unsigned_Type, ON_Signed_Type, ON_Float_Type,
      ON_Array_Type, ON_Array_Subtype,
      ON_Record_Type, ON_Record_Subtype,
      ON_Union_Type, ON_Access_Type);

   subtype ON_Array_Kinds is ON_Type_Kind
     range ON_Array_Type .. ON_Array_Subtype;

   type O_Tnode_Type (Kind : ON_Type_Kind) is record
      Decl : O_Dnode;
      --  True if the type was first created as an uncomplete type.
      Uncomplete : Boolean;
      --  True if the type is complete.
      Complete : Boolean;
      --  True if the type is fully constrained.
      Constrained : Boolean;
      case Kind is
         when ON_Boolean_Type =>
            True_N : O_Cnode;
            False_N : O_Cnode;
         when ON_Unsigned_Type
           | ON_Signed_Type =>
            Int_Size : Natural;
         when ON_Float_Type =>
            null;
         when ON_Enum_Type =>
            Nbr : Natural;
            Literals: O_Cnode;
         when ON_Access_Type =>
            D_Type : O_Tnode;
         when ON_Array_Type =>
            El_Type : O_Tnode;
            Index_Type : O_Tnode;
         when ON_Array_Subtype =>
            Length : O_Cnode;
            Arr_El_Type : O_Tnode;
            Arr_Base : O_Tnode;
         when ON_Record_Type
           | ON_Union_Type =>
            Rec_Elements : O_Fnode;
         when ON_Record_Subtype =>
            Subrec_Elements : O_Fnode;
            Subrec_Base : O_Tnode;
      end case;
   end record;

   type ON_Choice_Kind is (ON_Choice_Expr, ON_Choice_Range, ON_Choice_Default);
   type O_Choice_Type (Kind : ON_Choice_Kind);
   type O_Choice is access O_Choice_Type;
   type O_Choice_Type (Kind : ON_Choice_Kind) is record
      Next : O_Choice;
      case Kind is
         when ON_Choice_Expr =>
            Expr : O_Cnode;
         when ON_Choice_Range =>
            Low, High : O_Cnode;
         when ON_Choice_Default =>
            null;
      end case;
   end record;

   O_Snode_Null : constant O_Snode := null;
   type ON_Stmt_Kind is
     (ON_Declare_Stmt, ON_Assign_Stmt, ON_Return_Stmt, ON_If_Stmt,
      ON_Elsif_Stmt, ON_Loop_Stmt, ON_Exit_Stmt, ON_Next_Stmt,
      ON_Case_Stmt, ON_When_Stmt, ON_Call_Stmt,
      ON_Debug_Line_Stmt, ON_Debug_Comment_Stmt);
   type O_Snode_Type (Kind : ON_Stmt_Kind) is record
      Next : O_Snode;
      Lineno : Natural;
      case Kind is
         when ON_Declare_Stmt =>
            Decls : O_Dnode;
            Stmts : O_Snode;
            --  True if the statement is currently open.
            Alive : Boolean;
         when ON_Assign_Stmt =>
            Target : O_Lnode;
            Value : O_Enode;
         when ON_Return_Stmt =>
            Ret_Val : O_Enode;
         when ON_If_Stmt =>
            Elsifs : O_Snode;
            If_Last : O_Snode;
         when ON_Elsif_Stmt =>
            Cond : O_Enode;
            Next_Elsif : O_Snode;
         when ON_Loop_Stmt =>
            Loop_Last : O_Snode;
            Loop_Level : Natural;
         when ON_Exit_Stmt
           | ON_Next_Stmt =>
            Loop_Id : O_Snode;
         when ON_Case_Stmt =>
            Selector : O_Enode;
            --  Simply linked list of branches
            Branches : O_Snode;
            Case_Last : O_Snode;
         when ON_When_Stmt =>
            --  The corresponding 'case'
            Branch_Parent : O_Snode;
            Choice_List : O_Choice;
            Next_Branch : O_Snode;
         when ON_Call_Stmt =>
            Proc : O_Dnode;
            Assoc : O_Anode;
         when ON_Debug_Line_Stmt =>
            Line : Natural;
         when ON_Debug_Comment_Stmt =>
            Comment : Str_Acc;
      end case;
   end record;

   type O_Inter_List is record
      Func : O_Dnode;
      Last : O_Dnode;
   end record;

   type O_Element_List is record
      --  The type definition.
      Res : O_Tnode;
      --  The last element added.
      Last : O_Fnode;
   end record;

   type O_Element_Sublist is record
      --  The type definition.
      Res : O_Tnode;
      --  The last element added.
      Last : O_Fnode;
      --  The correspond field from the base type.
      Base_Field : O_Fnode;
   end record;

   type O_Record_Aggr_List is record
      Res : O_Cnode;
      Last : O_Cnode;
      Field : O_Fnode;
   end record;

   type O_Array_Aggr_List is record
      Res : O_Cnode;
      Last : O_Cnode;
      El_Type : O_Tnode;
   end record;

   type O_Assoc_List is record
      Subprg : O_Dnode;
      Interfaces : O_Dnode;
      First, Last : O_Anode;
   end record;

   type O_Enum_List is record
      --  The type built.
      Res : O_Tnode;

      --  the chain of declarations.
      Last : O_Cnode;
   end record;
   type O_Case_Block is record
      Case_Stmt : O_Snode;
   end record;

   type O_If_Block is record
      null;
   end record;
end Ortho_Debug;
