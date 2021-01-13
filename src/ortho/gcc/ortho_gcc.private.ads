--  GCC back-end for ortho.
--  Copyright (C) 2002-1014 Tristan Gingold
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
with System;
with Interfaces; use Interfaces;
with Ortho_Ident;
use Ortho_Ident;

--  Interface to create nodes.
package Ortho_Gcc is

private
   --  GCC supports nested subprograms.
   Has_Nested_Subprograms : constant Boolean := True;

   pragma Convention (C, O_Storage);
   --   pragma Convention (C, ON_Op_Kind);

   subtype Tree is System.Address;
   NULL_TREE : constant Tree := System.Null_Address;

   subtype Vec_Ptr is System.Address;

   type O_Cnode is new Tree;
   type O_Enode is new Tree;
   type O_Lnode is new Tree;
   type O_Gnode is new Tree;
   type O_Tnode is new Tree;
   type O_Fnode is new Tree;
   type O_Dnode is new Tree;
   type O_Snode is record
      Beg_Label : Tree;
      End_Label : Tree;
   end record;
   pragma Convention (C, O_Snode);

   O_Cnode_Null : constant O_Cnode := O_Cnode (NULL_TREE);
   O_Enode_Null : constant O_Enode := O_Enode (NULL_TREE);
   O_Lnode_Null : constant O_Lnode := O_Lnode (NULL_TREE);
   O_Gnode_Null : constant O_Gnode := O_Gnode (NULL_TREE);
   O_Tnode_Null : constant O_Tnode := O_Tnode (NULL_TREE);
   O_Fnode_Null : constant O_Fnode := O_Fnode (NULL_TREE);
   O_Snode_Null : constant O_Snode := (NULL_TREE, NULL_TREE);
   O_Dnode_Null : constant O_Dnode := O_Dnode (NULL_TREE);

   pragma Inline (New_Lit);
   pragma Inline (New_Obj);
   pragma Inline (New_Obj_Value);

   --  Efficiently append element EL to a chain.
   --  FIRST is the first element of the chain (must NULL_TREE if the chain
   --   is empty),
   --  LAST is the last element of the chain (idem).
   type Chain_Constr_Type is record
      First : Tree;
      Last : Tree;
   end record;
   pragma Convention (C, Chain_Constr_Type);
   procedure Chain_Init (Constr : out Chain_Constr_Type);
   pragma Import (C, Chain_Init);
   procedure Chain_Append (Constr : in out Chain_Constr_Type; El : Tree);
   pragma Import (C, Chain_Append);

   --  Efficiently append element EL to a list.
   type List_Constr_Type is record
      First : Tree;
      Last : Tree;
   end record;
   pragma Convention (C, List_Constr_Type);
   procedure List_Init (Constr : out List_Constr_Type);
   pragma Import (C, List_Init);
   procedure List_Append (Constr : in out List_Constr_Type; El : Tree);
   pragma Import (C, List_Append, "ortho_list_append");

   type O_Loop_Block is record
      Beg_Label : Tree;
      End_Label : Tree;
   end record;
   pragma Convention (C, O_Loop_Block);

   type O_Inter_List is record
      Ident : O_Ident;
      Storage : O_Storage;
      --  Return type.
      Rtype : O_Tnode;
      --  List of parameter types.
      Param_List : List_Constr_Type;
      --  Chain of parameters declarations.
      Param_Chain : Chain_Constr_Type;
   end record;
   pragma Convention (C, O_Inter_List);

   type O_Element_List is record
      Res : Tree;
      Chain : Chain_Constr_Type;
   end record;
   pragma Convention (C, O_Element_List);

   type O_Element_Sublist is record
      Base : Tree;
      Field : Tree;
      Res : Tree;
      Chain : Chain_Constr_Type;
   end record;
   pragma Convention (C, O_Element_Sublist);

   type O_Case_Block is record
      Prev_Stmts : Tree;
      Case_Type : Tree;
      End_Label : Tree;
      Add_Break : Integer;
   end record;
   pragma Convention (C, O_Case_Block);

   type O_If_Block is record
      Prev_Stmts : Tree;
      If_Stmt : Tree;
   end record;
   pragma Convention (C, O_If_Block);

   type O_Aggr_List is record
      Atype : Tree;
      Chain : Chain_Constr_Type;
   end record;

   type O_Record_Aggr_List is record
      Atype : Tree;
      Afield : Tree;
      Vec : Vec_Ptr;
   end record;
   pragma Convention (C, O_Record_Aggr_List);

   type O_Array_Aggr_List is record
      Atype : Tree;
      Vec : Vec_Ptr;
   end record;
   pragma Convention (C, O_Array_Aggr_List);

   type O_Assoc_List is record
      Subprg : Tree;
      List : List_Constr_Type;
   end record;
   pragma Convention (C, O_Assoc_List);

   type O_Enum_List is record
      --  The enumeral_type node.
      Res : Tree;
      --  Chain of literals.
      Chain : Chain_Constr_Type;
      --  Numeral value (from 0 to nbr - 1) of the next literal to be declared.
      Num : Natural;
      --  Size of the enumeration type.
      Size : Natural;
   end record;
   pragma Convention (C, O_Enum_List);

   pragma Import (C, New_Dyadic_Op);
   pragma Import (C, New_Monadic_Op);
   pragma Import (C, New_Compare_Op);

   pragma Import (C, New_Convert_Ov);
   pragma Import (C, New_Convert);
   pragma Import (C, New_Alloca);

   pragma Import (C, New_Signed_Literal);
   pragma Import (C, New_Unsigned_Literal);
   pragma Import (C, New_Float_Literal);
   pragma Import (C, New_Null_Access);

   pragma Import (C, Start_Record_Type);
   pragma Import (C, New_Record_Field);
   pragma Import (C, Finish_Record_Type);

   pragma Import (C, Start_Record_Subtype);
   pragma Import (C, New_Subrecord_Field);
   pragma Import (C, Finish_Record_Subtype);

   pragma Import (C, New_Uncomplete_Record_Type);
   pragma Import (C, Start_Uncomplete_Record_Type);

   pragma Import (C, Start_Union_Type);
   pragma Import (C, New_Union_Field);
   pragma Import (C, Finish_Union_Type);

   pragma Import (C, New_Unsigned_Type);
   pragma Import (C, New_Signed_Type);
   pragma Import (C, New_Float_Type);

   pragma Import (C, New_Access_Type);
   pragma Import (C, Finish_Access_Type);

   pragma Import (C, New_Array_Type);
   pragma Import (C, New_Array_Subtype);

   pragma Import (C, New_Boolean_Type);
   pragma Import (C, Start_Enum_Type);
   pragma Import (C, New_Enum_Literal);
   pragma Import (C, Finish_Enum_Type);

   pragma Import (C, Start_Record_Aggr);
   pragma Import (C, New_Record_Aggr_El);
   pragma Import (C, Finish_Record_Aggr);
   pragma Import (C, Start_Array_Aggr);
   pragma Import (C, New_Array_Aggr_El);
   pragma Import (C, Finish_Array_Aggr);
   pragma Import (C, New_Union_Aggr);
   pragma Import (C, New_Default_Value);

   pragma Import (C, New_Indexed_Element);
   pragma Import (C, New_Slice);
   pragma Import (C, New_Selected_Element);
   pragma Import (C, New_Access_Element);

   pragma Import (C, New_Sizeof);
   pragma Import (C, New_Record_Sizeof);
   pragma Import (C, New_Alignof);
   pragma Import (C, New_Offsetof);

   pragma Import (C, New_Address);
   pragma Import (C, New_Global_Address);
   pragma Import (C, New_Unchecked_Address);
   pragma Import (C, New_Global_Unchecked_Address);
   pragma Import (C, New_Subprogram_Address);

   pragma Import (C, New_Value);

   pragma Import (C, New_Type_Decl);
   pragma Import (C, New_Debug_Line_Decl);
   pragma Import (C, New_Const_Decl);
   pragma Import (C, New_Var_Decl);

   pragma Import (C, Start_Init_Value);
   pragma Import (C, Finish_Init_Value);

   pragma Import (C, Start_Function_Decl);
   pragma Import (C, Start_Procedure_Decl);
   pragma Import (C, New_Interface_Decl);
   pragma Import (C, Finish_Subprogram_Decl);

   pragma Import (C, Start_Subprogram_Body);
   pragma Import (C, Finish_Subprogram_Body);

   pragma Import (C, New_Debug_Line_Stmt);
   pragma Import (C, Start_Declare_Stmt);
   pragma Import (C, Finish_Declare_Stmt);
   pragma Import (C, Start_Association);
   pragma Import (C, New_Association);
   pragma Import (C, New_Function_Call);
   pragma Import (C, New_Procedure_Call);

   pragma Import (C, New_Assign_Stmt);

   pragma Import (C, Start_If_Stmt);
   pragma Import (C, New_Else_Stmt);
   pragma Import (C, Finish_If_Stmt);

   pragma Import (C, New_Return_Stmt);
   pragma Import_Procedure (New_Return_Stmt,
                              "new_func_return_stmt", (O_Enode));
   pragma Import_Procedure (New_Return_Stmt,
                              "new_proc_return_stmt", null);

   pragma Import (C, Start_Loop_Stmt);
   pragma Import (C, Finish_Loop_Stmt);
   pragma Import (C, New_Exit_Stmt);
   pragma Import (C, New_Next_Stmt);

   pragma Import (C, Start_Case_Stmt);
   pragma Import (C, Start_Choice);
   pragma Import (C, New_Expr_Choice);
   pragma Import (C, New_Range_Choice);
   pragma Import (C, New_Default_Choice);
   pragma Import (C, Finish_Choice);
   pragma Import (C, Finish_Case_Stmt);
end Ortho_Gcc;
