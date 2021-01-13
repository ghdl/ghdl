--  Mcode back-end for ortho.
--  Copyright (C) 2006 Tristan Gingold
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
with Ortho_Code; use Ortho_Code;
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.Consts; use Ortho_Code.Consts;
with Ortho_Code.Decls; use Ortho_Code.Decls;
with Ortho_Code.Exprs; use Ortho_Code.Exprs;

--  Interface to create nodes.
package Ortho_Mcode is
   --  Initialize nodes.
   procedure Init;
   procedure Finish;

   procedure Free_All;

private
   --  MCode supports nested subprograms.
   Has_Nested_Subprograms : constant Boolean := True;

   type O_Tnode is new Ortho_Code.O_Tnode;
   type O_Cnode is new Ortho_Code.O_Cnode;
   type O_Dnode is new Ortho_Code.O_Dnode;
   type O_Enode is new Ortho_Code.O_Enode;
   type O_Fnode is new Ortho_Code.O_Fnode;
   type O_Lnode is new Ortho_Code.O_Lnode;
   type O_Gnode is new Ortho_Code.O_Gnode;
   type O_Snode is new Ortho_Code.Exprs.O_Snode;

   O_Lnode_Null : constant O_Lnode := O_Lnode (Ortho_Code.O_Lnode_Null);
   O_Gnode_Null : constant O_Gnode := O_Gnode (Ortho_Code.O_Gnode_Null);
   O_Cnode_Null : constant O_Cnode := O_Cnode (Ortho_Code.O_Cnode_Null);
   O_Dnode_Null : constant O_Dnode := O_Dnode (Ortho_Code.O_Dnode_Null);
   O_Enode_Null : constant O_Enode := O_Enode (Ortho_Code.O_Enode_Null);
   O_Fnode_Null : constant O_Fnode := O_Fnode (Ortho_Code.O_Fnode_Null);
   O_Snode_Null : constant O_Snode := O_Snode (Ortho_Code.Exprs.O_Snode_Null);
   O_Tnode_Null : constant O_Tnode := O_Tnode (Ortho_Code.O_Tnode_Null);

   type O_Element_List     is new Ortho_Code.Types.O_Element_List;
   type O_Element_Sublist  is new Ortho_Code.Types.O_Element_List;
   type O_Enum_List        is new Ortho_Code.Types.O_Enum_List;
   type O_Inter_List       is new Ortho_Code.Decls.O_Inter_List;
   type O_Record_Aggr_List is new Ortho_Code.Consts.O_Record_Aggr_List;
   type O_Array_Aggr_List  is new Ortho_Code.Consts.O_Array_Aggr_List;
   type O_Assoc_List       is new Ortho_Code.Exprs.O_Assoc_List;
   type O_If_Block         is new Ortho_Code.Exprs.O_If_Block;
   type O_Case_Block       is new Ortho_Code.Exprs.O_Case_Block;

   pragma Inline (New_Lit);
   pragma Inline (New_Dyadic_Op);
   pragma Inline (New_Monadic_Op);
   pragma Inline (New_Compare_Op);
   pragma Inline (New_Signed_Literal);
   pragma Inline (New_Unsigned_Literal);
   pragma Inline (New_Float_Literal);
   pragma Inline (New_Null_Access);

   pragma Inline (Start_Record_Aggr);
   pragma Inline (New_Record_Aggr_El);
   pragma Inline (Finish_Record_Aggr);

   pragma Inline (Start_Array_Aggr);
   pragma Inline (New_Array_Aggr_El);
   pragma Inline (Finish_Array_Aggr);

   pragma Inline (New_Union_Aggr);
   pragma Inline (New_Sizeof);
   pragma Inline (New_Alignof);
   pragma Inline (New_Offsetof);

   pragma Inline (New_Indexed_Element);
   pragma Inline (New_Slice);
   pragma Inline (New_Selected_Element);
   pragma Inline (New_Access_Element);

   pragma Inline (New_Convert_Ov);

   pragma Inline (New_Address);
   pragma Inline (New_Global_Address);
   pragma Inline (New_Unchecked_Address);
   pragma Inline (New_Global_Unchecked_Address);
   pragma Inline (New_Subprogram_Address);

   pragma Inline (New_Value);
   pragma Inline (New_Obj_Value);

   pragma Inline (New_Alloca);

   pragma Inline (New_Debug_Filename_Decl);
   pragma Inline (New_Debug_Line_Decl);
   pragma Inline (New_Debug_Comment_Decl);

   pragma Inline (New_Type_Decl);
   pragma Inline (New_Const_Decl);

   pragma Inline (Start_Init_Value);
   pragma Inline (Finish_Init_Value);
   pragma Inline (New_Var_Decl);

   pragma Inline (New_Obj);
   pragma Inline (Start_Function_Decl);
   pragma Inline (Start_Procedure_Decl);
   pragma Inline (New_Interface_Decl);
   pragma Inline (Finish_Subprogram_Decl);
   pragma Inline (Start_Subprogram_Body);
   pragma Inline (Finish_Subprogram_Body);

   pragma Inline (New_Debug_Line_Stmt);
   pragma Inline (New_Debug_Comment_Stmt);

   pragma Inline (Start_Declare_Stmt);
   pragma Inline (Finish_Declare_Stmt);

   --  Create a function call or a procedure call.
   pragma Inline (Start_Association);
   pragma Inline (New_Association);
   pragma Inline (New_Function_Call);
   pragma Inline (New_Procedure_Call);

   pragma Inline (New_Assign_Stmt);
   pragma Inline (New_Return_Stmt);
   pragma Inline (Start_If_Stmt);
   pragma Inline (New_Else_Stmt);
   pragma Inline (Finish_If_Stmt);

   pragma Inline (Start_Loop_Stmt);
   pragma Inline (Finish_Loop_Stmt);
   pragma Inline (New_Exit_Stmt);
   pragma Inline (New_Next_Stmt);

   pragma Inline (Start_Case_Stmt);
   pragma Inline (Start_Choice);
   pragma Inline (New_Expr_Choice);
   pragma Inline (New_Range_Choice);
   pragma Inline (New_Default_Choice);
   pragma Inline (Finish_Choice);
   pragma Inline (Finish_Case_Stmt);
end Ortho_Mcode;
