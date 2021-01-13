--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
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
with Interfaces.C; use Interfaces.C;
with Ortho_Ident; use Ortho_Ident;

--  Interface to create nodes.
package Ortho_LLVM is
   procedure Init (Filename : String; Filename_Length : Natural);
   pragma Import (C, Init, "ortho_llvm_init");

   type Opaque_Type is null record;

   procedure Set_Optimization_Level (Level : Natural);
   pragma Import (C, Set_Optimization_Level);

   procedure Set_Debug_Level (Level : Natural);
   pragma Import (C, Set_Debug_Level);

   procedure Set_Dump_LLVM (Flag : Natural);
   pragma Import (C, Set_Dump_LLVM);

   procedure Set_Verify_LLVM (Flag : Natural);
   pragma Import (C, Set_Verify_LLVM);

   procedure Set_PIC_Flag (Flag : Natural);
   pragma Import (C, Set_PIC_Flag);

   procedure Generate_Object (Filename : System.Address);
   pragma Import (C, Generate_Object);
   procedure Generate_Assembly (Filename : System.Address);
   pragma Import (C, Generate_Assembly);
   procedure Generate_Llvm (Filename : System.Address);
   pragma Import (C, Generate_Llvm);
   procedure Generate_Bitcode (Filename : System.Address);
   pragma Import (C, Generate_Bitcode);

   --  LLVM specific: the module.
   --  Module : ModuleRef;

   --  Descriptor for the layout.
   --  Target_Data : LLVM.Target.TargetDataRef;

   --  Target_Machine : LLVM.TargetMachine.TargetMachineRef;

   --  Optimization level
   --  Optimization : LLVM.TargetMachine.CodeGenOptLevel :=
   --    LLVM.TargetMachine.CodeGenLevelDefault;

private
   pragma Convention (C, O_Storage);

   --  No support for nested subprograms in LLVM.
   Has_Nested_Subprograms : constant Boolean := False;

   type TypeRef is access Opaque_Type;
   pragma Convention (C, TypeRef);

   type BasicBlockRef is access Opaque_Type;
   pragma Convention (C, BasicBlockRef);

   Null_BasicBlockRef : constant BasicBlockRef := null;

   type ValueRef is access Opaque_Type;
   pragma Convention (C, ValueRef);

   Null_ValueRef : constant ValueRef := null;

   type O_Tnode is access Opaque_Type;
   pragma Convention (C, O_Tnode);

   O_Tnode_Null : constant O_Tnode := null;

   type O_Dnode is access Opaque_Type;
   pragma Convention (C, O_Dnode);

   type On_Decl_Kind is
     (ON_Type_Decl, ON_Completed_Type_Decl,
      ON_Const_Decl,
      ON_Var_Decl, ON_Local_Decl, ON_Interface_Decl,
      ON_Subprg_Decl,
      ON_No_Decl);

   --  type O_Dnode (Kind : On_Decl_Kind := ON_No_Decl) is record
   --     Dtype : O_Tnode;
   --     LLVM : ValueRef;
   --     case Kind is
   --        when ON_Var_Decl
   --          | ON_Const_Decl
   --          | ON_Local_Decl =>
   --           null;
   --        when ON_Subprg_Decl =>
   --           Subprg_Id : O_Ident;
   --           Nbr_Args : unsigned;
   --           Subprg_Inters : O_Inter_Acc;
   --        when ON_Interface_Decl =>
   --           Inter : O_Inter_Acc;
   --        when others =>
   --           null;
   --     end case;
   --  end record;

   O_Dnode_Null : constant O_Dnode := null;

   type OF_Kind is (OF_None, OF_Record, OF_Union);
   pragma Convention (C, OF_Kind);

   type O_Fnode is access Opaque_Type;
   pragma Convention (C, O_Fnode);

   O_Fnode_Null : constant O_Fnode := null;

   type O_Anode_Type;
   type O_Anode is access O_Anode_Type;
   type O_Anode_Type is record
      Next : O_Anode;
      Formal : O_Dnode;
      Actual : O_Enode;
   end record;

   type O_Cnode is record
      LLVM : ValueRef;
      Ctype : O_Tnode;
   end record;
   --  Note: this record is always passed by reference.
   pragma Convention (C, O_Cnode);

   O_Cnode_Null : constant O_Cnode := (LLVM => Null_ValueRef,
                                       Ctype => O_Tnode_Null);

   type O_Enode is record
      LLVM : ValueRef;
      Etype : O_Tnode;
   end record;
   pragma Convention (C_Pass_By_Copy, O_Enode);

   O_Enode_Null : constant O_Enode := (LLVM => Null_ValueRef,
                                       Etype => O_Tnode_Null);


   type O_Lnode is record
      --  If True, the LLVM component is the value (used for arguments).
      --  If False, the LLVM component is the address of the value (used
      --   for everything else).
      Direct : Boolean;
      LLVM : ValueRef;
      Ltype : O_Tnode;
   end record;
   --  Note: this record is always passed by reference.
   pragma Convention (C, O_Lnode);

   O_Lnode_Null : constant O_Lnode := (False, Null_ValueRef, O_Tnode_Null);

   type O_Gnode is record
      Ref : ValueRef;
      Ltype : O_Tnode;
   end record;
   pragma Convention (C_Pass_By_Copy, O_Gnode);

   O_Gnode_Null : constant O_Gnode := (Null_ValueRef, O_Tnode_Null);

   type O_Snode is record
      --  First BB in the loop body.
      Bb_Entry : BasicBlockRef;

      --  BB after the loop.
      Bb_Exit : BasicBlockRef;
   end record;
   pragma Convention (C, O_Snode);

   O_Snode_Null : constant O_Snode := (Null_BasicBlockRef,
                                       Null_BasicBlockRef);

   type Opaque_Acc is access Opaque_Type;

   type O_Inter_List is record
      Ident : O_Ident;
      Storage : O_Storage;
      Res_Type : O_Tnode;

      --  Vector of interfaces.
      Inters : Opaque_Acc;
   end record;
   pragma Convention (C, O_Inter_List);

   type O_Element_Vec is access Opaque_Type;
   pragma Convention (C, O_Element_Vec);

   --  Record and union builder.
   type O_Element_List is record
      Kind : OF_Kind;

      --  Number of fields.
      Nbr_Elements : Natural;

      --  For record: the access to the incomplete (but named) type.
      Rec_Type : O_Tnode;

      --  For unions: biggest for size and alignment
      Size : unsigned;
      Align : Unsigned_32;
      Align_Type : TypeRef;

      Els : O_Element_Vec;
   end record;
   pragma Convention (C, O_Element_List);

   type O_Element_Sublist is record
      --  Number of fields.
      Count : Natural;
      Base_Els : O_Element_Vec;
      Els : O_Element_Vec;
   end record;
   pragma Convention (C, O_Element_Sublist);

   type ValueRefArray_Acc is access Opaque_Type;
   pragma Convention (C, ValueRefArray_Acc);

   type O_Record_Aggr_List is record
      --  Current number of elements in Vals.
      Len : unsigned;

      --  Value of elements.
      Vals : ValueRefArray_Acc;

      --  Type of the aggregate.
      Atype : O_Tnode;
   end record;
   pragma Convention (C, O_Record_Aggr_List);

   type O_Array_Aggr_List is record
      --  Current number of elements in Vals.
      Len : unsigned;

      --  Value of elements.
      Vals : ValueRefArray_Acc;
      El_Type : TypeRef;

      --  Type of the aggregate.
      Atype : O_Tnode;
   end record;
   pragma Convention (C, O_Array_Aggr_List);

   type O_Assoc_List is record
      Subprg : O_Dnode;
      Idx : unsigned;
      Vals : ValueRefArray_Acc;
   end record;
   pragma Convention (C, O_Assoc_List);

   type O_Enum_List is record
      LLVM : TypeRef;
      Num : Natural;
      Etype : O_Tnode;
      Dbg : ValueRefArray_Acc;
   end record;

   type O_Choice_Type is record
      Low, High : ValueRef;
      Bb : BasicBlockRef;
   end record;

   type O_Choice_Array is array (Natural range <>) of O_Choice_Type;
   type O_Choice_Array_Acc is access O_Choice_Array;

   type O_Case_Block is record
      --  BB before the case.
      BB_Prev : BasicBlockRef;

      --  Select expression
      Value : ValueRef;
      Vtype : O_Tnode;

      --  BB after the case statement.
      BB_Next : BasicBlockRef;

      --  BB for others
      BB_Others : BasicBlockRef;

      --  BB for the current choice
      BB_Choice : BasicBlockRef;

      --  List of choices.
      Choices : O_Choice_Array_Acc;
   end record;
   pragma Convention (C, O_Case_Block);

   type O_If_Block is record
      --  The next basic block.
      --  After the 'If', this is the BB for the else part.  If there is no
      --   else part, this is the BB for statements after the if.
      --  After the 'else', this is the BB for statements after the if.
      Bb : BasicBlockRef;
   end record;
   pragma Convention (C, O_If_Block);

   --  function Get_LLVM_Type (Atype : O_Tnode) return TypeRef;

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
   pragma Import (C, New_Uncomplete_Record_Type);
   pragma Import (C, Start_Uncomplete_Record_Type);

   pragma Import (C, Start_Record_Subtype);
   pragma Import (C, New_Subrecord_Field);
   pragma Import (C, Finish_Record_Subtype);

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

   pragma Import (C, New_Lit);
   pragma Import (C, New_Obj);
   pragma Import (C, New_Obj_Value);
   pragma Import (C, New_Global);
   pragma Import (C, New_Global_Selected_Element);
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
end Ortho_LLVM;
