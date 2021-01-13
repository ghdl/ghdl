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

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Ortho_Ident; use Ortho_Ident;
with LLVM.Core; use LLVM.Core;
with LLVM.TargetMachine;
with LLVM.Target;

--  Interface to create nodes.
package Ortho_LLVM is
   procedure Init;

   --  LLVM specific: the module.
   Module : ModuleRef;

   --  Descriptor for the layout.
   Target_Data : LLVM.Target.TargetDataRef;

   Target_Machine : LLVM.TargetMachine.TargetMachineRef;

   --  Optimization level
   Optimization : LLVM.TargetMachine.CodeGenOptLevel :=
     LLVM.TargetMachine.CodeGenLevelDefault;

private
   --  No support for nested subprograms in LLVM.
   Has_Nested_Subprograms : constant Boolean := False;

   type O_Tnode_Type (<>);
   type O_Tnode is access O_Tnode_Type;
   O_Tnode_Null : constant O_Tnode := null;

   type ON_Type_Kind is
     (ON_No_Type,
      ON_Unsigned_Type, ON_Signed_Type, ON_Enum_Type, ON_Boolean_Type,
      ON_Float_Type,
      ON_Array_Type, ON_Array_Sub_Type,
      ON_Incomplete_Record_Type,
      ON_Record_Type, ON_Union_Type,
      ON_Incomplete_Access_Type, ON_Access_Type);

   subtype ON_Scalar_Types is ON_Type_Kind range
     ON_Unsigned_Type .. ON_Float_Type;

   subtype ON_Integer_Types is ON_Type_Kind range
     ON_Unsigned_Type .. ON_Boolean_Type;

   type O_Tnode_Type (Kind : ON_Type_Kind := ON_No_Type) is record
      LLVM : TypeRef;
      Dbg : ValueRef;
      case Kind is
         when ON_No_Type =>
            null;
         when ON_Union_Type =>
            Un_Size : unsigned;
            Un_Main_Field : TypeRef;
         when ON_Access_Type
           | ON_Incomplete_Access_Type =>
            Acc_Type : O_Tnode;
         when ON_Scalar_Types =>
            Scal_Size : Natural;
         when ON_Array_Type
           | ON_Array_Sub_Type =>
            --  Type of the element
            Arr_El_Type : O_Tnode;
         when ON_Record_Type
           | ON_Incomplete_Record_Type =>
            null;
      end case;
   end record;

   type O_Inter;
   type O_Inter_Acc is access O_Inter;
   type O_Inter is record
      Itype : O_Tnode;
      Ival : ValueRef;
      Ident : O_Ident;
      Next : O_Inter_Acc;
   end record;

   type On_Decl_Kind is
     (ON_Type_Decl, ON_Completed_Type_Decl,
      ON_Const_Decl,
      ON_Var_Decl, ON_Local_Decl, ON_Interface_Decl,
      ON_Subprg_Decl,
      ON_No_Decl);

   type O_Dnode (Kind : On_Decl_Kind := ON_No_Decl) is record
      Dtype : O_Tnode;
      LLVM : ValueRef;
      case Kind is
         when ON_Var_Decl
           | ON_Const_Decl
           | ON_Local_Decl =>
            null;
         when ON_Subprg_Decl =>
            Subprg_Id : O_Ident;
            Nbr_Args : unsigned;
            Subprg_Inters : O_Inter_Acc;
         when ON_Interface_Decl =>
            Inter : O_Inter_Acc;
         when others =>
            null;
      end case;
   end record;

   O_Dnode_Null : constant O_Dnode := (Kind => ON_No_Decl,
                                       Dtype => O_Tnode_Null,
                                       LLVM => Null_ValueRef);

   type OF_Kind is (OF_None, OF_Record, OF_Union);
   type O_Fnode (Kind : OF_Kind := OF_None) is record
      --  Type of the field.
      Ftype : O_Tnode;
      case Kind is
         when OF_None =>
            null;
         when OF_Record =>
            --  Field index (starting from 0).
            Index : Natural;
         when OF_Union =>
            Utype : TypeRef;
            Ptr_Type : TypeRef;
      end case;
   end record;

   O_Fnode_Null : constant O_Fnode := (Kind => OF_None,
                                       Ftype => O_Tnode_Null);

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
   O_Cnode_Null : constant O_Cnode := (LLVM => Null_ValueRef,
                                       Ctype => O_Tnode_Null);

   type O_Enode is record
      LLVM : ValueRef;
      Etype : O_Tnode;
   end record;
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

   O_Lnode_Null : constant O_Lnode := (False, Null_ValueRef, O_Tnode_Null);

   type O_Gnode is record
      LLVM : ValueRef;
      Ltype : O_Tnode;
   end record;

   O_Gnode_Null : constant O_Gnode := (Null_ValueRef, O_Tnode_Null);

   type O_Snode is record
      --  First BB in the loop body.
      Bb_Entry : BasicBlockRef;

      --  BB after the loop.
      Bb_Exit : BasicBlockRef;
   end record;

   O_Snode_Null : constant O_Snode := (Null_BasicBlockRef,
                                       Null_BasicBlockRef);

   type O_Inter_List is record
      Ident : O_Ident;
      Storage : O_Storage;
      Res_Type : O_Tnode;
      Nbr_Inter : Natural;
      First_Inter, Last_Inter : O_Inter_Acc;
   end record;

   type O_Element;
   type O_Element_Acc is access O_Element;
   type O_Element is record
      --  Identifier for the element
      Ident : O_Ident;

      --  Type of the element
      Etype : O_Tnode;

      --  Next element (in the linked list)
      Next : O_Element_Acc;
   end record;

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

      First_Elem, Last_Elem : O_Element_Acc;
   end record;

   type ValueRefArray_Acc is access ValueRefArray;

   type O_Record_Aggr_List is record
      --  Current number of elements in Vals.
      Len : unsigned;

      --  Value of elements.
      Vals : ValueRefArray_Acc;

      --  Type of the aggregate.
      Atype : O_Tnode;
   end record;

   type O_Array_Aggr_List is record
      --  Current number of elements in Vals.
      Len : unsigned;

      --  Value of elements.
      Vals : ValueRefArray_Acc;
      El_Type : TypeRef;

      --  Type of the aggregate.
      Atype : O_Tnode;
   end record;

   type O_Assoc_List is record
      Subprg : O_Dnode;
      Idx : unsigned;
      Vals : ValueRefArray_Acc;
   end record;

   type O_Enum_List is record
      LLVM : TypeRef;
      Num : Natural;
      Etype : O_Tnode;
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
      Nbr_Choices : Natural;
      Choices : O_Choice_Array_Acc;
   end record;

   type O_If_Block is record
      --  The next basic block.
      --  After the 'If', this is the BB for the else part.  If there is no
      --   else part, this is the BB for statements after the if.
      --  After the 'else', this is the BB for statements after the if.
      Bb : BasicBlockRef;
   end record;

   function Get_LLVM_Type (Atype : O_Tnode) return TypeRef;
end Ortho_LLVM;
