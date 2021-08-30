--  PSL - Nodes definition
--  Copyright (C) 2002-2016 Tristan Gingold
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
with PSL.Types; use PSL.Types;

package PSL.Nodes is
   type Nkind is
     (
      N_Error,

      N_Vmode,
      N_Vunit,
      N_Vprop,

      N_Hdl_Mod_Name,

      N_Assert_Directive,
      N_Property_Declaration,
      N_Sequence_Declaration,
      N_Endpoint_Declaration,

      --  Formal parameters
      N_Const_Parameter,
      N_Boolean_Parameter,
      N_Property_Parameter,
      N_Sequence_Parameter,

      N_Sequence_Instance,
      N_Endpoint_Instance,
      N_Property_Instance,
      N_Actual,

      N_Clock_Event,

      --  Properties
      N_Always,
      N_Never,
      N_Eventually,
      N_Strong,          --  !
      N_Imp_Seq,         --  |=>
      N_Overlap_Imp_Seq, --  |->
      N_Log_Imp_Prop,    --  ->
      N_Log_Equiv_Prop,  --  <->
      N_Next,
      N_Next_A,
      N_Next_E,
      N_Next_Event,
      N_Next_Event_A,
      N_Next_Event_E,
      N_Abort,
      N_Async_Abort,
      N_Sync_Abort,
      N_Until,
      N_Before,
      N_Or_Prop,
      N_And_Prop,
      N_Paren_Prop,

      --  Sequences/SERE.
      N_Braced_SERE,
      N_Concat_SERE,
      N_Fusion_SERE,
      N_Within_SERE,
      N_Clocked_SERE,

      N_Match_And_Seq,  --  &&
      N_And_Seq,
      N_Or_Seq,

      N_Star_Repeat_Seq,
      N_Goto_Repeat_Seq,
      N_Plus_Repeat_Seq,   -- [+]
      N_Equal_Repeat_Seq,  -- [= ]

      --  Boolean layer.
      N_Paren_Bool,
      N_Not_Bool,
      N_And_Bool,
      N_Or_Bool,
      N_Imp_Bool,       -- ->
      N_Equiv_Bool,     -- <->
      N_HDL_Expr,
      N_HDL_Bool,
      N_False,
      N_True,
      N_EOS,

      N_Name,
      N_Name_Decl,
      N_Inf,
      N_Number
     );
   for Nkind'Size use 8;

   subtype N_Booleans is Nkind range N_Paren_Bool .. N_True;
   subtype N_Sequences is Nkind range N_Braced_SERE .. N_Equal_Repeat_Seq;

   subtype N_HDLs is Nkind range N_HDL_Expr .. N_HDL_Bool;

   type PSL_Types is
     (
      Type_Unknown,
      Type_Boolean,
      Type_Bit,
      Type_Bitvector,
      Type_Numeric,
      Type_String,
      Type_Sequence,
      Type_Property
     );

   --  Within CSE, it is useful to know which sub-expression already compose
   --  an expression.
   --  Eg: suppose we want to build A and B.
   --  Each sub-expressions of B is marked either as Present_Pos or
   --  Present_Neg.
   --  If A is already present, return either B or FALSE.
   --  Otherwise, build the node.
   type PSL_Presence_Kind is
     (
      Present_Unknown,
      Present_Pos,
      Present_Neg
     );

   --  The next line marks the start of the node description.
   -- Start of Nkind.

   -- N_Error (Short)

   -- N_Vmode (Short)
   -- N_Vunit (Short)
   -- N_Vprop (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Instance (Field3)
   --
   --   Get/Set_Item_Chain (Field4)

   -- N_Hdl_Mod_Name (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Prefix (Field2)

   -- N_Assert_Directive (Short)
   --
   --   Get/Set_Label (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_String (Field3)
   --
   --   Get/Set_Property (Field4)
   --
   --   Get/Set_NFA (Field5)

   -- N_Property_Declaration (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Global_Clock (Field3)
   --
   --   Get/Set_Property (Field4)
   --
   --   Get/Set_Parameter_List (Field5)

   -- N_Sequence_Declaration (Short)
   -- N_Endpoint_Declaration (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Sequence (Field3)
   --
   --   Get/Set_Parameter_List (Field5)

   -- N_Const_Parameter (Short)
   -- N_Boolean_Parameter (Short)
   -- N_Property_Parameter (Short)
   -- N_Sequence_Parameter (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)
   --
   --  --  Current actual parameter.
   --   Get/Set_Actual (Field3)

   -- N_Sequence_Instance (Short)
   -- N_Endpoint_Instance (Short)
   -- N_Property_Instance (Short)
   --
   --   Get/Set_Declaration (Field1)
   --
   --   Get/Set_Association_Chain (Field2)

   -- N_Actual (Short)
   --
   --   Get/Set_Chain (Field2)
   --
   --   Get/Set_Actual (Field3)
   --
   --   Get/Set_Formal (Field4)

   -- N_Clock_Event (Short)
   --
   --   Get/Set_Property (Field4)
   --
   --   Get/Set_Boolean (Field3)

   -- N_Always (Short)
   -- N_Never (Short)
   -- N_Eventually (Short)
   -- N_Strong (Short)
   --
   --   Get/Set_Property (Field4)

   -- N_Next (Short)
   --
   --   Get/Set_Strong_Flag (Flag1)
   --
   --   Get/Set_Number (Field1)
   --
   --   Get/Set_Property (Field4)

   -- N_Name (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Decl (Field2)

   -- N_Name_Decl (Short)
   --
   --   Get/Set_Identifier (Field1)
   --
   --   Get/Set_Chain (Field2)

   -- N_Inf (Short)

   -- N_Number (Short)
   --
   --   Get/Set_Value (Field1)

   -- N_Braced_SERE (Short)
   --
   --   Get/Set_SERE (Field1)

   -- N_Clocked_SERE (Short)
   --
   --   Get/Set_SERE (Field1)
   --
   --   Get/Set_Boolean (Field3)

   -- N_Concat_SERE (Short)
   -- N_Fusion_SERE (Short)
   -- N_Within_SERE (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field2)

   -- N_Star_Repeat_Seq (Short)
   --
   --  Note: can be null_node for star_repeat_seq.
   --   Get/Set_Sequence (Field3)
   --
   --   Get/Set_Low_Bound (Field1)
   --
   --   Get/Set_High_Bound (Field2)

   -- N_Equal_Repeat_Seq (Short)
   -- N_Goto_Repeat_Seq (Short)
   --
   --   Get/Set_Boolean (Field3)
   --
   --   Get/Set_Low_Bound (Field1)
   --
   --   Get/Set_High_Bound (Field2)

   -- N_Plus_Repeat_Seq (Short)
   --
   --  Note: can be null_node.
   --   Get/Set_Sequence (Field3)

   -- N_Match_And_Seq (Short)
   -- N_And_Seq (Short)
   -- N_Or_Seq (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field2)

   -- N_Imp_Seq (Short)
   -- N_Overlap_Imp_Seq (Short)
   --
   --   Get/Set_Sequence (Field3)
   --
   --   Get/Set_Property (Field4)

   -- N_Log_Imp_Prop (Short)
   -- N_Log_Equiv_Prop (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field2)

   -- N_Next_A (Short)
   -- N_Next_E (Short)
   --
   --   Get/Set_Strong_Flag (Flag1)
   --
   --   Get/Set_Low_Bound (Field1)
   --
   --   Get/Set_High_Bound (Field2)
   --
   --   Get/Set_Property (Field4)

   -- N_Next_Event (Short)
   --
   --   Get/Set_Strong_Flag (Flag1)
   --
   --   Get/Set_Number (Field1)
   --
   --   Get/Set_Property (Field4)
   --
   --   Get/Set_Boolean (Field3)

   -- N_Or_Prop (Short)
   -- N_And_Prop (Short)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field2)

   -- N_Paren_Prop (Short)
   --
   --   Get/Set_Property (Field4)

   -- N_Until (Short)
   -- N_Before (Short)
   --
   --   Get/Set_Strong_Flag (Flag1)
   --
   --   Get/Set_Inclusive_Flag (Flag2)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field2)

   -- N_Next_Event_A (Short)
   -- N_Next_Event_E (Short)
   --
   --   Get/Set_Strong_Flag (Flag1)
   --
   --   Get/Set_Low_Bound (Field1)
   --
   --   Get/Set_High_Bound (Field2)
   --
   --   Get/Set_Property (Field4)
   --
   --   Get/Set_Boolean (Field3)

   -- N_Abort (Short)
   -- N_Async_Abort (Short)
   -- N_Sync_Abort (Short)
   --
   --   Get/Set_Property (Field4)
   --
   --   Get/Set_Boolean (Field3)


   -- N_HDL_Bool (Short)
   --  An HDL expression of boolean type, that could be hashed.
   --
   --   Get/Set_Presence (State1)
   --
   --   Get/Set_HDL_Node (Field1)
   --
   --   Get/Set_HDL_Index (Field2)
   --
   --   Get/Set_Hash (Field5)
   --
   --   Get/Set_Hash_Link (Field6)

   -- N_HDL_Expr (Short)
   --  An HDL expression.  Just a proxy to the N_HDL_Bool.  The node
   --  is removed when rewritten.  This node is present so that denoting
   --  names are kept in the PSL tree.
   --
   --   Get/Set_HDL_Node (Field1)
   --
   --   Get/Set_HDL_Hash (Field5)

   -- N_Paren_Bool (Short)
   --
   --   Get/Set_Presence (State1)
   --
   --   Get/Set_Boolean (Field3)
   --
   --   Get/Set_Hash (Field5)
   --
   --   Get/Set_Hash_Link (Field6)

   -- N_Not_Bool (Short)
   --
   --   Get/Set_Presence (State1)
   --
   --   Get/Set_Boolean (Field3)
   --
   --   Get/Set_Hash (Field5)
   --
   --   Get/Set_Hash_Link (Field6)

   -- N_And_Bool (Short)
   -- N_Or_Bool (Short)
   -- N_Imp_Bool (Short)
   -- N_Equiv_Bool (Short)
   --
   --   Get/Set_Presence (State1)
   --
   --   Get/Set_Left (Field1)
   --
   --   Get/Set_Right (Field2)
   --
   --   Get/Set_Hash (Field5)
   --
   --   Get/Set_Hash_Link (Field6)

   -- N_True (Short)
   -- N_False (Short)

   -- N_EOS (Short)
   --  End of simulation.
   --
   --   Get/Set_HDL_Index (Field2)
   --
   --   Get/Set_Hash (Field5)
   --
   --   Get/Set_Hash_Link (Field6)

   -- End of Nkind.

   subtype Node is Types.PSL_Node;

   Null_Node  : constant Node := 0;
   False_Node : constant Node := 1;
   True_Node  : constant Node := 2;
   One_Node   : constant Node := 3;
   EOS_Node   : constant Node := 4;

   subtype NFA is Types.PSL_NFA;

   subtype HDL_Node is Int32;
   HDL_Null : constant HDL_Node := 0;

   -- General methods.

   procedure Init (Loc : Location_Type);

   --  Get the number of the last node.
   --  To be used to size lateral tables.
   function Get_Last_Node return Node;

   -- subtype Regs_Type_Node is Node range Reg_Type_Node .. Time_Type_Node;

   function Create_Node (Kind : Nkind) return Node;
   procedure Free_Node (N : Node);

   --  Return the type of a node.
   function Get_Psl_Type (N : Node) return PSL_Types;

   --  Note: use field Location
   function Get_Location (N : Node) return Location_Type;
   procedure Set_Location (N : Node; Loc : Location_Type);
   procedure Copy_Location (N : Node; Src : Node);

   function Get_Kind (N : Node) return Nkind;
   pragma Inline (Get_Kind);

--   --  Disp: None
--   --  Field: Field6
--   function Get_Parent (N : Node) return Node;
--   procedure Set_Parent (N : Node; Parent : Node);

   --  Disp: Special
   --  Field: Field1 (pos)
   function Get_Identifier (N : Node) return Name_Id;
   procedure Set_Identifier (N : Node; Id : Name_Id);

   --  Disp: Special
   --  Field: Field1 (pos)
   function Get_Label (N : Node) return Name_Id;
   procedure Set_Label (N : Node; Id : Name_Id);

   --  Disp: Chain
   --  Field: Field2 Chain
   function Get_Chain (N : Node) return Node;
   procedure Set_Chain (N : Node; Chain : Node);

   --  Field: Field3
   function Get_Instance (N : Node) return Node;
   procedure Set_Instance (N : Node; Instance : Node);

   --  Field: Field2
   function Get_Prefix (N : Node) return Node;
   procedure Set_Prefix (N : Node; Prefix : Node);

   --  Field: Field4
   function Get_Item_Chain (N : Node) return Node;
   procedure Set_Item_Chain (N : Node; Item : Node);

   --  Field: Field4
   function Get_Property (N : Node) return Node;
   procedure Set_Property (N : Node; Property : Node);

   --  Field: Field3
   function Get_String (N : Node) return Node;
   procedure Set_String (N : Node; Str : Node);

   --  Field: Field1
   function Get_SERE (N : Node) return Node;
   procedure Set_SERE (N : Node; S : Node);

   --  Field: Field1
   function Get_Left (N : Node) return Node;
   procedure Set_Left (N : Node; S : Node);

   --  Field: Field2
   function Get_Right (N : Node) return Node;
   procedure Set_Right (N : Node; S : Node);

   --  Field: Field3
   function Get_Sequence (N : Node) return Node;
   procedure Set_Sequence (N : Node; S : Node);

   --  Field: Flag1
   function Get_Strong_Flag (N : Node) return Boolean;
   procedure Set_Strong_Flag (N : Node; B : Boolean);

   --  Field: Flag2
   function Get_Inclusive_Flag (N : Node) return Boolean;
   procedure Set_Inclusive_Flag (N : Node; B : Boolean);

   --  Field: Field1
   function Get_Low_Bound (N : Node) return Node;
   procedure Set_Low_Bound (N : Node; S : Node);

   --  Field: Field2
   function Get_High_Bound (N : Node) return Node;
   procedure Set_High_Bound (N : Node; S : Node);

   --  Field: Field1
   function Get_Number (N : Node) return Node;
   procedure Set_Number (N : Node; S : Node);

   --  Field: Field1 (uc)
   function Get_Value (N : Node) return Uns32;
   procedure Set_Value (N : Node; Val : Uns32);

   --  Field: Field3
   function Get_Boolean (N : Node) return Node;
   procedure Set_Boolean (N : Node; B : Node);

   --  Field: Field2
   function Get_Decl (N : Node) return Node;
   procedure Set_Decl (N : Node; D : Node);

   --  Field: Field1 (uc)
   function Get_HDL_Node (N : Node) return HDL_Node;
   procedure Set_HDL_Node (N : Node; H : HDL_Node);

   --  Field: Field5 (uc)
   function Get_Hash (N : Node) return Uns32;
   procedure Set_Hash (N : Node; E : Uns32);
   pragma Inline (Get_Hash);

   --  Field: Field6
   function Get_Hash_Link (N : Node) return Node;
   procedure Set_Hash_Link (N : Node; E : Node);
   pragma Inline (Get_Hash_Link);

   --  Field: Field2 (uc)
   function Get_HDL_Index (N : Node) return Int32;
   procedure Set_HDL_Index (N : Node; Idx : Int32);

   --  Link the the hash-able node.
   --  Field: Field5
   function Get_HDL_Hash (N : Node) return Node;
   procedure Set_HDL_Hash (N : Node; H : Node);

   --  Field: State1 (pos)
   function Get_Presence (N : Node) return PSL_Presence_Kind;
   procedure Set_Presence (N : Node; P : PSL_Presence_Kind);

   --  Field: Field5 (uc)
   function Get_NFA (N : Node) return NFA;
   procedure Set_NFA (N : Node; P : NFA);

   --  Field: Field5
   function Get_Parameter_List (N : Node) return Node;
   procedure Set_Parameter_List (N : Node; E : Node);

   --  Field: Field3
   function Get_Actual (N : Node) return Node;
   procedure Set_Actual (N : Node; E : Node);

   --  Field: Field4
   function Get_Formal (N : Node) return Node;
   procedure Set_Formal (N : Node; E : Node);

   --  Field: Field1 Ref
   function Get_Declaration (N : Node) return Node;
   procedure Set_Declaration (N : Node; Decl : Node);

   --  Field: Field2
   function Get_Association_Chain (N : Node) return Node;
   procedure Set_Association_Chain (N : Node; Chain : Node);

   --  Field: Field3
   function Get_Global_Clock (N : Node) return Node;
   procedure Set_Global_Clock (N : Node; Clock : Node);
end PSL.Nodes;
