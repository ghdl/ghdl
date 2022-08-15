--  PSL - Nodes definition.  This is in fact -*- Ada -*-
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

with Ada.Unchecked_Conversion;
with Tables;
with PSL.Errors;
with PSL.Hash;
with PSL.Nodes_Meta; use PSL.Nodes_Meta;

package body PSL.Nodes is
   --  Suppress the access check of the table base.  This is really safe to
   --  suppress this check because the table base cannot be null.
   pragma Suppress (Access_Check);

   --  Suppress the index check on the table.
   --  Could be done during non-debug, since this may catch errors (reading
   --  Null_Node.
   --pragma Suppress (Index_Check);

   type Format_Type is
     (
      Format_Short
     );

   -- Common fields are:
   --   Flag1 : Boolean
   --   Flag2 : Boolean
   --   Flag3 : Boolean
   --   Flag4 : Boolean
   --   Flag5 : Boolean
   --   Flag6 : Boolean
   --   Nkind : Kind_Type
   --   State1 : Bit2_Type
   --   State2 : Bit2_Type
   --   Location : Int32
   --   Field1 : Node
   --   Field2 : Node
   --   Field3 : Node
   --   Field4 : Node

   -- Fields of Format_Short:
   --   Field5 : Node
   --   Field6 : Node

   type State_Type is range 0 .. 3;
   type Bit3_Type is range 0 .. 7;

   type Node_Record is record
      Kind : Nkind;
      Flag1 : Boolean;
      Flag2 : Boolean;
      Flag3 : Boolean;
      Flag4 : Boolean;
      Flag5 : Boolean;
      Flag6 : Boolean;
      Flag7 : Boolean;
      Flag8 : Boolean;
      Flag9 : Boolean;
      Flag10 : Boolean;
      Flag11 : Boolean;
      Flag12 : Boolean;
      Flag13 : Boolean;
      Flag14 : Boolean;
      Flag15 : Boolean;
      Flag16 : Boolean;
      State1 : State_Type;
      B3_1 : Bit3_Type;
      Flag17 : Boolean;
      Flag18 : Boolean;
      Flag19 : Boolean;

      Location : Int32;
      Field1 : Node;
      Field2 : Node;
      Field3 : Node;
      Field4 : Node;
      Field5 : Node;
      Field6 : Node;
   end record;
   pragma Pack (Node_Record);
   for Node_Record'Size use 8 * 32;

   package Nodet is new Tables
     (Table_Component_Type => Node_Record,
      Table_Index_Type => Node,
      Table_Low_Bound => 1,
      Table_Initial => 1024);

   Init_Node : constant Node_Record := (Kind => N_Error,
                                        Flag1 => False,
                                        Flag2 => False,
                                        State1 => 0,
                                        B3_1 => 0,
                                        Location => 0,
                                        Field1 => 0,
                                        Field2 => 0,
                                        Field3 => 0,
                                        Field4 => 0,
                                        Field5 => 0,
                                        Field6 => 0,
                                        others => False);

   Free_Nodes : Node := Null_Node;


   function Get_Last_Node return Node is
   begin
      return Nodet.Last;
   end Get_Last_Node;

   function Node_To_Uns32 is new Ada.Unchecked_Conversion
     (Source => Node, Target => Uns32);

   function Uns32_To_Node is new Ada.Unchecked_Conversion
     (Source => Uns32, Target => Node);

   function Node_To_Int32 is new Ada.Unchecked_Conversion
     (Source => Node, Target => Int32);

   function Int32_To_Node is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Node);

   function Node_To_NFA is new Ada.Unchecked_Conversion
     (Source => Node, Target => NFA);

   function NFA_To_Node is new Ada.Unchecked_Conversion
     (Source => NFA, Target => Node);

   function Node_To_HDL_Node is new Ada.Unchecked_Conversion
     (Source => Node, Target => HDL_Node);

   function HDL_Node_To_Node is new Ada.Unchecked_Conversion
     (Source => HDL_Node, Target => Node);

   procedure Set_Kind (N : Node; K : Nkind) is
   begin
      Nodet.Table (N).Kind := K;
   end Set_Kind;

   function Get_Kind (N : Node) return Nkind is
   begin
      return Nodet.Table (N).Kind;
   end Get_Kind;


   procedure Set_Flag1 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag1 := Flag;
   end Set_Flag1;

   function Get_Flag1 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag1;
   end Get_Flag1;

   procedure Set_Flag2 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag2 := Flag;
   end Set_Flag2;

   function Get_Flag2 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag2;
   end Get_Flag2;


   procedure Set_State1 (N : Node; S : State_Type) is
   begin
      Nodet.Table (N).State1 := S;
   end Set_State1;

   function Get_State1 (N : Node) return State_Type is
   begin
      return Nodet.Table (N).State1;
   end Get_State1;


   function Get_Location (N : Node) return Location_Type is
   begin
      return Location_Type (Nodet.Table (N).Location);
   end Get_Location;

   procedure Set_Location (N : Node; Loc : Location_Type) is
   begin
      Nodet.Table (N).Location := Int32 (Loc);
   end Set_Location;

   procedure Copy_Location (N : Node; Src : Node) is
   begin
      Set_Location (N, Get_Location (Src));
   end Copy_Location;

   procedure Set_Field1 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field1 := V;
   end Set_Field1;

   function Get_Field1 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field1;
   end Get_Field1;


   procedure Set_Field2 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field2 := V;
   end Set_Field2;

   function Get_Field2 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field2;
   end Get_Field2;


   function Get_Field3 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field3;
   end Get_Field3;

   procedure Set_Field3 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field3 := V;
   end Set_Field3;


   function Get_Field4 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field4;
   end Get_Field4;

   procedure Set_Field4 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field4 := V;
   end Set_Field4;


   function Get_Field5 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field5;
   end Get_Field5;

   procedure Set_Field5 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field5 := V;
   end Set_Field5;


   function Get_Field6 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field6;
   end Get_Field6;

   procedure Set_Field6 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field6 := V;
   end Set_Field6;


   function Get_Format (Kind : Nkind) return Format_Type;
   pragma Unreferenced (Get_Format);

   function Create_Node (Kind : Nkind) return Node
   is
      Res : Node;
   begin
      if Free_Nodes /= Null_Node then
         Res := Free_Nodes;
         Free_Nodes := Get_Field1 (Res);
      else
         Nodet.Increment_Last;
         Res := Nodet.Last;
      end if;
      Nodet.Table (Res) := Init_Node;
      Set_Kind (Res, Kind);
      return Res;
   end Create_Node;

   procedure Free_Node (N : Node)
   is
   begin
      Set_Kind (N, N_Error);
      Set_Field1 (N, Free_Nodes);
      Free_Nodes := N;
   end Free_Node;

   procedure Failed (Msg : String; N : Node)
   is
   begin
      Errors.Error_Kind (Msg, N);
   end Failed;

   procedure Init (Loc : Location_Type) is
   begin
      pragma Assert (Loc /= No_Location);
      Nodet.Init;

      if Create_Node (N_False) /= False_Node then
         raise Internal_Error;
      end if;
      Set_Location (False_Node, Loc);

      if Create_Node (N_True) /= True_Node then
         raise Internal_Error;
      end if;
      Set_Location (True_Node, Loc);

      if Create_Node (N_Number) /= One_Node then
         raise Internal_Error;
      end if;
      Set_Value (One_Node, 1);
      Set_Location (One_Node, Loc);

      if Create_Node (N_EOS) /= EOS_Node then
         raise Internal_Error;
      end if;
      Set_Hash (EOS_Node, 0);
      Set_Location (EOS_Node, Loc);
      PSL.Hash.Init;
   end Init;

   function Get_Psl_Type (N : Node) return PSL_Types is
   begin
      case Get_Kind (N) is
         when N_And_Prop
            | N_Or_Prop
            | N_Paren_Prop
            | N_Log_Imp_Prop
            | N_Log_Equiv_Prop
            | N_Overlap_Imp_Seq
            | N_Imp_Seq
            | N_Always
            | N_Never
            | N_Eventually
            | N_Next
            | N_Next_E
            | N_Next_A
            | N_Next_Event
            | N_Next_Event_A
            | N_Next_Event_E
            | N_Before
            | N_Until
            | N_Abort
            | N_Async_Abort
            | N_Sync_Abort
            | N_Strong
            | N_Property_Parameter
            | N_Property_Instance =>
            return Type_Property;
         when N_Braced_SERE
            | N_Concat_SERE
            | N_Fusion_SERE
            | N_Within_SERE
            | N_Clocked_SERE
            | N_And_Seq
            | N_Or_Seq
            | N_Match_And_Seq
            | N_Star_Repeat_Seq
            | N_Goto_Repeat_Seq
            | N_Equal_Repeat_Seq
            | N_Plus_Repeat_Seq
            | N_Clock_Event
            | N_Sequence_Instance
            | N_Endpoint_Instance
            | N_Sequence_Parameter =>
            return Type_Sequence;
         when N_Name =>
            return Get_Psl_Type (Get_Decl (N));
         when N_HDL_Expr =>
            --  FIXME.
            return Type_Boolean;
         when N_Or_Bool
            | N_And_Bool
            | N_Not_Bool
            | N_Imp_Bool
            | N_Equiv_Bool
            | N_False
            | N_True
            | N_Boolean_Parameter
            | N_Paren_Bool
            | N_HDL_Bool =>
            return Type_Boolean;
         when N_Number
            | N_Const_Parameter
            | N_Inf =>
            return Type_Numeric;
         when N_Vmode
            | N_Vunit
            | N_Vprop
            | N_Hdl_Mod_Name
            | N_Assert_Directive
            | N_Sequence_Declaration
            | N_Endpoint_Declaration
            | N_Property_Declaration
            | N_Actual
            | N_Name_Decl
            | N_Error
            | N_EOS =>
            PSL.Errors.Error_Kind ("get_psl_type", N);
      end case;
   end Get_Psl_Type;

   procedure Reference_Failed (Msg : String; N : Node) is
   begin
      Failed (Msg, N);
   end Reference_Failed;
   pragma Unreferenced (Reference_Failed);

   --  Subprograms
   function Get_Format (Kind : Nkind) return Format_Type is
   begin
      case Kind is
         when N_Error
           | N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Hdl_Mod_Name
           | N_Assert_Directive
           | N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Property_Instance
           | N_Actual
           | N_Clock_Event
           | N_Always
           | N_Never
           | N_Eventually
           | N_Strong
           | N_Imp_Seq
           | N_Overlap_Imp_Seq
           | N_Log_Imp_Prop
           | N_Log_Equiv_Prop
           | N_Next
           | N_Next_A
           | N_Next_E
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Abort
           | N_Async_Abort
           | N_Sync_Abort
           | N_Until
           | N_Before
           | N_Or_Prop
           | N_And_Prop
           | N_Paren_Prop
           | N_Braced_SERE
           | N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Clocked_SERE
           | N_Match_And_Seq
           | N_And_Seq
           | N_Or_Seq
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Plus_Repeat_Seq
           | N_Equal_Repeat_Seq
           | N_Paren_Bool
           | N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_Equiv_Bool
           | N_HDL_Expr
           | N_HDL_Bool
           | N_False
           | N_True
           | N_EOS
           | N_Name
           | N_Name_Decl
           | N_Inf
           | N_Number =>
            return Format_Short;
      end case;
   end Get_Format;

   function Get_Identifier (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Identifier (Get_Kind (N)),
                     "no field Identifier");
      return Name_Id'Val (Get_Field1 (N));
   end Get_Identifier;

   procedure Set_Identifier (N : Node; Id : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Identifier (Get_Kind (N)),
                     "no field Identifier");
      Set_Field1 (N, Name_Id'Pos (Id));
   end Set_Identifier;

   function Get_Label (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label (Get_Kind (N)),
                     "no field Label");
      return Name_Id'Val (Get_Field1 (N));
   end Get_Label;

   procedure Set_Label (N : Node; Id : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label (Get_Kind (N)),
                     "no field Label");
      Set_Field1 (N, Name_Id'Pos (Id));
   end Set_Label;

   function Get_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Chain (Get_Kind (N)),
                     "no field Chain");
      return Get_Field2 (N);
   end Get_Chain;

   procedure Set_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Chain (Get_Kind (N)),
                     "no field Chain");
      Set_Field2 (N, Chain);
   end Set_Chain;

   function Get_Instance (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance (Get_Kind (N)),
                     "no field Instance");
      return Get_Field3 (N);
   end Get_Instance;

   procedure Set_Instance (N : Node; Instance : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance (Get_Kind (N)),
                     "no field Instance");
      Set_Field3 (N, Instance);
   end Set_Instance;

   function Get_Prefix (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Prefix (Get_Kind (N)),
                     "no field Prefix");
      return Get_Field2 (N);
   end Get_Prefix;

   procedure Set_Prefix (N : Node; Prefix : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Prefix (Get_Kind (N)),
                     "no field Prefix");
      Set_Field2 (N, Prefix);
   end Set_Prefix;

   function Get_Item_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Item_Chain (Get_Kind (N)),
                     "no field Item_Chain");
      return Get_Field4 (N);
   end Get_Item_Chain;

   procedure Set_Item_Chain (N : Node; Item : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Item_Chain (Get_Kind (N)),
                     "no field Item_Chain");
      Set_Field4 (N, Item);
   end Set_Item_Chain;

   function Get_Property (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Property (Get_Kind (N)),
                     "no field Property");
      return Get_Field4 (N);
   end Get_Property;

   procedure Set_Property (N : Node; Property : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Property (Get_Kind (N)),
                     "no field Property");
      Set_Field4 (N, Property);
   end Set_Property;

   function Get_String (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String (Get_Kind (N)),
                     "no field String");
      return Get_Field3 (N);
   end Get_String;

   procedure Set_String (N : Node; Str : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String (Get_Kind (N)),
                     "no field String");
      Set_Field3 (N, Str);
   end Set_String;

   function Get_SERE (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_SERE (Get_Kind (N)),
                     "no field SERE");
      return Get_Field1 (N);
   end Get_SERE;

   procedure Set_SERE (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_SERE (Get_Kind (N)),
                     "no field SERE");
      Set_Field1 (N, S);
   end Set_SERE;

   function Get_Left (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Left (Get_Kind (N)),
                     "no field Left");
      return Get_Field1 (N);
   end Get_Left;

   procedure Set_Left (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Left (Get_Kind (N)),
                     "no field Left");
      Set_Field1 (N, S);
   end Set_Left;

   function Get_Right (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Right (Get_Kind (N)),
                     "no field Right");
      return Get_Field2 (N);
   end Get_Right;

   procedure Set_Right (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Right (Get_Kind (N)),
                     "no field Right");
      Set_Field2 (N, S);
   end Set_Right;

   function Get_Sequence (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Sequence (Get_Kind (N)),
                     "no field Sequence");
      return Get_Field3 (N);
   end Get_Sequence;

   procedure Set_Sequence (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Sequence (Get_Kind (N)),
                     "no field Sequence");
      Set_Field3 (N, S);
   end Set_Sequence;

   function Get_Strong_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Strong_Flag (Get_Kind (N)),
                     "no field Strong_Flag");
      return Get_Flag1 (N);
   end Get_Strong_Flag;

   procedure Set_Strong_Flag (N : Node; B : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Strong_Flag (Get_Kind (N)),
                     "no field Strong_Flag");
      Set_Flag1 (N, B);
   end Set_Strong_Flag;

   function Get_Inclusive_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Inclusive_Flag (Get_Kind (N)),
                     "no field Inclusive_Flag");
      return Get_Flag2 (N);
   end Get_Inclusive_Flag;

   procedure Set_Inclusive_Flag (N : Node; B : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Inclusive_Flag (Get_Kind (N)),
                     "no field Inclusive_Flag");
      Set_Flag2 (N, B);
   end Set_Inclusive_Flag;

   function Get_Has_Identifier_List (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (N)),
                     "no field Has_Identifier_List");
      return Get_Flag1 (N);
   end Get_Has_Identifier_List;

   procedure Set_Has_Identifier_List (N : Node; B : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (N)),
                     "no field Has_Identifier_List");
      Set_Flag1 (N, B);
   end Set_Has_Identifier_List;

   function Get_Low_Bound (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Low_Bound (Get_Kind (N)),
                     "no field Low_Bound");
      return Get_Field1 (N);
   end Get_Low_Bound;

   procedure Set_Low_Bound (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Low_Bound (Get_Kind (N)),
                     "no field Low_Bound");
      Set_Field1 (N, S);
   end Set_Low_Bound;

   function Get_High_Bound (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_High_Bound (Get_Kind (N)),
                     "no field High_Bound");
      return Get_Field2 (N);
   end Get_High_Bound;

   procedure Set_High_Bound (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_High_Bound (Get_Kind (N)),
                     "no field High_Bound");
      Set_Field2 (N, S);
   end Set_High_Bound;

   function Get_Number (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number (Get_Kind (N)),
                     "no field Number");
      return Get_Field1 (N);
   end Get_Number;

   procedure Set_Number (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number (Get_Kind (N)),
                     "no field Number");
      Set_Field1 (N, S);
   end Set_Number;

   function Get_Value (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Value (Get_Kind (N)),
                     "no field Value");
      return Node_To_Uns32 (Get_Field1 (N));
   end Get_Value;

   procedure Set_Value (N : Node; Val : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Value (Get_Kind (N)),
                     "no field Value");
      Set_Field1 (N, Uns32_To_Node (Val));
   end Set_Value;

   function Get_Boolean (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Boolean (Get_Kind (N)),
                     "no field Boolean");
      return Get_Field3 (N);
   end Get_Boolean;

   procedure Set_Boolean (N : Node; B : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Boolean (Get_Kind (N)),
                     "no field Boolean");
      Set_Field3 (N, B);
   end Set_Boolean;

   function Get_Decl (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Decl (Get_Kind (N)),
                     "no field Decl");
      return Get_Field2 (N);
   end Get_Decl;

   procedure Set_Decl (N : Node; D : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Decl (Get_Kind (N)),
                     "no field Decl");
      Set_Field2 (N, D);
   end Set_Decl;

   function Get_HDL_Node (N : Node) return HDL_Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_HDL_Node (Get_Kind (N)),
                     "no field HDL_Node");
      return Node_To_HDL_Node (Get_Field1 (N));
   end Get_HDL_Node;

   procedure Set_HDL_Node (N : Node; H : HDL_Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_HDL_Node (Get_Kind (N)),
                     "no field HDL_Node");
      Set_Field1 (N, HDL_Node_To_Node (H));
   end Set_HDL_Node;

   function Get_Hash (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Hash (Get_Kind (N)),
                     "no field Hash");
      return Node_To_Uns32 (Get_Field5 (N));
   end Get_Hash;

   procedure Set_Hash (N : Node; E : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Hash (Get_Kind (N)),
                     "no field Hash");
      Set_Field5 (N, Uns32_To_Node (E));
   end Set_Hash;

   function Get_Hash_Link (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Hash_Link (Get_Kind (N)),
                     "no field Hash_Link");
      return Get_Field6 (N);
   end Get_Hash_Link;

   procedure Set_Hash_Link (N : Node; E : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Hash_Link (Get_Kind (N)),
                     "no field Hash_Link");
      Set_Field6 (N, E);
   end Set_Hash_Link;

   function Get_HDL_Index (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_HDL_Index (Get_Kind (N)),
                     "no field HDL_Index");
      return Node_To_Int32 (Get_Field2 (N));
   end Get_HDL_Index;

   procedure Set_HDL_Index (N : Node; Idx : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_HDL_Index (Get_Kind (N)),
                     "no field HDL_Index");
      Set_Field2 (N, Int32_To_Node (Idx));
   end Set_HDL_Index;

   function Get_HDL_Hash (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_HDL_Hash (Get_Kind (N)),
                     "no field HDL_Hash");
      return Get_Field5 (N);
   end Get_HDL_Hash;

   procedure Set_HDL_Hash (N : Node; H : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_HDL_Hash (Get_Kind (N)),
                     "no field HDL_Hash");
      Set_Field5 (N, H);
   end Set_HDL_Hash;

   function Get_Presence (N : Node) return PSL_Presence_Kind is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Presence (Get_Kind (N)),
                     "no field Presence");
      return PSL_Presence_Kind'Val (Get_State1 (N));
   end Get_Presence;

   procedure Set_Presence (N : Node; P : PSL_Presence_Kind) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Presence (Get_Kind (N)),
                     "no field Presence");
      Set_State1 (N, PSL_Presence_Kind'Pos (P));
   end Set_Presence;

   function Get_NFA (N : Node) return NFA is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_NFA (Get_Kind (N)),
                     "no field NFA");
      return Node_To_NFA (Get_Field5 (N));
   end Get_NFA;

   procedure Set_NFA (N : Node; P : NFA) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_NFA (Get_Kind (N)),
                     "no field NFA");
      Set_Field5 (N, NFA_To_Node (P));
   end Set_NFA;

   function Get_Parameter_List (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_List (Get_Kind (N)),
                     "no field Parameter_List");
      return Get_Field5 (N);
   end Get_Parameter_List;

   procedure Set_Parameter_List (N : Node; E : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_List (Get_Kind (N)),
                     "no field Parameter_List");
      Set_Field5 (N, E);
   end Set_Parameter_List;

   function Get_Actual (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Actual (Get_Kind (N)),
                     "no field Actual");
      return Get_Field3 (N);
   end Get_Actual;

   procedure Set_Actual (N : Node; E : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Actual (Get_Kind (N)),
                     "no field Actual");
      Set_Field3 (N, E);
   end Set_Actual;

   function Get_Formal (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Formal (Get_Kind (N)),
                     "no field Formal");
      return Get_Field4 (N);
   end Get_Formal;

   procedure Set_Formal (N : Node; E : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Formal (Get_Kind (N)),
                     "no field Formal");
      Set_Field4 (N, E);
   end Set_Formal;

   function Get_Declaration (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Declaration (Get_Kind (N)),
                     "no field Declaration");
      return Get_Field1 (N);
   end Get_Declaration;

   procedure Set_Declaration (N : Node; Decl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Declaration (Get_Kind (N)),
                     "no field Declaration");
      Set_Field1 (N, Decl);
   end Set_Declaration;

   function Get_Association_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Association_Chain (Get_Kind (N)),
                     "no field Association_Chain");
      return Get_Field2 (N);
   end Get_Association_Chain;

   procedure Set_Association_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Association_Chain (Get_Kind (N)),
                     "no field Association_Chain");
      Set_Field2 (N, Chain);
   end Set_Association_Chain;

   function Get_Global_Clock (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Global_Clock (Get_Kind (N)),
                     "no field Global_Clock");
      return Get_Field3 (N);
   end Get_Global_Clock;

   procedure Set_Global_Clock (N : Node; Clock : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Global_Clock (Get_Kind (N)),
                     "no field Global_Clock");
      Set_Field3 (N, Clock);
   end Set_Global_Clock;


end PSL.Nodes;
