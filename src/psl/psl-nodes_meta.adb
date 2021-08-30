--  Meta description of nodes.
--  Copyright (C) 2015 Tristan Gingold
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

package body PSL.Nodes_Meta is
   Fields_Type : constant array (Fields_Enum) of Types_Enum :=
     (
      Field_Identifier => Type_Name_Id,
      Field_Label => Type_Name_Id,
      Field_Chain => Type_Node,
      Field_Instance => Type_Node,
      Field_Prefix => Type_Node,
      Field_Item_Chain => Type_Node,
      Field_Property => Type_Node,
      Field_String => Type_Node,
      Field_SERE => Type_Node,
      Field_Left => Type_Node,
      Field_Right => Type_Node,
      Field_Sequence => Type_Node,
      Field_Strong_Flag => Type_Boolean,
      Field_Inclusive_Flag => Type_Boolean,
      Field_Low_Bound => Type_Node,
      Field_High_Bound => Type_Node,
      Field_Number => Type_Node,
      Field_Value => Type_Uns32,
      Field_Boolean => Type_Node,
      Field_Decl => Type_Node,
      Field_HDL_Node => Type_HDL_Node,
      Field_Hash => Type_Uns32,
      Field_Hash_Link => Type_Node,
      Field_HDL_Index => Type_Int32,
      Field_HDL_Hash => Type_Node,
      Field_Presence => Type_PSL_Presence_Kind,
      Field_NFA => Type_NFA,
      Field_Parameter_List => Type_Node,
      Field_Actual => Type_Node,
      Field_Formal => Type_Node,
      Field_Declaration => Type_Node,
      Field_Association_Chain => Type_Node,
      Field_Global_Clock => Type_Node
     );

   function Get_Field_Type (F : Fields_Enum) return Types_Enum is
   begin
      return Fields_Type (F);
   end Get_Field_Type;

   function Get_Field_Image (F : Fields_Enum) return String is
   begin
      case F is
         when Field_Identifier =>
            return "identifier";
         when Field_Label =>
            return "label";
         when Field_Chain =>
            return "chain";
         when Field_Instance =>
            return "instance";
         when Field_Prefix =>
            return "prefix";
         when Field_Item_Chain =>
            return "item_chain";
         when Field_Property =>
            return "property";
         when Field_String =>
            return "string";
         when Field_SERE =>
            return "sere";
         when Field_Left =>
            return "left";
         when Field_Right =>
            return "right";
         when Field_Sequence =>
            return "sequence";
         when Field_Strong_Flag =>
            return "strong_flag";
         when Field_Inclusive_Flag =>
            return "inclusive_flag";
         when Field_Low_Bound =>
            return "low_bound";
         when Field_High_Bound =>
            return "high_bound";
         when Field_Number =>
            return "number";
         when Field_Value =>
            return "value";
         when Field_Boolean =>
            return "boolean";
         when Field_Decl =>
            return "decl";
         when Field_HDL_Node =>
            return "hdl_node";
         when Field_Hash =>
            return "hash";
         when Field_Hash_Link =>
            return "hash_link";
         when Field_HDL_Index =>
            return "hdl_index";
         when Field_HDL_Hash =>
            return "hdl_hash";
         when Field_Presence =>
            return "presence";
         when Field_NFA =>
            return "nfa";
         when Field_Parameter_List =>
            return "parameter_list";
         when Field_Actual =>
            return "actual";
         when Field_Formal =>
            return "formal";
         when Field_Declaration =>
            return "declaration";
         when Field_Association_Chain =>
            return "association_chain";
         when Field_Global_Clock =>
            return "global_clock";
      end case;
   end Get_Field_Image;

   function Get_Nkind_Image (K : Nkind) return String is
   begin
      case K is
         when N_Error =>
            return "error";
         when N_Vmode =>
            return "vmode";
         when N_Vunit =>
            return "vunit";
         when N_Vprop =>
            return "vprop";
         when N_Hdl_Mod_Name =>
            return "hdl_mod_name";
         when N_Assert_Directive =>
            return "assert_directive";
         when N_Property_Declaration =>
            return "property_declaration";
         when N_Sequence_Declaration =>
            return "sequence_declaration";
         when N_Endpoint_Declaration =>
            return "endpoint_declaration";
         when N_Const_Parameter =>
            return "const_parameter";
         when N_Boolean_Parameter =>
            return "boolean_parameter";
         when N_Property_Parameter =>
            return "property_parameter";
         when N_Sequence_Parameter =>
            return "sequence_parameter";
         when N_Sequence_Instance =>
            return "sequence_instance";
         when N_Endpoint_Instance =>
            return "endpoint_instance";
         when N_Property_Instance =>
            return "property_instance";
         when N_Actual =>
            return "actual";
         when N_Clock_Event =>
            return "clock_event";
         when N_Always =>
            return "always";
         when N_Never =>
            return "never";
         when N_Eventually =>
            return "eventually";
         when N_Strong =>
            return "strong";
         when N_Imp_Seq =>
            return "imp_seq";
         when N_Overlap_Imp_Seq =>
            return "overlap_imp_seq";
         when N_Log_Imp_Prop =>
            return "log_imp_prop";
         when N_Log_Equiv_Prop =>
            return "log_equiv_prop";
         when N_Next =>
            return "next";
         when N_Next_A =>
            return "next_a";
         when N_Next_E =>
            return "next_e";
         when N_Next_Event =>
            return "next_event";
         when N_Next_Event_A =>
            return "next_event_a";
         when N_Next_Event_E =>
            return "next_event_e";
         when N_Abort =>
            return "abort";
         when N_Async_Abort =>
            return "async_abort";
         when N_Sync_Abort =>
            return "sync_abort";
         when N_Until =>
            return "until";
         when N_Before =>
            return "before";
         when N_Or_Prop =>
            return "or_prop";
         when N_And_Prop =>
            return "and_prop";
         when N_Paren_Prop =>
            return "paren_prop";
         when N_Braced_SERE =>
            return "braced_sere";
         when N_Concat_SERE =>
            return "concat_sere";
         when N_Fusion_SERE =>
            return "fusion_sere";
         when N_Within_SERE =>
            return "within_sere";
         when N_Clocked_SERE =>
            return "clocked_sere";
         when N_Match_And_Seq =>
            return "match_and_seq";
         when N_And_Seq =>
            return "and_seq";
         when N_Or_Seq =>
            return "or_seq";
         when N_Star_Repeat_Seq =>
            return "star_repeat_seq";
         when N_Goto_Repeat_Seq =>
            return "goto_repeat_seq";
         when N_Plus_Repeat_Seq =>
            return "plus_repeat_seq";
         when N_Equal_Repeat_Seq =>
            return "equal_repeat_seq";
         when N_Paren_Bool =>
            return "paren_bool";
         when N_Not_Bool =>
            return "not_bool";
         when N_And_Bool =>
            return "and_bool";
         when N_Or_Bool =>
            return "or_bool";
         when N_Imp_Bool =>
            return "imp_bool";
         when N_Equiv_Bool =>
            return "equiv_bool";
         when N_HDL_Expr =>
            return "hdl_expr";
         when N_HDL_Bool =>
            return "hdl_bool";
         when N_False =>
            return "false";
         when N_True =>
            return "true";
         when N_EOS =>
            return "eos";
         when N_Name =>
            return "name";
         when N_Name_Decl =>
            return "name_decl";
         when N_Inf =>
            return "inf";
         when N_Number =>
            return "number";
      end case;
   end Get_Nkind_Image;

   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute is
   begin
      case F is
         when Field_Identifier =>
            return Attr_None;
         when Field_Label =>
            return Attr_None;
         when Field_Chain =>
            return Attr_Chain;
         when Field_Instance =>
            return Attr_None;
         when Field_Prefix =>
            return Attr_None;
         when Field_Item_Chain =>
            return Attr_None;
         when Field_Property =>
            return Attr_None;
         when Field_String =>
            return Attr_None;
         when Field_SERE =>
            return Attr_None;
         when Field_Left =>
            return Attr_None;
         when Field_Right =>
            return Attr_None;
         when Field_Sequence =>
            return Attr_None;
         when Field_Strong_Flag =>
            return Attr_None;
         when Field_Inclusive_Flag =>
            return Attr_None;
         when Field_Low_Bound =>
            return Attr_None;
         when Field_High_Bound =>
            return Attr_None;
         when Field_Number =>
            return Attr_None;
         when Field_Value =>
            return Attr_None;
         when Field_Boolean =>
            return Attr_None;
         when Field_Decl =>
            return Attr_None;
         when Field_HDL_Node =>
            return Attr_None;
         when Field_Hash =>
            return Attr_None;
         when Field_Hash_Link =>
            return Attr_None;
         when Field_HDL_Index =>
            return Attr_None;
         when Field_HDL_Hash =>
            return Attr_None;
         when Field_Presence =>
            return Attr_None;
         when Field_NFA =>
            return Attr_None;
         when Field_Parameter_List =>
            return Attr_None;
         when Field_Actual =>
            return Attr_None;
         when Field_Formal =>
            return Attr_None;
         when Field_Declaration =>
            return Attr_Ref;
         when Field_Association_Chain =>
            return Attr_None;
         when Field_Global_Clock =>
            return Attr_None;
      end case;
   end Get_Field_Attribute;

   Fields_Of_Nodes : constant Fields_Array :=
     (
      --  N_Error
      --  N_Vmode
      Field_Identifier,
      Field_Chain,
      Field_Instance,
      Field_Item_Chain,
      --  N_Vunit
      Field_Identifier,
      Field_Chain,
      Field_Instance,
      Field_Item_Chain,
      --  N_Vprop
      Field_Identifier,
      Field_Chain,
      Field_Instance,
      Field_Item_Chain,
      --  N_Hdl_Mod_Name
      Field_Identifier,
      Field_Prefix,
      --  N_Assert_Directive
      Field_Label,
      Field_NFA,
      Field_Chain,
      Field_String,
      Field_Property,
      --  N_Property_Declaration
      Field_Identifier,
      Field_Chain,
      Field_Global_Clock,
      Field_Property,
      Field_Parameter_List,
      --  N_Sequence_Declaration
      Field_Identifier,
      Field_Chain,
      Field_Sequence,
      Field_Parameter_List,
      --  N_Endpoint_Declaration
      Field_Identifier,
      Field_Chain,
      Field_Sequence,
      Field_Parameter_List,
      --  N_Const_Parameter
      Field_Identifier,
      Field_Chain,
      Field_Actual,
      --  N_Boolean_Parameter
      Field_Identifier,
      Field_Chain,
      Field_Actual,
      --  N_Property_Parameter
      Field_Identifier,
      Field_Chain,
      Field_Actual,
      --  N_Sequence_Parameter
      Field_Identifier,
      Field_Chain,
      Field_Actual,
      --  N_Sequence_Instance
      Field_Declaration,
      Field_Association_Chain,
      --  N_Endpoint_Instance
      Field_Declaration,
      Field_Association_Chain,
      --  N_Property_Instance
      Field_Declaration,
      Field_Association_Chain,
      --  N_Actual
      Field_Chain,
      Field_Actual,
      Field_Formal,
      --  N_Clock_Event
      Field_Property,
      Field_Boolean,
      --  N_Always
      Field_Property,
      --  N_Never
      Field_Property,
      --  N_Eventually
      Field_Property,
      --  N_Strong
      Field_Property,
      --  N_Imp_Seq
      Field_Sequence,
      Field_Property,
      --  N_Overlap_Imp_Seq
      Field_Sequence,
      Field_Property,
      --  N_Log_Imp_Prop
      Field_Left,
      Field_Right,
      --  N_Log_Equiv_Prop
      Field_Left,
      Field_Right,
      --  N_Next
      Field_Strong_Flag,
      Field_Number,
      Field_Property,
      --  N_Next_A
      Field_Strong_Flag,
      Field_Low_Bound,
      Field_High_Bound,
      Field_Property,
      --  N_Next_E
      Field_Strong_Flag,
      Field_Low_Bound,
      Field_High_Bound,
      Field_Property,
      --  N_Next_Event
      Field_Strong_Flag,
      Field_Number,
      Field_Property,
      Field_Boolean,
      --  N_Next_Event_A
      Field_Strong_Flag,
      Field_Low_Bound,
      Field_High_Bound,
      Field_Property,
      Field_Boolean,
      --  N_Next_Event_E
      Field_Strong_Flag,
      Field_Low_Bound,
      Field_High_Bound,
      Field_Property,
      Field_Boolean,
      --  N_Abort
      Field_Property,
      Field_Boolean,
      --  N_Async_Abort
      Field_Property,
      Field_Boolean,
      --  N_Sync_Abort
      Field_Property,
      Field_Boolean,
      --  N_Until
      Field_Strong_Flag,
      Field_Inclusive_Flag,
      Field_Left,
      Field_Right,
      --  N_Before
      Field_Strong_Flag,
      Field_Inclusive_Flag,
      Field_Left,
      Field_Right,
      --  N_Or_Prop
      Field_Left,
      Field_Right,
      --  N_And_Prop
      Field_Left,
      Field_Right,
      --  N_Paren_Prop
      Field_Property,
      --  N_Braced_SERE
      Field_SERE,
      --  N_Concat_SERE
      Field_Left,
      Field_Right,
      --  N_Fusion_SERE
      Field_Left,
      Field_Right,
      --  N_Within_SERE
      Field_Left,
      Field_Right,
      --  N_Clocked_SERE
      Field_SERE,
      Field_Boolean,
      --  N_Match_And_Seq
      Field_Left,
      Field_Right,
      --  N_And_Seq
      Field_Left,
      Field_Right,
      --  N_Or_Seq
      Field_Left,
      Field_Right,
      --  N_Star_Repeat_Seq
      Field_Sequence,
      Field_Low_Bound,
      Field_High_Bound,
      --  N_Goto_Repeat_Seq
      Field_Boolean,
      Field_Low_Bound,
      Field_High_Bound,
      --  N_Plus_Repeat_Seq
      Field_Sequence,
      --  N_Equal_Repeat_Seq
      Field_Boolean,
      Field_Low_Bound,
      Field_High_Bound,
      --  N_Paren_Bool
      Field_Hash,
      Field_Presence,
      Field_Boolean,
      Field_Hash_Link,
      --  N_Not_Bool
      Field_Hash,
      Field_Presence,
      Field_Boolean,
      Field_Hash_Link,
      --  N_And_Bool
      Field_Hash,
      Field_Presence,
      Field_Left,
      Field_Right,
      Field_Hash_Link,
      --  N_Or_Bool
      Field_Hash,
      Field_Presence,
      Field_Left,
      Field_Right,
      Field_Hash_Link,
      --  N_Imp_Bool
      Field_Hash,
      Field_Presence,
      Field_Left,
      Field_Right,
      Field_Hash_Link,
      --  N_Equiv_Bool
      Field_Hash,
      Field_Presence,
      Field_Left,
      Field_Right,
      Field_Hash_Link,
      --  N_HDL_Expr
      Field_HDL_Node,
      Field_HDL_Hash,
      --  N_HDL_Bool
      Field_HDL_Node,
      Field_HDL_Index,
      Field_Hash,
      Field_Presence,
      Field_Hash_Link,
      --  N_False
      --  N_True
      --  N_EOS
      Field_HDL_Index,
      Field_Hash,
      Field_Hash_Link,
      --  N_Name
      Field_Identifier,
      Field_Decl,
      --  N_Name_Decl
      Field_Identifier,
      Field_Chain,
      --  N_Inf
      --  N_Number
      Field_Value
     );

   Fields_Of_Nodes_Last : constant array (Nkind) of Integer :=
     (
      N_Error => -1,
      N_Vmode => 3,
      N_Vunit => 7,
      N_Vprop => 11,
      N_Hdl_Mod_Name => 13,
      N_Assert_Directive => 18,
      N_Property_Declaration => 23,
      N_Sequence_Declaration => 27,
      N_Endpoint_Declaration => 31,
      N_Const_Parameter => 34,
      N_Boolean_Parameter => 37,
      N_Property_Parameter => 40,
      N_Sequence_Parameter => 43,
      N_Sequence_Instance => 45,
      N_Endpoint_Instance => 47,
      N_Property_Instance => 49,
      N_Actual => 52,
      N_Clock_Event => 54,
      N_Always => 55,
      N_Never => 56,
      N_Eventually => 57,
      N_Strong => 58,
      N_Imp_Seq => 60,
      N_Overlap_Imp_Seq => 62,
      N_Log_Imp_Prop => 64,
      N_Log_Equiv_Prop => 66,
      N_Next => 69,
      N_Next_A => 73,
      N_Next_E => 77,
      N_Next_Event => 81,
      N_Next_Event_A => 86,
      N_Next_Event_E => 91,
      N_Abort => 93,
      N_Async_Abort => 95,
      N_Sync_Abort => 97,
      N_Until => 101,
      N_Before => 105,
      N_Or_Prop => 107,
      N_And_Prop => 109,
      N_Paren_Prop => 110,
      N_Braced_SERE => 111,
      N_Concat_SERE => 113,
      N_Fusion_SERE => 115,
      N_Within_SERE => 117,
      N_Clocked_SERE => 119,
      N_Match_And_Seq => 121,
      N_And_Seq => 123,
      N_Or_Seq => 125,
      N_Star_Repeat_Seq => 128,
      N_Goto_Repeat_Seq => 131,
      N_Plus_Repeat_Seq => 132,
      N_Equal_Repeat_Seq => 135,
      N_Paren_Bool => 139,
      N_Not_Bool => 143,
      N_And_Bool => 148,
      N_Or_Bool => 153,
      N_Imp_Bool => 158,
      N_Equiv_Bool => 163,
      N_HDL_Expr => 165,
      N_HDL_Bool => 170,
      N_False => 170,
      N_True => 170,
      N_EOS => 173,
      N_Name => 175,
      N_Name_Decl => 177,
      N_Inf => 177,
      N_Number => 178
     );

   function Get_Fields (K : Nkind) return Fields_Array
   is
      First : Natural;
      Last : Integer;
   begin
      if K = Nkind'First then
         First := Fields_Of_Nodes'First;
      else
         First := Fields_Of_Nodes_Last (Nkind'Pred (K)) + 1;
      end if;
      Last := Fields_Of_Nodes_Last (K);
      return Fields_Of_Nodes (First .. Last);
   end Get_Fields;

   function Get_Boolean
      (N : Node; F : Fields_Enum) return Boolean is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Strong_Flag =>
            return Get_Strong_Flag (N);
         when Field_Inclusive_Flag =>
            return Get_Inclusive_Flag (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Boolean;

   procedure Set_Boolean
      (N : Node; F : Fields_Enum; V: Boolean) is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Strong_Flag =>
            Set_Strong_Flag (N, V);
         when Field_Inclusive_Flag =>
            Set_Inclusive_Flag (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Boolean;

   function Get_HDL_Node
      (N : Node; F : Fields_Enum) return HDL_Node is
   begin
      pragma Assert (Fields_Type (F) = Type_HDL_Node);
      case F is
         when Field_HDL_Node =>
            return Get_HDL_Node (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_HDL_Node;

   procedure Set_HDL_Node
      (N : Node; F : Fields_Enum; V: HDL_Node) is
   begin
      pragma Assert (Fields_Type (F) = Type_HDL_Node);
      case F is
         when Field_HDL_Node =>
            Set_HDL_Node (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_HDL_Node;

   function Get_Int32
      (N : Node; F : Fields_Enum) return Int32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_HDL_Index =>
            return Get_HDL_Index (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Int32;

   procedure Set_Int32
      (N : Node; F : Fields_Enum; V: Int32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_HDL_Index =>
            Set_HDL_Index (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Int32;

   function Get_NFA
      (N : Node; F : Fields_Enum) return NFA is
   begin
      pragma Assert (Fields_Type (F) = Type_NFA);
      case F is
         when Field_NFA =>
            return Get_NFA (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_NFA;

   procedure Set_NFA
      (N : Node; F : Fields_Enum; V: NFA) is
   begin
      pragma Assert (Fields_Type (F) = Type_NFA);
      case F is
         when Field_NFA =>
            Set_NFA (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_NFA;

   function Get_Name_Id
      (N : Node; F : Fields_Enum) return Name_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Identifier =>
            return Get_Identifier (N);
         when Field_Label =>
            return Get_Label (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Name_Id;

   procedure Set_Name_Id
      (N : Node; F : Fields_Enum; V: Name_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Identifier =>
            Set_Identifier (N, V);
         when Field_Label =>
            Set_Label (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Name_Id;

   function Get_Node
      (N : Node; F : Fields_Enum) return Node is
   begin
      pragma Assert (Fields_Type (F) = Type_Node);
      case F is
         when Field_Chain =>
            return Get_Chain (N);
         when Field_Instance =>
            return Get_Instance (N);
         when Field_Prefix =>
            return Get_Prefix (N);
         when Field_Item_Chain =>
            return Get_Item_Chain (N);
         when Field_Property =>
            return Get_Property (N);
         when Field_String =>
            return Get_String (N);
         when Field_SERE =>
            return Get_SERE (N);
         when Field_Left =>
            return Get_Left (N);
         when Field_Right =>
            return Get_Right (N);
         when Field_Sequence =>
            return Get_Sequence (N);
         when Field_Low_Bound =>
            return Get_Low_Bound (N);
         when Field_High_Bound =>
            return Get_High_Bound (N);
         when Field_Number =>
            return Get_Number (N);
         when Field_Boolean =>
            return Get_Boolean (N);
         when Field_Decl =>
            return Get_Decl (N);
         when Field_Hash_Link =>
            return Get_Hash_Link (N);
         when Field_HDL_Hash =>
            return Get_HDL_Hash (N);
         when Field_Parameter_List =>
            return Get_Parameter_List (N);
         when Field_Actual =>
            return Get_Actual (N);
         when Field_Formal =>
            return Get_Formal (N);
         when Field_Declaration =>
            return Get_Declaration (N);
         when Field_Association_Chain =>
            return Get_Association_Chain (N);
         when Field_Global_Clock =>
            return Get_Global_Clock (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Node;

   procedure Set_Node
      (N : Node; F : Fields_Enum; V: Node) is
   begin
      pragma Assert (Fields_Type (F) = Type_Node);
      case F is
         when Field_Chain =>
            Set_Chain (N, V);
         when Field_Instance =>
            Set_Instance (N, V);
         when Field_Prefix =>
            Set_Prefix (N, V);
         when Field_Item_Chain =>
            Set_Item_Chain (N, V);
         when Field_Property =>
            Set_Property (N, V);
         when Field_String =>
            Set_String (N, V);
         when Field_SERE =>
            Set_SERE (N, V);
         when Field_Left =>
            Set_Left (N, V);
         when Field_Right =>
            Set_Right (N, V);
         when Field_Sequence =>
            Set_Sequence (N, V);
         when Field_Low_Bound =>
            Set_Low_Bound (N, V);
         when Field_High_Bound =>
            Set_High_Bound (N, V);
         when Field_Number =>
            Set_Number (N, V);
         when Field_Boolean =>
            Set_Boolean (N, V);
         when Field_Decl =>
            Set_Decl (N, V);
         when Field_Hash_Link =>
            Set_Hash_Link (N, V);
         when Field_HDL_Hash =>
            Set_HDL_Hash (N, V);
         when Field_Parameter_List =>
            Set_Parameter_List (N, V);
         when Field_Actual =>
            Set_Actual (N, V);
         when Field_Formal =>
            Set_Formal (N, V);
         when Field_Declaration =>
            Set_Declaration (N, V);
         when Field_Association_Chain =>
            Set_Association_Chain (N, V);
         when Field_Global_Clock =>
            Set_Global_Clock (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Node;

   function Get_PSL_Presence_Kind
      (N : Node; F : Fields_Enum) return PSL_Presence_Kind is
   begin
      pragma Assert (Fields_Type (F) = Type_PSL_Presence_Kind);
      case F is
         when Field_Presence =>
            return Get_Presence (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_PSL_Presence_Kind;

   procedure Set_PSL_Presence_Kind
      (N : Node; F : Fields_Enum; V: PSL_Presence_Kind) is
   begin
      pragma Assert (Fields_Type (F) = Type_PSL_Presence_Kind);
      case F is
         when Field_Presence =>
            Set_Presence (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_PSL_Presence_Kind;

   function Get_Uns32
      (N : Node; F : Fields_Enum) return Uns32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Uns32);
      case F is
         when Field_Value =>
            return Get_Value (N);
         when Field_Hash =>
            return Get_Hash (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Uns32;

   procedure Set_Uns32
      (N : Node; F : Fields_Enum; V: Uns32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Uns32);
      case F is
         when Field_Value =>
            Set_Value (N, V);
         when Field_Hash =>
            Set_Hash (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Uns32;

   function Has_Identifier (K : Nkind) return Boolean is
   begin
      case K is
         when N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Hdl_Mod_Name
           | N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Name
           | N_Name_Decl =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Identifier;

   function Has_Label (K : Nkind) return Boolean is
   begin
      return K = N_Assert_Directive;
   end Has_Label;

   function Has_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Assert_Directive
           | N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Actual
           | N_Name_Decl =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Chain;

   function Has_Instance (K : Nkind) return Boolean is
   begin
      case K is
         when N_Vmode
           | N_Vunit
           | N_Vprop =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Instance;

   function Has_Prefix (K : Nkind) return Boolean is
   begin
      return K = N_Hdl_Mod_Name;
   end Has_Prefix;

   function Has_Item_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Vmode
           | N_Vunit
           | N_Vprop =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Item_Chain;

   function Has_Property (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assert_Directive
           | N_Property_Declaration
           | N_Clock_Event
           | N_Always
           | N_Never
           | N_Eventually
           | N_Strong
           | N_Imp_Seq
           | N_Overlap_Imp_Seq
           | N_Next
           | N_Next_A
           | N_Next_E
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Abort
           | N_Async_Abort
           | N_Sync_Abort
           | N_Paren_Prop =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Property;

   function Has_String (K : Nkind) return Boolean is
   begin
      return K = N_Assert_Directive;
   end Has_String;

   function Has_SERE (K : Nkind) return Boolean is
   begin
      case K is
         when N_Braced_SERE
           | N_Clocked_SERE =>
            return True;
         when others =>
            return False;
      end case;
   end Has_SERE;

   function Has_Left (K : Nkind) return Boolean is
   begin
      case K is
         when N_Log_Imp_Prop
           | N_Log_Equiv_Prop
           | N_Until
           | N_Before
           | N_Or_Prop
           | N_And_Prop
           | N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Match_And_Seq
           | N_And_Seq
           | N_Or_Seq
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_Equiv_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Left;

   function Has_Right (K : Nkind) return Boolean is
   begin
      case K is
         when N_Log_Imp_Prop
           | N_Log_Equiv_Prop
           | N_Until
           | N_Before
           | N_Or_Prop
           | N_And_Prop
           | N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Match_And_Seq
           | N_And_Seq
           | N_Or_Seq
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_Equiv_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Right;

   function Has_Sequence (K : Nkind) return Boolean is
   begin
      case K is
         when N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Imp_Seq
           | N_Overlap_Imp_Seq
           | N_Star_Repeat_Seq
           | N_Plus_Repeat_Seq =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Sequence;

   function Has_Strong_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Next
           | N_Next_A
           | N_Next_E
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Until
           | N_Before =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Strong_Flag;

   function Has_Inclusive_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Until
           | N_Before =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Inclusive_Flag;

   function Has_Low_Bound (K : Nkind) return Boolean is
   begin
      case K is
         when N_Next_A
           | N_Next_E
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Low_Bound;

   function Has_High_Bound (K : Nkind) return Boolean is
   begin
      case K is
         when N_Next_A
           | N_Next_E
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq =>
            return True;
         when others =>
            return False;
      end case;
   end Has_High_Bound;

   function Has_Number (K : Nkind) return Boolean is
   begin
      case K is
         when N_Next
           | N_Next_Event =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number;

   function Has_Value (K : Nkind) return Boolean is
   begin
      return K = N_Number;
   end Has_Value;

   function Has_Boolean (K : Nkind) return Boolean is
   begin
      case K is
         when N_Clock_Event
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Abort
           | N_Async_Abort
           | N_Sync_Abort
           | N_Clocked_SERE
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq
           | N_Paren_Bool
           | N_Not_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Boolean;

   function Has_Decl (K : Nkind) return Boolean is
   begin
      return K = N_Name;
   end Has_Decl;

   function Has_HDL_Node (K : Nkind) return Boolean is
   begin
      case K is
         when N_HDL_Expr
           | N_HDL_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Has_HDL_Node;

   function Has_Hash (K : Nkind) return Boolean is
   begin
      case K is
         when N_Paren_Bool
           | N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_Equiv_Bool
           | N_HDL_Bool
           | N_EOS =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Hash;

   function Has_Hash_Link (K : Nkind) return Boolean is
   begin
      case K is
         when N_Paren_Bool
           | N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_Equiv_Bool
           | N_HDL_Bool
           | N_EOS =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Hash_Link;

   function Has_HDL_Index (K : Nkind) return Boolean is
   begin
      case K is
         when N_HDL_Bool
           | N_EOS =>
            return True;
         when others =>
            return False;
      end case;
   end Has_HDL_Index;

   function Has_HDL_Hash (K : Nkind) return Boolean is
   begin
      return K = N_HDL_Expr;
   end Has_HDL_Hash;

   function Has_Presence (K : Nkind) return Boolean is
   begin
      case K is
         when N_Paren_Bool
           | N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_Equiv_Bool
           | N_HDL_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Presence;

   function Has_NFA (K : Nkind) return Boolean is
   begin
      return K = N_Assert_Directive;
   end Has_NFA;

   function Has_Parameter_List (K : Nkind) return Boolean is
   begin
      case K is
         when N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter_List;

   function Has_Actual (K : Nkind) return Boolean is
   begin
      case K is
         when N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Actual =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Actual;

   function Has_Formal (K : Nkind) return Boolean is
   begin
      return K = N_Actual;
   end Has_Formal;

   function Has_Declaration (K : Nkind) return Boolean is
   begin
      case K is
         when N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Property_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Declaration;

   function Has_Association_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Property_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Association_Chain;

   function Has_Global_Clock (K : Nkind) return Boolean is
   begin
      return K = N_Property_Declaration;
   end Has_Global_Clock;

end PSL.Nodes_Meta;
