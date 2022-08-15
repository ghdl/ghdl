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

with Types; use Types;
with PSL.Nodes; use PSL.Nodes;

package PSL.Nodes_Meta is
   --  The enumeration of all possible types in the nodes.
   type Types_Enum is
     (
      Type_Boolean,
      Type_HDL_Node,
      Type_Int32,
      Type_NFA,
      Type_Name_Id,
      Type_Node,
      Type_PSL_Presence_Kind,
      Type_Uns32
     );

   --  The enumeration of all fields defined in iirs.
   type Fields_Enum is
     (
      Field_Identifier,
      Field_Label,
      Field_Chain,
      Field_Instance,
      Field_Prefix,
      Field_Item_Chain,
      Field_Property,
      Field_String,
      Field_SERE,
      Field_Left,
      Field_Right,
      Field_Sequence,
      Field_Strong_Flag,
      Field_Inclusive_Flag,
      Field_Has_Identifier_List,
      Field_Low_Bound,
      Field_High_Bound,
      Field_Number,
      Field_Value,
      Field_Boolean,
      Field_Decl,
      Field_HDL_Node,
      Field_Hash,
      Field_Hash_Link,
      Field_HDL_Index,
      Field_HDL_Hash,
      Field_Presence,
      Field_NFA,
      Field_Parameter_List,
      Field_Actual,
      Field_Formal,
      Field_Declaration,
      Field_Association_Chain,
      Field_Global_Clock
     );
   pragma Discard_Names (Fields_Enum);

   --  Return the type of field F.
   function Get_Field_Type (F : Fields_Enum) return Types_Enum;

   --  Get the name of a field.
   function Get_Field_Image (F : Fields_Enum) return String;

   --  Get the name of a kind.
   function Get_Nkind_Image (K : Nkind) return String;

   --  Possible attributes of a field.
   type Field_Attribute is
     (
      Attr_None,
      Attr_Ref, Attr_Maybe_Ref, Attr_Of_Ref,
      Attr_Chain, Attr_Chain_Next
     );

   --  Get the attribute of a field.
   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute;

   type Fields_Array is array (Natural range <>) of Fields_Enum;

   --  Return the list of fields for node K.  The fields are sorted: first
   --  the non nodes/list of nodes, then the nodes/lists that aren't reference,
   --  and then the reference.
   function Get_Fields (K : Nkind) return Fields_Array;

   --  Get/Set a field.
   function Get_Boolean
      (N : Node; F : Fields_Enum) return Boolean;
   procedure Set_Boolean
      (N : Node; F : Fields_Enum; V: Boolean);

   function Get_HDL_Node
      (N : Node; F : Fields_Enum) return HDL_Node;
   procedure Set_HDL_Node
      (N : Node; F : Fields_Enum; V: HDL_Node);

   function Get_Int32
      (N : Node; F : Fields_Enum) return Int32;
   procedure Set_Int32
      (N : Node; F : Fields_Enum; V: Int32);

   function Get_NFA
      (N : Node; F : Fields_Enum) return NFA;
   procedure Set_NFA
      (N : Node; F : Fields_Enum; V: NFA);

   function Get_Name_Id
      (N : Node; F : Fields_Enum) return Name_Id;
   procedure Set_Name_Id
      (N : Node; F : Fields_Enum; V: Name_Id);

   function Get_Node
      (N : Node; F : Fields_Enum) return Node;
   procedure Set_Node
      (N : Node; F : Fields_Enum; V: Node);

   function Get_PSL_Presence_Kind
      (N : Node; F : Fields_Enum) return PSL_Presence_Kind;
   procedure Set_PSL_Presence_Kind
      (N : Node; F : Fields_Enum; V: PSL_Presence_Kind);

   function Get_Uns32
      (N : Node; F : Fields_Enum) return Uns32;
   procedure Set_Uns32
      (N : Node; F : Fields_Enum; V: Uns32);

   function Has_Identifier (K : Nkind) return Boolean;
   function Has_Label (K : Nkind) return Boolean;
   function Has_Chain (K : Nkind) return Boolean;
   function Has_Instance (K : Nkind) return Boolean;
   function Has_Prefix (K : Nkind) return Boolean;
   function Has_Item_Chain (K : Nkind) return Boolean;
   function Has_Property (K : Nkind) return Boolean;
   function Has_String (K : Nkind) return Boolean;
   function Has_SERE (K : Nkind) return Boolean;
   function Has_Left (K : Nkind) return Boolean;
   function Has_Right (K : Nkind) return Boolean;
   function Has_Sequence (K : Nkind) return Boolean;
   function Has_Strong_Flag (K : Nkind) return Boolean;
   function Has_Inclusive_Flag (K : Nkind) return Boolean;
   function Has_Has_Identifier_List (K : Nkind) return Boolean;
   function Has_Low_Bound (K : Nkind) return Boolean;
   function Has_High_Bound (K : Nkind) return Boolean;
   function Has_Number (K : Nkind) return Boolean;
   function Has_Value (K : Nkind) return Boolean;
   function Has_Boolean (K : Nkind) return Boolean;
   function Has_Decl (K : Nkind) return Boolean;
   function Has_HDL_Node (K : Nkind) return Boolean;
   function Has_Hash (K : Nkind) return Boolean;
   function Has_Hash_Link (K : Nkind) return Boolean;
   function Has_HDL_Index (K : Nkind) return Boolean;
   function Has_HDL_Hash (K : Nkind) return Boolean;
   function Has_Presence (K : Nkind) return Boolean;
   function Has_NFA (K : Nkind) return Boolean;
   function Has_Parameter_List (K : Nkind) return Boolean;
   function Has_Actual (K : Nkind) return Boolean;
   function Has_Formal (K : Nkind) return Boolean;
   function Has_Declaration (K : Nkind) return Boolean;
   function Has_Association_Chain (K : Nkind) return Boolean;
   function Has_Global_Clock (K : Nkind) return Boolean;
end PSL.Nodes_Meta;
