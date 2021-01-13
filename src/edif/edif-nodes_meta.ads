--  Meta description of nodes.
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

with Types; use Types;
with Edif.Nodes; use Edif.Nodes;

package Edif.Nodes_Meta is
   --  The enumeration of all possible types in the nodes.
   type Types_Enum is
     (
      Type_Boolean,
      Type_Dir_Type,
      Type_Int32,
      Type_Name_Id,
      Type_Node,
      Type_String8_Id,
      Type_Uns32
     );

   --  The enumeration of all fields defined in iirs.
   type Fields_Enum is
     (
      Field_CAR,
      Field_CDR,
      Field_Symbol,
      Field_Keyword,
      Field_Number,
      Field_String_Id,
      Field_String_Len,
      Field_Name,
      Field_Edif_Level,
      Field_Edif_Version,
      Field_Keyword_Map,
      Field_Status,
      Field_Chain,
      Field_External_Chain,
      Field_Library_Chain,
      Field_Cells_Chain,
      Field_Ports_Chain,
      Field_Contents_Chain,
      Field_Properties_Chain,
      Field_Port_Instances_Chain,
      Field_Joined_Chain,
      Field_Design,
      Field_Designator,
      Field_Technology,
      Field_Cell_Type,
      Field_View_Type,
      Field_Interface,
      Field_View_Ref,
      Field_Cell_Ref,
      Field_Library_Ref,
      Field_View,
      Field_Direction,
      Field_Boolean,
      Field_Value,
      Field_Owner,
      Field_Instance_Ref,
      Field_Port,
      Field_Index,
      Field_Array_Length,
      Field_Unit,
      Field_String
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
      Attr_Maybe_Ref, Attr_Maybe_Ref2,
      Attr_None,
      Attr_Ref, Attr_Forward_Ref,
      Attr_Chain, Attr_Chain_Next
     );

   --  Attributes without Maybe_Ref*
   subtype Field_Actual_Attribute is Field_Attribute range
     Attr_None .. Field_Attribute'Last;

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

   function Get_Dir_Type
      (N : Node; F : Fields_Enum) return Dir_Type;
   procedure Set_Dir_Type
      (N : Node; F : Fields_Enum; V: Dir_Type);

   function Get_Int32
      (N : Node; F : Fields_Enum) return Int32;
   procedure Set_Int32
      (N : Node; F : Fields_Enum; V: Int32);

   function Get_Name_Id
      (N : Node; F : Fields_Enum) return Name_Id;
   procedure Set_Name_Id
      (N : Node; F : Fields_Enum; V: Name_Id);

   function Get_Node
      (N : Node; F : Fields_Enum) return Node;
   procedure Set_Node
      (N : Node; F : Fields_Enum; V: Node);

   function Get_String8_Id
      (N : Node; F : Fields_Enum) return String8_Id;
   procedure Set_String8_Id
      (N : Node; F : Fields_Enum; V: String8_Id);

   function Get_Uns32
      (N : Node; F : Fields_Enum) return Uns32;
   procedure Set_Uns32
      (N : Node; F : Fields_Enum; V: Uns32);

   function Has_CAR (K : Nkind) return Boolean;
   function Has_CDR (K : Nkind) return Boolean;
   function Has_Symbol (K : Nkind) return Boolean;
   function Has_Keyword (K : Nkind) return Boolean;
   function Has_Number (K : Nkind) return Boolean;
   function Has_String_Id (K : Nkind) return Boolean;
   function Has_String_Len (K : Nkind) return Boolean;
   function Has_Name (K : Nkind) return Boolean;
   function Has_Edif_Level (K : Nkind) return Boolean;
   function Has_Edif_Version (K : Nkind) return Boolean;
   function Has_Keyword_Map (K : Nkind) return Boolean;
   function Has_Status (K : Nkind) return Boolean;
   function Has_Chain (K : Nkind) return Boolean;
   function Has_External_Chain (K : Nkind) return Boolean;
   function Has_Library_Chain (K : Nkind) return Boolean;
   function Has_Cells_Chain (K : Nkind) return Boolean;
   function Has_Ports_Chain (K : Nkind) return Boolean;
   function Has_Contents_Chain (K : Nkind) return Boolean;
   function Has_Properties_Chain (K : Nkind) return Boolean;
   function Has_Port_Instances_Chain (K : Nkind) return Boolean;
   function Has_Joined_Chain (K : Nkind) return Boolean;
   function Has_Design (K : Nkind) return Boolean;
   function Has_Designator (K : Nkind) return Boolean;
   function Has_Technology (K : Nkind) return Boolean;
   function Has_Cell_Type (K : Nkind) return Boolean;
   function Has_View_Type (K : Nkind) return Boolean;
   function Has_Interface (K : Nkind) return Boolean;
   function Has_View_Ref (K : Nkind) return Boolean;
   function Has_Cell_Ref (K : Nkind) return Boolean;
   function Has_Library_Ref (K : Nkind) return Boolean;
   function Has_View (K : Nkind) return Boolean;
   function Has_Direction (K : Nkind) return Boolean;
   function Has_Boolean (K : Nkind) return Boolean;
   function Has_Value (K : Nkind) return Boolean;
   function Has_Owner (K : Nkind) return Boolean;
   function Has_Instance_Ref (K : Nkind) return Boolean;
   function Has_Port (K : Nkind) return Boolean;
   function Has_Index (K : Nkind) return Boolean;
   function Has_Array_Length (K : Nkind) return Boolean;
   function Has_Unit (K : Nkind) return Boolean;
   function Has_String (K : Nkind) return Boolean;
end Edif.Nodes_Meta;
