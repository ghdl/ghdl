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

package body Edif.Nodes_Meta is
   Fields_Type : constant array (Fields_Enum) of Types_Enum :=
     (
      Field_CAR => Type_Node,
      Field_CDR => Type_Node,
      Field_Symbol => Type_Name_Id,
      Field_Keyword => Type_Name_Id,
      Field_Number => Type_Int32,
      Field_String_Id => Type_String8_Id,
      Field_String_Len => Type_Uns32,
      Field_Name => Type_Node,
      Field_Edif_Level => Type_Int32,
      Field_Edif_Version => Type_Int32,
      Field_Keyword_Map => Type_Node,
      Field_Status => Type_Node,
      Field_Chain => Type_Node,
      Field_External_Chain => Type_Node,
      Field_Library_Chain => Type_Node,
      Field_Cells_Chain => Type_Node,
      Field_Ports_Chain => Type_Node,
      Field_Contents_Chain => Type_Node,
      Field_Properties_Chain => Type_Node,
      Field_Port_Instances_Chain => Type_Node,
      Field_Joined_Chain => Type_Node,
      Field_Design => Type_Node,
      Field_Designator => Type_Node,
      Field_Technology => Type_Node,
      Field_Cell_Type => Type_Name_Id,
      Field_View_Type => Type_Name_Id,
      Field_Interface => Type_Node,
      Field_View_Ref => Type_Name_Id,
      Field_Cell_Ref => Type_Node,
      Field_Library_Ref => Type_Node,
      Field_View => Type_Node,
      Field_Direction => Type_Dir_Type,
      Field_Boolean => Type_Boolean,
      Field_Value => Type_Node,
      Field_Owner => Type_Node,
      Field_Instance_Ref => Type_Node,
      Field_Port => Type_Node,
      Field_Index => Type_Int32,
      Field_Array_Length => Type_Int32,
      Field_Unit => Type_Name_Id,
      Field_String => Type_Node
     );

   function Get_Field_Type (F : Fields_Enum) return Types_Enum is
   begin
      return Fields_Type (F);
   end Get_Field_Type;

   function Get_Field_Image (F : Fields_Enum) return String is
   begin
      case F is
         when Field_CAR =>
            return "car";
         when Field_CDR =>
            return "cdr";
         when Field_Symbol =>
            return "symbol";
         when Field_Keyword =>
            return "keyword";
         when Field_Number =>
            return "number";
         when Field_String_Id =>
            return "string_id";
         when Field_String_Len =>
            return "string_len";
         when Field_Name =>
            return "name";
         when Field_Edif_Level =>
            return "edif_level";
         when Field_Edif_Version =>
            return "edif_version";
         when Field_Keyword_Map =>
            return "keyword_map";
         when Field_Status =>
            return "status";
         when Field_Chain =>
            return "chain";
         when Field_External_Chain =>
            return "external_chain";
         when Field_Library_Chain =>
            return "library_chain";
         when Field_Cells_Chain =>
            return "cells_chain";
         when Field_Ports_Chain =>
            return "ports_chain";
         when Field_Contents_Chain =>
            return "contents_chain";
         when Field_Properties_Chain =>
            return "properties_chain";
         when Field_Port_Instances_Chain =>
            return "port_instances_chain";
         when Field_Joined_Chain =>
            return "joined_chain";
         when Field_Design =>
            return "design";
         when Field_Designator =>
            return "designator";
         when Field_Technology =>
            return "technology";
         when Field_Cell_Type =>
            return "cell_type";
         when Field_View_Type =>
            return "view_type";
         when Field_Interface =>
            return "interface";
         when Field_View_Ref =>
            return "view_ref";
         when Field_Cell_Ref =>
            return "cell_ref";
         when Field_Library_Ref =>
            return "library_ref";
         when Field_View =>
            return "view";
         when Field_Direction =>
            return "direction";
         when Field_Boolean =>
            return "boolean";
         when Field_Value =>
            return "value";
         when Field_Owner =>
            return "owner";
         when Field_Instance_Ref =>
            return "instance_ref";
         when Field_Port =>
            return "port";
         when Field_Index =>
            return "index";
         when Field_Array_Length =>
            return "array_length";
         when Field_Unit =>
            return "unit";
         when Field_String =>
            return "string";
      end case;
   end Get_Field_Image;

   function Get_Nkind_Image (K : Nkind) return String is
   begin
      case K is
         when N_Error =>
            return "error";
         when N_Keyword =>
            return "keyword";
         when N_Symbol =>
            return "symbol";
         when N_Number =>
            return "number";
         when N_String =>
            return "string";
         when N_Chain =>
            return "chain";
         when N_Edif =>
            return "edif";
         when N_External =>
            return "external";
         when N_Cell =>
            return "cell";
         when N_View =>
            return "view";
         when N_Port =>
            return "port";
         when N_Library =>
            return "library";
         when N_Interface =>
            return "interface";
         when N_Instance =>
            return "instance";
         when N_Net =>
            return "net";
         when N_Design =>
            return "design";
         when N_Port_Ref =>
            return "port_ref";
         when N_Cell_Ref =>
            return "cell_ref";
         when N_View_Ref =>
            return "view_ref";
         when N_Member =>
            return "member";
         when N_Property =>
            return "property";
         when N_Userdata =>
            return "userdata";
         when N_Port_Instance =>
            return "port_instance";
         when N_Array =>
            return "array";
         when N_Rename =>
            return "rename";
         when N_Boolean =>
            return "boolean";
      end case;
   end Get_Nkind_Image;

   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute is
   begin
      case F is
         when Field_CAR =>
            return Attr_None;
         when Field_CDR =>
            return Attr_None;
         when Field_Symbol =>
            return Attr_None;
         when Field_Keyword =>
            return Attr_None;
         when Field_Number =>
            return Attr_None;
         when Field_String_Id =>
            return Attr_None;
         when Field_String_Len =>
            return Attr_None;
         when Field_Name =>
            return Attr_None;
         when Field_Edif_Level =>
            return Attr_None;
         when Field_Edif_Version =>
            return Attr_None;
         when Field_Keyword_Map =>
            return Attr_None;
         when Field_Status =>
            return Attr_None;
         when Field_Chain =>
            return Attr_Chain_Next;
         when Field_External_Chain =>
            return Attr_Chain;
         when Field_Library_Chain =>
            return Attr_Chain;
         when Field_Cells_Chain =>
            return Attr_Chain;
         when Field_Ports_Chain =>
            return Attr_Chain;
         when Field_Contents_Chain =>
            return Attr_Chain;
         when Field_Properties_Chain =>
            return Attr_Chain;
         when Field_Port_Instances_Chain =>
            return Attr_Chain;
         when Field_Joined_Chain =>
            return Attr_Chain;
         when Field_Design =>
            return Attr_None;
         when Field_Designator =>
            return Attr_None;
         when Field_Technology =>
            return Attr_None;
         when Field_Cell_Type =>
            return Attr_None;
         when Field_View_Type =>
            return Attr_None;
         when Field_Interface =>
            return Attr_None;
         when Field_View_Ref =>
            return Attr_None;
         when Field_Cell_Ref =>
            return Attr_None;
         when Field_Library_Ref =>
            return Attr_None;
         when Field_View =>
            return Attr_None;
         when Field_Direction =>
            return Attr_None;
         when Field_Boolean =>
            return Attr_None;
         when Field_Value =>
            return Attr_None;
         when Field_Owner =>
            return Attr_None;
         when Field_Instance_Ref =>
            return Attr_None;
         when Field_Port =>
            return Attr_None;
         when Field_Index =>
            return Attr_None;
         when Field_Array_Length =>
            return Attr_None;
         when Field_Unit =>
            return Attr_None;
         when Field_String =>
            return Attr_None;
      end case;
   end Get_Field_Attribute;

   Fields_Of_Iir : constant Fields_Array :=
     (
      --  N_Error
      --  N_Keyword
      Field_Keyword,
      Field_CDR,
      --  N_Symbol
      Field_Symbol,
      --  N_Number
      Field_Number,
      --  N_String
      Field_String_Id,
      Field_String_Len,
      --  N_Chain
      Field_CAR,
      Field_CDR,
      --  N_Edif
      Field_Name,
      Field_Edif_Version,
      Field_Edif_Level,
      Field_Keyword_Map,
      Field_Status,
      Field_External_Chain,
      Field_Library_Chain,
      Field_Design,
      --  N_External
      Field_Name,
      Field_Edif_Level,
      Field_Technology,
      Field_Cells_Chain,
      Field_Chain,
      --  N_Cell
      Field_Name,
      Field_Cell_Type,
      Field_Properties_Chain,
      Field_View,
      Field_Chain,
      --  N_View
      Field_Name,
      Field_View_Type,
      Field_Interface,
      Field_Properties_Chain,
      Field_Contents_Chain,
      Field_Chain,
      --  N_Port
      Field_Name,
      Field_Direction,
      Field_Properties_Chain,
      Field_Designator,
      Field_Chain,
      --  N_Library
      Field_Name,
      Field_Edif_Level,
      Field_Technology,
      Field_Cells_Chain,
      Field_Chain,
      --  N_Interface
      Field_Ports_Chain,
      Field_Properties_Chain,
      Field_Designator,
      --  N_Instance
      Field_Name,
      Field_Instance_Ref,
      Field_Port_Instances_Chain,
      Field_Properties_Chain,
      Field_Chain,
      --  N_Net
      Field_Name,
      Field_Joined_Chain,
      Field_Properties_Chain,
      Field_Chain,
      --  N_Design
      Field_Name,
      Field_Cell_Ref,
      Field_Properties_Chain,
      --  N_Port_Ref
      Field_Port,
      Field_Instance_Ref,
      Field_Chain,
      --  N_Cell_Ref
      Field_Name,
      Field_Library_Ref,
      --  N_View_Ref
      Field_Name,
      Field_Cell_Ref,
      --  N_Member
      Field_Name,
      Field_Index,
      --  N_Property
      Field_Name,
      Field_Value,
      Field_Owner,
      Field_Unit,
      Field_Chain,
      --  N_Userdata
      Field_Name,
      Field_CDR,
      Field_Chain,
      --  N_Port_Instance
      Field_Name,
      Field_Properties_Chain,
      Field_Chain,
      --  N_Array
      Field_Name,
      Field_Array_Length,
      --  N_Rename
      Field_Name,
      Field_String,
      --  N_Boolean
      Field_Boolean
     );

   Fields_Of_Iir_Last : constant array (Nkind) of Integer :=
     (
      N_Error => -1,
      N_Keyword => 1,
      N_Symbol => 2,
      N_Number => 3,
      N_String => 5,
      N_Chain => 7,
      N_Edif => 15,
      N_External => 20,
      N_Cell => 25,
      N_View => 31,
      N_Port => 36,
      N_Library => 41,
      N_Interface => 44,
      N_Instance => 49,
      N_Net => 53,
      N_Design => 56,
      N_Port_Ref => 59,
      N_Cell_Ref => 61,
      N_View_Ref => 63,
      N_Member => 65,
      N_Property => 70,
      N_Userdata => 73,
      N_Port_Instance => 76,
      N_Array => 78,
      N_Rename => 80,
      N_Boolean => 81
     );

   function Get_Fields (K : Nkind) return Fields_Array
   is
      First : Natural;
      Last : Integer;
   begin
      if K = Nkind'First then
         First := Fields_Of_Iir'First;
      else
         First := Fields_Of_Iir_Last (Nkind'Pred (K)) + 1;
      end if;
      Last := Fields_Of_Iir_Last (K);
      return Fields_Of_Iir (First .. Last);
   end Get_Fields;

   function Get_Boolean
      (N : Node; F : Fields_Enum) return Boolean is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Boolean =>
            return Get_Boolean (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Boolean;

   procedure Set_Boolean
      (N : Node; F : Fields_Enum; V: Boolean) is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Boolean =>
            Set_Boolean (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Boolean;

   function Get_Dir_Type
      (N : Node; F : Fields_Enum) return Dir_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Dir_Type);
      case F is
         when Field_Direction =>
            return Get_Direction (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Dir_Type;

   procedure Set_Dir_Type
      (N : Node; F : Fields_Enum; V: Dir_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Dir_Type);
      case F is
         when Field_Direction =>
            Set_Direction (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Dir_Type;

   function Get_Int32
      (N : Node; F : Fields_Enum) return Int32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_Number =>
            return Get_Number (N);
         when Field_Edif_Level =>
            return Get_Edif_Level (N);
         when Field_Edif_Version =>
            return Get_Edif_Version (N);
         when Field_Index =>
            return Get_Index (N);
         when Field_Array_Length =>
            return Get_Array_Length (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Int32;

   procedure Set_Int32
      (N : Node; F : Fields_Enum; V: Int32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_Number =>
            Set_Number (N, V);
         when Field_Edif_Level =>
            Set_Edif_Level (N, V);
         when Field_Edif_Version =>
            Set_Edif_Version (N, V);
         when Field_Index =>
            Set_Index (N, V);
         when Field_Array_Length =>
            Set_Array_Length (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Int32;

   function Get_Name_Id
      (N : Node; F : Fields_Enum) return Name_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Symbol =>
            return Get_Symbol (N);
         when Field_Keyword =>
            return Get_Keyword (N);
         when Field_Cell_Type =>
            return Get_Cell_Type (N);
         when Field_View_Type =>
            return Get_View_Type (N);
         when Field_View_Ref =>
            return Get_View_Ref (N);
         when Field_Unit =>
            return Get_Unit (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Name_Id;

   procedure Set_Name_Id
      (N : Node; F : Fields_Enum; V: Name_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Symbol =>
            Set_Symbol (N, V);
         when Field_Keyword =>
            Set_Keyword (N, V);
         when Field_Cell_Type =>
            Set_Cell_Type (N, V);
         when Field_View_Type =>
            Set_View_Type (N, V);
         when Field_View_Ref =>
            Set_View_Ref (N, V);
         when Field_Unit =>
            Set_Unit (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Name_Id;

   function Get_Node
      (N : Node; F : Fields_Enum) return Node is
   begin
      pragma Assert (Fields_Type (F) = Type_Node);
      case F is
         when Field_CAR =>
            return Get_CAR (N);
         when Field_CDR =>
            return Get_CDR (N);
         when Field_Name =>
            return Get_Name (N);
         when Field_Keyword_Map =>
            return Get_Keyword_Map (N);
         when Field_Status =>
            return Get_Status (N);
         when Field_Chain =>
            return Get_Chain (N);
         when Field_External_Chain =>
            return Get_External_Chain (N);
         when Field_Library_Chain =>
            return Get_Library_Chain (N);
         when Field_Cells_Chain =>
            return Get_Cells_Chain (N);
         when Field_Ports_Chain =>
            return Get_Ports_Chain (N);
         when Field_Contents_Chain =>
            return Get_Contents_Chain (N);
         when Field_Properties_Chain =>
            return Get_Properties_Chain (N);
         when Field_Port_Instances_Chain =>
            return Get_Port_Instances_Chain (N);
         when Field_Joined_Chain =>
            return Get_Joined_Chain (N);
         when Field_Design =>
            return Get_Design (N);
         when Field_Designator =>
            return Get_Designator (N);
         when Field_Technology =>
            return Get_Technology (N);
         when Field_Interface =>
            return Get_Interface (N);
         when Field_Cell_Ref =>
            return Get_Cell_Ref (N);
         when Field_Library_Ref =>
            return Get_Library_Ref (N);
         when Field_View =>
            return Get_View (N);
         when Field_Value =>
            return Get_Value (N);
         when Field_Owner =>
            return Get_Owner (N);
         when Field_Instance_Ref =>
            return Get_Instance_Ref (N);
         when Field_Port =>
            return Get_Port (N);
         when Field_String =>
            return Get_String (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Node;

   procedure Set_Node
      (N : Node; F : Fields_Enum; V: Node) is
   begin
      pragma Assert (Fields_Type (F) = Type_Node);
      case F is
         when Field_CAR =>
            Set_CAR (N, V);
         when Field_CDR =>
            Set_CDR (N, V);
         when Field_Name =>
            Set_Name (N, V);
         when Field_Keyword_Map =>
            Set_Keyword_Map (N, V);
         when Field_Status =>
            Set_Status (N, V);
         when Field_Chain =>
            Set_Chain (N, V);
         when Field_External_Chain =>
            Set_External_Chain (N, V);
         when Field_Library_Chain =>
            Set_Library_Chain (N, V);
         when Field_Cells_Chain =>
            Set_Cells_Chain (N, V);
         when Field_Ports_Chain =>
            Set_Ports_Chain (N, V);
         when Field_Contents_Chain =>
            Set_Contents_Chain (N, V);
         when Field_Properties_Chain =>
            Set_Properties_Chain (N, V);
         when Field_Port_Instances_Chain =>
            Set_Port_Instances_Chain (N, V);
         when Field_Joined_Chain =>
            Set_Joined_Chain (N, V);
         when Field_Design =>
            Set_Design (N, V);
         when Field_Designator =>
            Set_Designator (N, V);
         when Field_Technology =>
            Set_Technology (N, V);
         when Field_Interface =>
            Set_Interface (N, V);
         when Field_Cell_Ref =>
            Set_Cell_Ref (N, V);
         when Field_Library_Ref =>
            Set_Library_Ref (N, V);
         when Field_View =>
            Set_View (N, V);
         when Field_Value =>
            Set_Value (N, V);
         when Field_Owner =>
            Set_Owner (N, V);
         when Field_Instance_Ref =>
            Set_Instance_Ref (N, V);
         when Field_Port =>
            Set_Port (N, V);
         when Field_String =>
            Set_String (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Node;

   function Get_String8_Id
      (N : Node; F : Fields_Enum) return String8_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_String8_Id);
      case F is
         when Field_String_Id =>
            return Get_String_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_String8_Id;

   procedure Set_String8_Id
      (N : Node; F : Fields_Enum; V: String8_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_String8_Id);
      case F is
         when Field_String_Id =>
            Set_String_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_String8_Id;

   function Get_Uns32
      (N : Node; F : Fields_Enum) return Uns32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Uns32);
      case F is
         when Field_String_Len =>
            return Get_String_Len (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Uns32;

   procedure Set_Uns32
      (N : Node; F : Fields_Enum; V: Uns32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Uns32);
      case F is
         when Field_String_Len =>
            Set_String_Len (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Uns32;

   function Has_CAR (K : Nkind) return Boolean is
   begin
      return K = N_Chain;
   end Has_CAR;

   function Has_CDR (K : Nkind) return Boolean is
   begin
      case K is
         when N_Keyword
           | N_Chain
           | N_Userdata =>
            return True;
         when others =>
            return False;
      end case;
   end Has_CDR;

   function Has_Symbol (K : Nkind) return Boolean is
   begin
      return K = N_Symbol;
   end Has_Symbol;

   function Has_Keyword (K : Nkind) return Boolean is
   begin
      return K = N_Keyword;
   end Has_Keyword;

   function Has_Number (K : Nkind) return Boolean is
   begin
      return K = N_Number;
   end Has_Number;

   function Has_String_Id (K : Nkind) return Boolean is
   begin
      return K = N_String;
   end Has_String_Id;

   function Has_String_Len (K : Nkind) return Boolean is
   begin
      return K = N_String;
   end Has_String_Len;

   function Has_Name (K : Nkind) return Boolean is
   begin
      case K is
         when N_Edif
           | N_External
           | N_Cell
           | N_View
           | N_Port
           | N_Library
           | N_Instance
           | N_Net
           | N_Design
           | N_Cell_Ref
           | N_View_Ref
           | N_Member
           | N_Property
           | N_Userdata
           | N_Port_Instance
           | N_Array
           | N_Rename =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Name;

   function Has_Edif_Level (K : Nkind) return Boolean is
   begin
      case K is
         when N_Edif
           | N_External
           | N_Library =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Edif_Level;

   function Has_Edif_Version (K : Nkind) return Boolean is
   begin
      return K = N_Edif;
   end Has_Edif_Version;

   function Has_Keyword_Map (K : Nkind) return Boolean is
   begin
      return K = N_Edif;
   end Has_Keyword_Map;

   function Has_Status (K : Nkind) return Boolean is
   begin
      return K = N_Edif;
   end Has_Status;

   function Has_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_External
           | N_Cell
           | N_View
           | N_Port
           | N_Library
           | N_Instance
           | N_Net
           | N_Port_Ref
           | N_Property
           | N_Userdata
           | N_Port_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Chain;

   function Has_External_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Edif;
   end Has_External_Chain;

   function Has_Library_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Edif;
   end Has_Library_Chain;

   function Has_Cells_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_External
           | N_Library =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Cells_Chain;

   function Has_Ports_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Interface;
   end Has_Ports_Chain;

   function Has_Contents_Chain (K : Nkind) return Boolean is
   begin
      return K = N_View;
   end Has_Contents_Chain;

   function Has_Properties_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Cell
           | N_View
           | N_Port
           | N_Interface
           | N_Instance
           | N_Net
           | N_Design
           | N_Port_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Properties_Chain;

   function Has_Port_Instances_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Instance;
   end Has_Port_Instances_Chain;

   function Has_Joined_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Net;
   end Has_Joined_Chain;

   function Has_Design (K : Nkind) return Boolean is
   begin
      return K = N_Edif;
   end Has_Design;

   function Has_Designator (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port
           | N_Interface =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Designator;

   function Has_Technology (K : Nkind) return Boolean is
   begin
      case K is
         when N_External
           | N_Library =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Technology;

   function Has_Cell_Type (K : Nkind) return Boolean is
   begin
      return K = N_Cell;
   end Has_Cell_Type;

   function Has_View_Type (K : Nkind) return Boolean is
   begin
      return K = N_View;
   end Has_View_Type;

   function Has_Interface (K : Nkind) return Boolean is
   begin
      return K = N_View;
   end Has_Interface;

   function Has_View_Ref (K : Nkind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_View_Ref;

   function Has_Cell_Ref (K : Nkind) return Boolean is
   begin
      case K is
         when N_Design
           | N_View_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Cell_Ref;

   function Has_Library_Ref (K : Nkind) return Boolean is
   begin
      return K = N_Cell_Ref;
   end Has_Library_Ref;

   function Has_View (K : Nkind) return Boolean is
   begin
      return K = N_Cell;
   end Has_View;

   function Has_Direction (K : Nkind) return Boolean is
   begin
      return K = N_Port;
   end Has_Direction;

   function Has_Boolean (K : Nkind) return Boolean is
   begin
      return K = N_Boolean;
   end Has_Boolean;

   function Has_Value (K : Nkind) return Boolean is
   begin
      return K = N_Property;
   end Has_Value;

   function Has_Owner (K : Nkind) return Boolean is
   begin
      return K = N_Property;
   end Has_Owner;

   function Has_Instance_Ref (K : Nkind) return Boolean is
   begin
      case K is
         when N_Instance
           | N_Port_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Instance_Ref;

   function Has_Port (K : Nkind) return Boolean is
   begin
      return K = N_Port_Ref;
   end Has_Port;

   function Has_Index (K : Nkind) return Boolean is
   begin
      return K = N_Member;
   end Has_Index;

   function Has_Array_Length (K : Nkind) return Boolean is
   begin
      return K = N_Array;
   end Has_Array_Length;

   function Has_Unit (K : Nkind) return Boolean is
   begin
      return K = N_Property;
   end Has_Unit;

   function Has_String (K : Nkind) return Boolean is
   begin
      return K = N_Rename;
   end Has_String;

end Edif.Nodes_Meta;
