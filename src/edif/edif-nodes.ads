--  EDIF nodes.
--  Copyright (C) 2019 Tristan Gingold
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

package Edif.Nodes is

   type Nkind is
     (
      N_Error,

      --  Generic nodes.
      N_Keyword,
      N_Symbol,
      N_Number,
      N_String,
      N_Chain,

      --  Edif 2.0.0 nodes
      N_Edif,
      N_External,
      N_Cell,
      N_View,
      N_Port,
      N_Library,
      N_Interface,
      N_Instance,
      N_Net,
      N_Design,
      N_Port_Ref,
      N_Cell_Ref,
      N_View_Ref,
      N_Member,
      N_Property,
      N_Userdata,
      N_Port_Instance,

      N_Array,
      N_Rename,
      N_Boolean

     );

   type Node is new Nat32;
   for Node'Size use 32;

   Null_Node : constant Node := 0;

   type Dir_Type is
     (
      Dir_Input,
      Dir_Output,
      Dir_Inout
     );

   --  The next line marks the start of the node description.
   -- Start of Nkind.

   -- N_Error (X1)

   -- N_Keyword (X1)
   --   Get/Set_Keyword (Field1)
   --
   --   Get/Set_CDR (Field2)

   -- N_Chain (X1)
   --   Get/Set_CAR (Field1)
   --
   --   Get/Set_CDR (Field2)

   -- N_Symbol (X1)
   --   Get/Set_Symbol (Field1)

   -- N_Number (X1)
   --   Get/Set_Number (Field1)

   -- N_String (X1)
   --   Get/Set_String_Id (Field1)
   --
   --   Get/Set_String_Len (Field2)

   -- N_Edif (X4)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Edif_Version (Field3)
   --
   --   Get/Set_Edif_Level (Field2)
   --
   --   Get/Set_Keyword_Map (Field4)
   --
   --   Get/Set_Status (Field5)
   --
   --   Get/Set_External_Chain (Field6)
   --
   --   Get/Set_Library_Chain (Field7)
   --
   --   Get/Set_Design (Field8)

   -- N_External (X2)
   -- N_Library (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Edif_Level (Field2)
   --
   --   Get/Set_Technology (Field3)
   --
   --   Get/Set_Cells_Chain (Field4)
   --
   --   Get/Set_Chain (Field5)

   -- N_Cell (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Cell_Type (Field2)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --   Get/Set_View (Field4)
   --
   --   Get/Set_Chain (Field5)

   -- N_Interface (X2)
   --   Get/Set_Ports_Chain (Field2)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --   Get/Set_Designator (Field4)

   -- N_View (X4)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_View_Type (Field2)
   --
   --   Get/Set_Interface (Field6)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --  Instances and nets.
   --   Get/Set_Contents_Chain (Field4)
   --
   --   Get/Set_Chain (Field5)

   -- N_Port (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Direction (State1)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --   Get/Set_Designator (Field4)
   --
   --   Get/Set_Chain (Field5)

   -- N_Property (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Value (Field2)
   --
   --   Get/Set_Owner (Field3)
   --
   --   Get/Set_Unit (Field4)
   --
   --   Get/Set_Chain (Field5)

   -- N_Userdata (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_CDR (Field2)
   --
   --   Get/Set_Chain (Field5)

   -- N_Instance (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Instance_Ref (Field2)
   --
   --   Get/Set_Port_Instances_Chain (Field4)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --   Get/Set_Chain (Field5)

   -- N_Net (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Joined_Chain (Field2)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --   Get/Set_Chain (Field5)

   -- N_Design (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Cell_Ref (Field2)
   --
   --   Get/Set_Properties_Chain (Field3)

   -- N_Port_Ref (X2)
   --   Get/Set_Port (Field1)
   --
   --   Get/Set_Instance_Ref (Field2)
   --
   --   Get/Set_Chain (Field5)

   -- N_View_Ref (X1)
   --
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Cell_Ref (Field2)

   -- N_Cell_Ref (X1)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Library_Ref (Field2)

   -- N_Port_Instance (X2)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Properties_Chain (Field3)
   --
   --   Get/Set_Chain (Field5)

   -- N_Member (X1)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Index (Field2)

   -- N_Array (X1)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_Array_Length (Field2)

   -- N_Rename (X1)
   --   Get/Set_Name (Field1)
   --
   --   Get/Set_String (Field2)

   -- N_Boolean (X1)
   --   Get/Set_Boolean (Flag1)

   -- End of Nkind.

   -- General methods.

   function Create_Node (Kind : Nkind) return Node;
   procedure Free_Node (N : Node);

   --  Note: use Field0
   function Get_Location (N : Node) return Location_Type;
   procedure Set_Location (N : Node; Loc : Location_Type);

   function Get_Kind (N : Node) return Nkind;

   --  Field: Field1
   function Get_CAR (N : Node) return Node;
   procedure Set_CAR (N : Node; V : Node);

   --  Field: Field2
   function Get_CDR (N : Node) return Node;
   procedure Set_CDR (N : Node; V : Node);

   --  Field: Field1 (pos)
   function Get_Symbol (N : Node) return Name_Id;
   procedure Set_Symbol (N : Node; Id : Name_Id);

   --  Field: Field1 (pos)
   function Get_Keyword (N : Node) return Name_Id;
   procedure Set_Keyword (N : Node; Id : Name_Id);

   --  Field: Field1 (uc)
   function Get_Number (N : Node) return Int32;
   procedure Set_Number (N : Node; Val : Int32);

   --  Field: Field1 (pos)
   function Get_String_Id (N : Node) return String8_Id;
   procedure Set_String_Id (N : Node; Id : String8_Id);

   --  Field: Field2 (uc)
   function Get_String_Len (N : Node) return Uns32;
   procedure Set_String_Len (N : Node; Bn : Uns32);


   --  Field: Field1
   function Get_Name (N : Node) return Node;
   procedure Set_Name (N : Node; Name : Node);

   --  Field: Field2 (uc)
   function Get_Edif_Level (N : Node) return Int32;
   procedure Set_Edif_Level (N : Node; Level : Int32);

   --  Major*100 + Minor*10 + Release
   --  Field: Field3 (uc)
   function Get_Edif_Version (N : Node) return Int32;
   procedure Set_Edif_Version (N : Node; Version : Int32);

   --  Field: Field4
   function Get_Keyword_Map (N : Node) return Node;
   procedure Set_Keyword_Map (N : Node; Map : Node);

   --  Field: Field5
   function Get_Status (N : Node) return Node;
   procedure Set_Status (N : Node; Status : Node);

   --  Field: Field5 Chain_Next
   function Get_Chain (N : Node) return Node;
   procedure Set_Chain (N : Node; Chain : Node);

   --  Field: Field6 Chain
   function Get_External_Chain (N : Node) return Node;
   procedure Set_External_Chain (N : Node; Chain : Node);

   --  Field: Field7 Chain
   function Get_Library_Chain (N : Node) return Node;
   procedure Set_Library_Chain (N : Node; Chain : Node);

   --  Field: Field4 Chain
   function Get_Cells_Chain (N : Node) return Node;
   procedure Set_Cells_Chain (N : Node; Chain : Node);

   --  Field: Field2 Chain
   function Get_Ports_Chain (N : Node) return Node;
   procedure Set_Ports_Chain (N : Node; Chain : Node);

   --  Field: Field4 Chain
   function Get_Contents_Chain (N : Node) return Node;
   procedure Set_Contents_Chain (N : Node; Chain : Node);

   --  Field: Field3 Chain
   function Get_Properties_Chain (N : Node) return Node;
   procedure Set_Properties_Chain (N : Node; Chain : Node);

   --  Field: Field4 Chain
   function Get_Port_Instances_Chain (N : Node) return Node;
   procedure Set_Port_Instances_Chain (N : Node; Chain : Node);

   --  Field: Field2 Chain
   function Get_Joined_Chain (N : Node) return Node;
   procedure Set_Joined_Chain (N : Node; Chain : Node);

   --  Field: Field8
   function Get_Design (N : Node) return Node;
   procedure Set_Design (N : Node; Design : Node);

   --  Field: Field4
   function Get_Designator (N : Node) return Node;
   procedure Set_Designator (N : Node; Id : Node);

   --  Field: Field3
   function Get_Technology (N : Node) return Node;
   procedure Set_Technology (N : Node; Design : Node);

   --  Field: Field2 (pos)
   function Get_Cell_Type (N : Node) return Name_Id;
   procedure Set_Cell_Type (N : Node; Ctype : Name_Id);

   --  Field: Field2 (pos)
   function Get_View_Type (N : Node) return Name_Id;
   procedure Set_View_Type (N : Node; Vtype : Name_Id);

   --  Field: Field6
   function Get_Interface (N : Node) return Node;
   procedure Set_Interface (N : Node; Inter : Node);

   --  Field: Field1 (pos)
   function Get_View_Ref (N : Node) return Name_Id;
   procedure Set_View_Ref (N : Node; Ref : Name_Id);

   --  Field: Field2
   function Get_Cell_Ref (N : Node) return Node;
   procedure Set_Cell_Ref (N : Node; Ref : Node);

   --  Field: Field2
   function Get_Library_Ref (N : Node) return Node;
   procedure Set_Library_Ref (N : Node; Ref : Node);

   --  Field: Field4
   function Get_View (N : Node) return Node;
   procedure Set_View (N : Node; View : Node);

   --  Field: State1 (uc)
   function Get_Direction (N : Node) return Dir_Type;
   procedure Set_Direction (N : Node; Dir : Dir_Type);

   --  Field: Flag1
   function Get_Boolean (N : Node) return Boolean;
   procedure Set_Boolean (N : Node; Val : Boolean);

   --  Field: Field2
   function Get_Value (N : Node) return Node;
   procedure Set_Value (N : Node; Val : Node);

   --  Field: Field3
   function Get_Owner (N : Node) return Node;
   procedure Set_Owner (N : Node; Owner : Node);

   --  Field: Field2
   function Get_Instance_Ref (N : Node) return Node;
   procedure Set_Instance_Ref (N : Node; Ref : Node);

   --  Field: Field1
   function Get_Port (N : Node) return Node;
   procedure Set_Port (N : Node; Port : Node);

   --  Field: Field2 (uc)
   function Get_Index (N : Node) return Int32;
   procedure Set_Index (N : Node; Idx : Int32);

   --  Field: Field2 (uc)
   function Get_Array_Length (N : Node) return Int32;
   procedure Set_Array_Length (N : Node; Len : Int32);

   --  Field: Field4 (pos)
   function Get_Unit (N : Node) return Name_Id;
   procedure Set_Unit (N : Node; Unit : Name_Id);

   --  Field: Field2
   function Get_String (N : Node) return Node;
   procedure Set_String (N : Node; Str : Node);

end Edif.Nodes;
