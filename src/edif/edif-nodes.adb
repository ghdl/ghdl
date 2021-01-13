--  EDIF nodes. This is in fact -*- Ada -*-
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

with Ada.Unchecked_Conversion;
with Tables;
with Edif.Nodes_Meta; use Edif.Nodes_Meta;

package body Edif.Nodes is
   type Format_Type is
     (
      Format_X1,
      Format_X2,
      Format_X4
     );

   -- Common fields are:
   --   Nkind : Kind_Type
   --   State1 : Bit2_Type
   --   Flag1 : Boolean
   --   Flag2 : Boolean
   --   Flag3 : Boolean
   --   Flag4 : Boolean
   --   Flag5 : Boolean
   --   Flag6 : Boolean
   --   Flag7 : Boolean
   --   Flag8 : Boolean
   --   Flag9 : Boolean
   --   Flag10 : Boolean
   --   Flag11 : Boolean
   --   Flag12 : Boolean
   --   Flag13 : Boolean
   --   Flag14 : Boolean
   --   Field0 : Node
   --   Field1 : Node
   --   Field2 : Node

   -- Fields of Format_X1:

   -- Fields of Format_X2:
   --   Field3 : Node
   --   Field4 : Node
   --   Field5 : Node

   -- Fields of Format_X4:
   --   Field3 : Node
   --   Field4 : Node
   --   Field5 : Node
   --   Field6 : Node
   --   Field7 : Node
   --   Field8 : Node
   --   Field9 : Node
   --   Field10 : Node
   --   Field11 : Node

   type Bit2_Type is range 0 .. 2 ** 2 - 1;

   type Node_Record is record
      Kind : Nkind;      --  8 bits
      State1 : Bit2_Type;
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
      Flag17 : Boolean;
      Flag18 : Boolean;
      Flag19 : Boolean;
      Flag20 : Boolean;
      Flag21 : Boolean;
      Flag22 : Boolean;

      Field0 : Node;
      Field1 : Node;
      Field2 : Node;
   end record;
   pragma Pack (Node_Record);
   for Node_Record'Size use 4 * 32;

   package Nodet is new Tables
     (Table_Component_Type => Node_Record,
      Table_Index_Type => Node,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   Init_Node : constant Node_Record :=
     (Kind => N_Error,
      Flag1 | Flag2 | Flag3 | Flag4 | Flag5 | Flag6 | Flag7 | Flag8 => False,
      Flag9 | Flag10 | Flag11 | Flag12 | Flag13 | Flag14 | Flag15 => False,
      Flag16 | Flag17 | Flag18 | Flag19 | Flag20 | Flag21 | Flag22 => False,
      State1 => 0,
      Field0 | Field1 | Field2 => 0);

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

   function Bit2_Type_To_Dir_Type is new Ada.Unchecked_Conversion
     (Bit2_Type, Dir_Type);
   function Dir_Type_To_Bit2_Type is new Ada.Unchecked_Conversion
     (Dir_Type, Bit2_Type);


   function Node_To_Location_Type (N : Node) return Location_Type is
   begin
      return Location_Type (N);
   end Node_To_Location_Type;

   function Location_Type_To_Node (L : Location_Type) return Node is
   begin
      return Node (L);
   end Location_Type_To_Node;


   procedure Set_Kind (N : Node; K : Nkind) is
   begin
      Nodet.Table (N).Kind := K;
   end Set_Kind;

   function Get_Kind (N : Node) return Nkind is
   begin
      pragma Assert (N /= Null_Node, "get_kind: null node");
      return Nodet.Table (N).Kind;
   end Get_Kind;

   procedure Set_State1 (N : Node; State : Bit2_Type) is
   begin
      Nodet.Table (N).State1 := State;
   end Set_State1;

   function Get_State1 (N : Node) return Bit2_Type is
   begin
      return Nodet.Table (N).State1;
   end Get_State1;


   procedure Set_Flag1 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag1 := Flag;
   end Set_Flag1;

   function Get_Flag1 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag1;
   end Get_Flag1;


   procedure Set_Field0 (N : Node; V : Node) is
   begin
      Nodet.Table (N).Field0 := V;
   end Set_Field0;

   function Get_Field0 (N : Node) return Node is
   begin
      return Nodet.Table (N).Field0;
   end Get_Field0;


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


   procedure Set_Field3 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field0 := V;
   end Set_Field3;

   function Get_Field3 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field0;
   end Get_Field3;


   procedure Set_Field4 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field1 := V;
   end Set_Field4;

   function Get_Field4 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field1;
   end Get_Field4;


   procedure Set_Field5 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field2 := V;
   end Set_Field5;

   function Get_Field5 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field2;
   end Get_Field5;


   procedure Set_Field6 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 2).Field0 := V;
   end Set_Field6;

   function Get_Field6 (N : Node) return Node is
   begin
      return Nodet.Table (N + 2).Field0;
   end Get_Field6;


   procedure Set_Field7 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 2).Field1 := V;
   end Set_Field7;

   function Get_Field7 (N : Node) return Node is
   begin
      return Nodet.Table (N + 2).Field1;
   end Get_Field7;


   procedure Set_Field8 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 2).Field2 := V;
   end Set_Field8;

   function Get_Field8 (N : Node) return Node is
   begin
      return Nodet.Table (N + 2).Field2;
   end Get_Field8;


   function Get_Format (Kind : Nkind) return Format_Type;

   function Create_Node (Kind : Nkind) return Node
   is
      Res : Node;
   begin
      case Get_Format (Kind) is
         when Format_X1 =>
            if Free_Nodes /= Null_Node then
               Res := Free_Nodes;
               Free_Nodes := Get_Field1 (Res);
            else
               Nodet.Increment_Last;
               Res := Nodet.Last;
            end if;
         when Format_X2 =>
            Res := Nodet.Allocate (2);
            Nodet.Table (Res + 1) := Init_Node;
         when Format_X4 =>
            Res := Nodet.Allocate (4);
            Nodet.Table (Res + 1) := Init_Node;
            Nodet.Table (Res + 2) := Init_Node;
            Nodet.Table (Res + 3) := Init_Node;
      end case;
      Nodet.Table (Res) := Init_Node;
      Set_Kind (Res, Kind);
      return Res;
   end Create_Node;

   procedure Free_Node (N : Node)
   is
   begin
      --  FIXME: handle extended nodes.
      Set_Kind (N, N_Error);
      Set_Field1 (N, Free_Nodes);
      Free_Nodes := N;
   end Free_Node;

   function Get_Location (N : Node) return Location_Type is
   begin
      return Node_To_Location_Type (Get_Field0 (N));
   end Get_Location;

   procedure Set_Location (N : Node; Loc : Location_Type) is
   begin
      Set_Field0 (N, Location_Type_To_Node (Loc));
   end Set_Location;

   pragma Unreferenced (Get_Last_Node);

   --  Subprograms
   function Get_Format (Kind : Nkind) return Format_Type is
   begin
      case Kind is
         when N_Error
           | N_Keyword
           | N_Symbol
           | N_Number
           | N_String
           | N_Chain
           | N_Cell_Ref
           | N_View_Ref
           | N_Member
           | N_Array
           | N_Rename
           | N_Boolean =>
            return Format_X1;
         when N_External
           | N_Cell
           | N_Port
           | N_Library
           | N_Interface
           | N_Instance
           | N_Net
           | N_Design
           | N_Port_Ref
           | N_Property
           | N_Userdata
           | N_Port_Instance =>
            return Format_X2;
         when N_Edif
           | N_View =>
            return Format_X4;
      end case;
   end Get_Format;

   function Get_CAR (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_CAR (Get_Kind (N)),
                     "no field CAR");
      return Get_Field1 (N);
   end Get_CAR;

   procedure Set_CAR (N : Node; V : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_CAR (Get_Kind (N)),
                     "no field CAR");
      Set_Field1 (N, V);
   end Set_CAR;

   function Get_CDR (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_CDR (Get_Kind (N)),
                     "no field CDR");
      return Get_Field2 (N);
   end Get_CDR;

   procedure Set_CDR (N : Node; V : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_CDR (Get_Kind (N)),
                     "no field CDR");
      Set_Field2 (N, V);
   end Set_CDR;

   function Get_Symbol (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Symbol (Get_Kind (N)),
                     "no field Symbol");
      return Name_Id'Val (Get_Field1 (N));
   end Get_Symbol;

   procedure Set_Symbol (N : Node; Id : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Symbol (Get_Kind (N)),
                     "no field Symbol");
      Set_Field1 (N, Name_Id'Pos (Id));
   end Set_Symbol;

   function Get_Keyword (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Keyword (Get_Kind (N)),
                     "no field Keyword");
      return Name_Id'Val (Get_Field1 (N));
   end Get_Keyword;

   procedure Set_Keyword (N : Node; Id : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Keyword (Get_Kind (N)),
                     "no field Keyword");
      Set_Field1 (N, Name_Id'Pos (Id));
   end Set_Keyword;

   function Get_Number (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number (Get_Kind (N)),
                     "no field Number");
      return Node_To_Int32 (Get_Field1 (N));
   end Get_Number;

   procedure Set_Number (N : Node; Val : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number (Get_Kind (N)),
                     "no field Number");
      Set_Field1 (N, Int32_To_Node (Val));
   end Set_Number;

   function Get_String_Id (N : Node) return String8_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String_Id (Get_Kind (N)),
                     "no field String_Id");
      return String8_Id'Val (Get_Field1 (N));
   end Get_String_Id;

   procedure Set_String_Id (N : Node; Id : String8_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String_Id (Get_Kind (N)),
                     "no field String_Id");
      Set_Field1 (N, String8_Id'Pos (Id));
   end Set_String_Id;

   function Get_String_Len (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String_Len (Get_Kind (N)),
                     "no field String_Len");
      return Node_To_Uns32 (Get_Field2 (N));
   end Get_String_Len;

   procedure Set_String_Len (N : Node; Bn : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String_Len (Get_Kind (N)),
                     "no field String_Len");
      Set_Field2 (N, Uns32_To_Node (Bn));
   end Set_String_Len;

   function Get_Name (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Name (Get_Kind (N)),
                     "no field Name");
      return Get_Field1 (N);
   end Get_Name;

   procedure Set_Name (N : Node; Name : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Name (Get_Kind (N)),
                     "no field Name");
      Set_Field1 (N, Name);
   end Set_Name;

   function Get_Edif_Level (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Edif_Level (Get_Kind (N)),
                     "no field Edif_Level");
      return Node_To_Int32 (Get_Field2 (N));
   end Get_Edif_Level;

   procedure Set_Edif_Level (N : Node; Level : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Edif_Level (Get_Kind (N)),
                     "no field Edif_Level");
      Set_Field2 (N, Int32_To_Node (Level));
   end Set_Edif_Level;

   function Get_Edif_Version (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Edif_Version (Get_Kind (N)),
                     "no field Edif_Version");
      return Node_To_Int32 (Get_Field3 (N));
   end Get_Edif_Version;

   procedure Set_Edif_Version (N : Node; Version : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Edif_Version (Get_Kind (N)),
                     "no field Edif_Version");
      Set_Field3 (N, Int32_To_Node (Version));
   end Set_Edif_Version;

   function Get_Keyword_Map (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Keyword_Map (Get_Kind (N)),
                     "no field Keyword_Map");
      return Get_Field4 (N);
   end Get_Keyword_Map;

   procedure Set_Keyword_Map (N : Node; Map : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Keyword_Map (Get_Kind (N)),
                     "no field Keyword_Map");
      Set_Field4 (N, Map);
   end Set_Keyword_Map;

   function Get_Status (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Status (Get_Kind (N)),
                     "no field Status");
      return Get_Field5 (N);
   end Get_Status;

   procedure Set_Status (N : Node; Status : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Status (Get_Kind (N)),
                     "no field Status");
      Set_Field5 (N, Status);
   end Set_Status;

   function Get_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Chain (Get_Kind (N)),
                     "no field Chain");
      return Get_Field5 (N);
   end Get_Chain;

   procedure Set_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Chain (Get_Kind (N)),
                     "no field Chain");
      Set_Field5 (N, Chain);
   end Set_Chain;

   function Get_External_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_External_Chain (Get_Kind (N)),
                     "no field External_Chain");
      return Get_Field6 (N);
   end Get_External_Chain;

   procedure Set_External_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_External_Chain (Get_Kind (N)),
                     "no field External_Chain");
      Set_Field6 (N, Chain);
   end Set_External_Chain;

   function Get_Library_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Library_Chain (Get_Kind (N)),
                     "no field Library_Chain");
      return Get_Field7 (N);
   end Get_Library_Chain;

   procedure Set_Library_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Library_Chain (Get_Kind (N)),
                     "no field Library_Chain");
      Set_Field7 (N, Chain);
   end Set_Library_Chain;

   function Get_Cells_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cells_Chain (Get_Kind (N)),
                     "no field Cells_Chain");
      return Get_Field4 (N);
   end Get_Cells_Chain;

   procedure Set_Cells_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cells_Chain (Get_Kind (N)),
                     "no field Cells_Chain");
      Set_Field4 (N, Chain);
   end Set_Cells_Chain;

   function Get_Ports_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Ports_Chain (Get_Kind (N)),
                     "no field Ports_Chain");
      return Get_Field2 (N);
   end Get_Ports_Chain;

   procedure Set_Ports_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Ports_Chain (Get_Kind (N)),
                     "no field Ports_Chain");
      Set_Field2 (N, Chain);
   end Set_Ports_Chain;

   function Get_Contents_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Contents_Chain (Get_Kind (N)),
                     "no field Contents_Chain");
      return Get_Field4 (N);
   end Get_Contents_Chain;

   procedure Set_Contents_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Contents_Chain (Get_Kind (N)),
                     "no field Contents_Chain");
      Set_Field4 (N, Chain);
   end Set_Contents_Chain;

   function Get_Properties_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Properties_Chain (Get_Kind (N)),
                     "no field Properties_Chain");
      return Get_Field3 (N);
   end Get_Properties_Chain;

   procedure Set_Properties_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Properties_Chain (Get_Kind (N)),
                     "no field Properties_Chain");
      Set_Field3 (N, Chain);
   end Set_Properties_Chain;

   function Get_Port_Instances_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Port_Instances_Chain (Get_Kind (N)),
                     "no field Port_Instances_Chain");
      return Get_Field4 (N);
   end Get_Port_Instances_Chain;

   procedure Set_Port_Instances_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Port_Instances_Chain (Get_Kind (N)),
                     "no field Port_Instances_Chain");
      Set_Field4 (N, Chain);
   end Set_Port_Instances_Chain;

   function Get_Joined_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Joined_Chain (Get_Kind (N)),
                     "no field Joined_Chain");
      return Get_Field2 (N);
   end Get_Joined_Chain;

   procedure Set_Joined_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Joined_Chain (Get_Kind (N)),
                     "no field Joined_Chain");
      Set_Field2 (N, Chain);
   end Set_Joined_Chain;

   function Get_Design (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Design (Get_Kind (N)),
                     "no field Design");
      return Get_Field8 (N);
   end Get_Design;

   procedure Set_Design (N : Node; Design : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Design (Get_Kind (N)),
                     "no field Design");
      Set_Field8 (N, Design);
   end Set_Design;

   function Get_Designator (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Designator (Get_Kind (N)),
                     "no field Designator");
      return Get_Field4 (N);
   end Get_Designator;

   procedure Set_Designator (N : Node; Id : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Designator (Get_Kind (N)),
                     "no field Designator");
      Set_Field4 (N, Id);
   end Set_Designator;

   function Get_Technology (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Technology (Get_Kind (N)),
                     "no field Technology");
      return Get_Field3 (N);
   end Get_Technology;

   procedure Set_Technology (N : Node; Design : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Technology (Get_Kind (N)),
                     "no field Technology");
      Set_Field3 (N, Design);
   end Set_Technology;

   function Get_Cell_Type (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cell_Type (Get_Kind (N)),
                     "no field Cell_Type");
      return Name_Id'Val (Get_Field2 (N));
   end Get_Cell_Type;

   procedure Set_Cell_Type (N : Node; Ctype : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cell_Type (Get_Kind (N)),
                     "no field Cell_Type");
      Set_Field2 (N, Name_Id'Pos (Ctype));
   end Set_Cell_Type;

   function Get_View_Type (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_View_Type (Get_Kind (N)),
                     "no field View_Type");
      return Name_Id'Val (Get_Field2 (N));
   end Get_View_Type;

   procedure Set_View_Type (N : Node; Vtype : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_View_Type (Get_Kind (N)),
                     "no field View_Type");
      Set_Field2 (N, Name_Id'Pos (Vtype));
   end Set_View_Type;

   function Get_Interface (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Interface (Get_Kind (N)),
                     "no field Interface");
      return Get_Field6 (N);
   end Get_Interface;

   procedure Set_Interface (N : Node; Inter : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Interface (Get_Kind (N)),
                     "no field Interface");
      Set_Field6 (N, Inter);
   end Set_Interface;

   function Get_View_Ref (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_View_Ref (Get_Kind (N)),
                     "no field View_Ref");
      return Name_Id'Val (Get_Field1 (N));
   end Get_View_Ref;

   procedure Set_View_Ref (N : Node; Ref : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_View_Ref (Get_Kind (N)),
                     "no field View_Ref");
      Set_Field1 (N, Name_Id'Pos (Ref));
   end Set_View_Ref;

   function Get_Cell_Ref (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cell_Ref (Get_Kind (N)),
                     "no field Cell_Ref");
      return Get_Field2 (N);
   end Get_Cell_Ref;

   procedure Set_Cell_Ref (N : Node; Ref : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cell_Ref (Get_Kind (N)),
                     "no field Cell_Ref");
      Set_Field2 (N, Ref);
   end Set_Cell_Ref;

   function Get_Library_Ref (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Library_Ref (Get_Kind (N)),
                     "no field Library_Ref");
      return Get_Field2 (N);
   end Get_Library_Ref;

   procedure Set_Library_Ref (N : Node; Ref : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Library_Ref (Get_Kind (N)),
                     "no field Library_Ref");
      Set_Field2 (N, Ref);
   end Set_Library_Ref;

   function Get_View (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_View (Get_Kind (N)),
                     "no field View");
      return Get_Field4 (N);
   end Get_View;

   procedure Set_View (N : Node; View : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_View (Get_Kind (N)),
                     "no field View");
      Set_Field4 (N, View);
   end Set_View;

   function Get_Direction (N : Node) return Dir_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Direction (Get_Kind (N)),
                     "no field Direction");
      return Bit2_Type_To_Dir_Type (Get_State1 (N));
   end Get_Direction;

   procedure Set_Direction (N : Node; Dir : Dir_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Direction (Get_Kind (N)),
                     "no field Direction");
      Set_State1 (N, Dir_Type_To_Bit2_Type (Dir));
   end Set_Direction;

   function Get_Boolean (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Boolean (Get_Kind (N)),
                     "no field Boolean");
      return Get_Flag1 (N);
   end Get_Boolean;

   procedure Set_Boolean (N : Node; Val : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Boolean (Get_Kind (N)),
                     "no field Boolean");
      Set_Flag1 (N, Val);
   end Set_Boolean;

   function Get_Value (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Value (Get_Kind (N)),
                     "no field Value");
      return Get_Field2 (N);
   end Get_Value;

   procedure Set_Value (N : Node; Val : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Value (Get_Kind (N)),
                     "no field Value");
      Set_Field2 (N, Val);
   end Set_Value;

   function Get_Owner (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Owner (Get_Kind (N)),
                     "no field Owner");
      return Get_Field3 (N);
   end Get_Owner;

   procedure Set_Owner (N : Node; Owner : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Owner (Get_Kind (N)),
                     "no field Owner");
      Set_Field3 (N, Owner);
   end Set_Owner;

   function Get_Instance_Ref (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance_Ref (Get_Kind (N)),
                     "no field Instance_Ref");
      return Get_Field2 (N);
   end Get_Instance_Ref;

   procedure Set_Instance_Ref (N : Node; Ref : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance_Ref (Get_Kind (N)),
                     "no field Instance_Ref");
      Set_Field2 (N, Ref);
   end Set_Instance_Ref;

   function Get_Port (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Port (Get_Kind (N)),
                     "no field Port");
      return Get_Field1 (N);
   end Get_Port;

   procedure Set_Port (N : Node; Port : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Port (Get_Kind (N)),
                     "no field Port");
      Set_Field1 (N, Port);
   end Set_Port;

   function Get_Index (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Index (Get_Kind (N)),
                     "no field Index");
      return Node_To_Int32 (Get_Field2 (N));
   end Get_Index;

   procedure Set_Index (N : Node; Idx : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Index (Get_Kind (N)),
                     "no field Index");
      Set_Field2 (N, Int32_To_Node (Idx));
   end Set_Index;

   function Get_Array_Length (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Array_Length (Get_Kind (N)),
                     "no field Array_Length");
      return Node_To_Int32 (Get_Field2 (N));
   end Get_Array_Length;

   procedure Set_Array_Length (N : Node; Len : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Array_Length (Get_Kind (N)),
                     "no field Array_Length");
      Set_Field2 (N, Int32_To_Node (Len));
   end Set_Array_Length;

   function Get_Unit (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Unit (Get_Kind (N)),
                     "no field Unit");
      return Name_Id'Val (Get_Field4 (N));
   end Get_Unit;

   procedure Set_Unit (N : Node; Unit : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Unit (Get_Kind (N)),
                     "no field Unit");
      Set_Field4 (N, Name_Id'Pos (Unit));
   end Set_Unit;

   function Get_String (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String (Get_Kind (N)),
                     "no field String");
      return Get_Field2 (N);
   end Get_String;

   procedure Set_String (N : Node; Str : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String (Get_Kind (N)),
                     "no field String");
      Set_Field2 (N, Str);
   end Set_String;


end Edif.Nodes;
