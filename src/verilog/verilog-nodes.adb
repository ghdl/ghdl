--  This is in fact -*- Ada -*-
--  Verilog template file for nodes
--  Copyright (C) 2023 Tristan Gingold
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
with Verilog.Nodes_Meta; use Verilog.Nodes_Meta;
with Verilog.Nutils; use Verilog.Nutils;

package body Verilog.Nodes is
   type Format_Type is
     (
      Format_Short,
      Format_Medium
     );

   -- Common fields are:
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
   --   Flag19 : Boolean
   --   Nkind : Kind_Type
   --   State1 : Bit2_Type
   --   Field0 : Node
   --   Field1 : Node
   --   Field2 : Node
   --   Field3 : Node
   --   Field4 : Node
   --   Field5 : Node
   --   Field6 : Node

   -- Fields of Format_Short:

   -- Fields of Format_Medium:
   --   Odigit1 : Bit3_Type
   --   Odigit2 : Bit3_Type
   --   State3 : Bit2_Type
   --   State4 : Bit2_Type
   --   Field7 : Node (Field0)
   --   Field8 : Node
   --   Field9 : Node
   --   Field10 : Node
   --   Field11 : Node
   --   Field12 : Node
   --   Field13 : Node

   type State_Type is range 0 ..3;

   type Node_Record is record
      Kind : Nkind;      --  9 bits
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
      State1 : State_Type;  -- 2 bits

      Field0 : Node;
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
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   Init_Node : constant Node_Record :=
     (Kind => N_Error,
      Flag1 | Flag2 | Flag3 | Flag4 | Flag5 | Flag6 | Flag7 | Flag8 => False,
      Flag9 | Flag10 | Flag11 | Flag12 | Flag13 | Flag14 | Flag15 => False,
      Flag16 | Flag17 | Flag18 | Flag19 | Flag20 | Flag21 => False,
      State1 => 0,
      Field0 | Field1 | Field2 | Field3 | Field4 | Field5 | Field6 => 0);

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

   function Node_To_Width_Type is new Ada.Unchecked_Conversion
     (Source => Node, Target => Width_Type);
   function Width_Type_To_Node is new Ada.Unchecked_Conversion
     (Source => Width_Type, Target => Node);

   function Node_To_Tsize_Type is new Ada.Unchecked_Conversion
     (Source => Node, Target => Tsize_Type);
   function Tsize_Type_To_Node is new Ada.Unchecked_Conversion
     (Source => Tsize_Type, Target => Node);

   function Node_To_Bn_Index is new Ada.Unchecked_Conversion
     (Source => Node, Target => Bn_Index);
   function Bn_Index_To_Node is new Ada.Unchecked_Conversion
     (Source => Bn_Index, Target => Node);

   function Node_To_Location_Type (N : Node) return Location_Type is
   begin
      return Location_Type (N);
   end Node_To_Location_Type;

   function Location_Type_To_Node (L : Location_Type) return Node is
   begin
      return Node (L);
   end Location_Type_To_Node;


   function Drive_Strength_To_Int32 (D0, D1 : Drive_Strength_Type)
                                    return Int32
   is
   begin
      return Drive_Strength_Type'Pos (D1) * 16 + Drive_Strength_Type'Pos (D0);
   end Drive_Strength_To_Int32;

   function Get_Drive_Strength_0 (V : Int32) return Drive_Strength_Type is
   begin
      return Drive_Strength_Type'Val (V mod 16);
   end Get_Drive_Strength_0;

   function Get_Drive_Strength_1 (V : Int32) return Drive_Strength_Type is
   begin
      return Drive_Strength_Type'Val ((V / 16) mod 16);
   end Get_Drive_Strength_1;

   procedure Set_Kind (N : Node; K : Nkind) is
   begin
      Nodet.Table (N).Kind := K;
   end Set_Kind;

   function Get_Kind (N : Node) return Nkind is
   begin
      pragma Assert (N /= Null_Node, "get_kind: null node");
      return Nodet.Table (N).Kind;
   end Get_Kind;

   procedure Mutate_Instance (N : Node; Kind : Nkind) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Get_Kind (N) = N_Module_Instance);
      pragma Assert (Kind in Nkinds_Instance);
      Set_Kind (N, Kind);
   end Mutate_Instance;

   procedure Mutate_Port (N : Node; Kind : Nkind) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Get_Kind (N) in Nkinds_Net_Port);
      pragma Assert (Kind = N_Interface_Port or Kind = N_Modport_Port);
      Set_Kind (N, Kind);
   end Mutate_Port;

   procedure Mutate_Dotted_Name (N : Node; Kind : Nkind) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Nkind_In (Get_Kind (N), N_Dotted_Name, N_Hierarchical));
      pragma Assert (Kind = N_Hierarchical
                       or else Kind = N_Interface_Item
                       or else Kind = N_Modport_Item
                       or else Kind = N_Member_Name
                       or else Kind = N_Property_Name
                       or else Kind = N_Method_Name
                       or else Kind = N_Class_Qualified_Name);
      Set_Kind (N, Kind);
   end Mutate_Dotted_Name;

   procedure Mutate_Name (N : Node; Kind : Nkind) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Get_Kind (N) = N_Name);
      pragma Assert (Kind = N_This_Name);
      Set_Kind (N, Kind);
   end Mutate_Name;

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


   procedure Set_Flag3 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag3 := Flag;
   end Set_Flag3;

   function Get_Flag3 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag3;
   end Get_Flag3;


   procedure Set_Flag4 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag4 := Flag;
   end Set_Flag4;

   function Get_Flag4 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag4;
   end Get_Flag4;


   procedure Set_Flag5 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag5 := Flag;
   end Set_Flag5;

   function Get_Flag5 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag5;
   end Get_Flag5;


   procedure Set_Flag6 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag6 := Flag;
   end Set_Flag6;

   function Get_Flag6 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag6;
   end Get_Flag6;


   procedure Set_Flag7 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag7 := Flag;
   end Set_Flag7;

   function Get_Flag7 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag7;
   end Get_Flag7;


   procedure Set_Flag8 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag8 := Flag;
   end Set_Flag8;

   function Get_Flag8 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag8;
   end Get_Flag8;


   procedure Set_Flag9 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag9 := Flag;
   end Set_Flag9;

   function Get_Flag9 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag9;
   end Get_Flag9;


   procedure Set_Flag10 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag10 := Flag;
   end Set_Flag10;

   function Get_Flag10 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag10;
   end Get_Flag10;


   procedure Set_Flag11 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag11 := Flag;
   end Set_Flag11;

   function Get_Flag11 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag11;
   end Get_Flag11;


   procedure Set_Flag12 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag12 := Flag;
   end Set_Flag12;

   function Get_Flag12 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag12;
   end Get_Flag12;


   procedure Set_Flag13 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag13 := Flag;
   end Set_Flag13;

   function Get_Flag13 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag13;
   end Get_Flag13;


   procedure Set_Flag14 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag14 := Flag;
   end Set_Flag14;

   function Get_Flag14 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag14;
   end Get_Flag14;


   procedure Set_Flag19 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag19 := Flag;
   end Set_Flag19;

   function Get_Flag19 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag19;
   end Get_Flag19;


   procedure Set_State1 (N : Node; S : State_Type) is
   begin
      Nodet.Table (N).State1 := S;
   end Set_State1;

   function Get_State1 (N : Node) return State_Type is
   begin
      return Nodet.Table (N).State1;
   end Get_State1;


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


   procedure Set_Field7 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field0 := V;
   end Set_Field7;

   function Get_Field7 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field0;
   end Get_Field7;


   procedure Set_Field8 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field1 := V;
   end Set_Field8;

   function Get_Field8 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field1;
   end Get_Field8;


   procedure Set_Field9 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field2 := V;
   end Set_Field9;

   function Get_Field9 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field2;
   end Get_Field9;


   procedure Set_Field10 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field3 := V;
   end Set_Field10;

   function Get_Field10 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field3;
   end Get_Field10;


   procedure Set_Field11 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field4 := V;
   end Set_Field11;

   function Get_Field11 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field4;
   end Get_Field11;


   procedure Set_Field12 (N : Node; V : Node) is
   begin
      Nodet.Table (N + 1).Field5 := V;
   end Set_Field12;

   function Get_Field12 (N : Node) return Node is
   begin
      return Nodet.Table (N + 1).Field5;
   end Get_Field12;


   function Get_Format (Kind : Nkind) return Format_Type;

   function Create_Node (Kind : Nkind) return Node
   is
      Res : Node;
   begin
      case Get_Format (Kind) is
         when Format_Medium =>
            Nodet.Increment_Last;
            Res := Nodet.Last;
            Nodet.Increment_Last;
            Nodet.Table (Res) := Init_Node;
            Nodet.Table (Res + 1) := Init_Node;
         when Format_Short =>
            if Free_Nodes /= Null_Node then
               Res := Free_Nodes;
               Free_Nodes := Get_Field1 (Res);
            else
               Nodet.Increment_Last;
               Res := Nodet.Last;
            end if;
      end case;
      Nodet.Table (Res) := Init_Node;
      Set_Kind (Res, Kind);
      return Res;
   end Create_Node;

   procedure Free_Node (N : Node)
   is
      Kind : Nkind;
   begin
      if N = Null_Node then
         return;
      end if;
      Kind := Get_Kind (N);
      pragma Assert (Kind /= N_Error);

      --  TODO: format medium.
      case Get_Format (Kind) is
         when Format_Medium =>
            Set_Kind (N, N_Error);
            Set_Field1 (N, Free_Nodes);
            Free_Nodes := N;

            Set_Kind (N + 1, N_Error);
            Set_Field1 (N + 1, Free_Nodes);
            Free_Nodes := N + 1;
         when Format_Short =>
            Set_Kind (N, N_Error);
            Set_Field1 (N, Free_Nodes);
            Free_Nodes := N;
      end case;
   end Free_Node;

   function Get_Location (N : Node) return Location_Type is
   begin
      return Node_To_Location_Type (Get_Field0 (N));
   end Get_Location;

   procedure Set_Location (N : Node; Loc : Location_Type) is
   begin
      Set_Field0 (N, Location_Type_To_Node (Loc));
   end Set_Location;

   function Boolean_To_Lifetime_Type (F : Boolean) return Lifetime_Type is
   begin
      return Lifetime_Type'Val (Boolean'Pos (F));
   end Boolean_To_Lifetime_Type;

   function Lifetime_Type_To_Boolean (L : Lifetime_Type) return Boolean is
   begin
      return Boolean'Val (Lifetime_Type'Pos (L));
   end Lifetime_Type_To_Boolean;

   --  Subprograms
   function Get_Format (Kind : Nkind) return Format_Type is
   begin
      case Kind is
         when N_Error
           | N_Error_Expr
           | N_Timescale_Directive
           | N_Timeunits_Declaration
           | N_Timeunit
           | N_Timeprecision
           | N_Logic_Type
           | N_Bit_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Packed_Array
           | N_Array
           | N_Struct_Type
           | N_Packed_Struct_Type
           | N_Union_Type
           | N_Packed_Union_Type
           | N_Queue
           | N_Queue_Cst
           | N_Dynamic_Array_Cst
           | N_Dynamic_Array
           | N_Associative_Array
           | N_Associative_Array_Cst
           | N_Enum_Type
           | N_String_Type
           | N_Chandle_Type
           | N_Event_Type
           | N_Void_Type
           | N_Error_Type
           | N_Null_Type
           | N_Nature
           | N_Wildcard_Type
           | N_Compilation_Unit
           | N_Port
           | N_Export_DPI_Function
           | N_Export_DPI_Task
           | N_Clocking
           | N_Default_Clocking
           | N_Disable_Iff
           | N_Specify
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Type_Parameter
           | N_Type_Localparam
           | N_Var
           | N_Return_Var
           | N_This_Var
           | N_Iterator_Argument
           | N_Wire_Direct
           | N_Typedef
           | N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward
           | N_Predefined_Typedef
           | N_Package_Import
           | N_Genvar
           | N_Enum_Name
           | N_Enum_Range
           | N_Foreach_Variable
           | N_Clock_Var
           | N_Modport
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Modport_Clocking
           | N_Modport_Import_Tf
           | N_Modport_Export_Tf
           | N_Constraint
           | N_Constraint_Expression
           | N_Constraint_If
           | N_Constraint_Foreach
           | N_Discipline
           | N_Branch
           | N_Port_Branch
           | N_Nature_Attribute
           | N_Nature_Access
           | N_Discipline_Domain
           | N_Discipline_Potential
           | N_Discipline_Flow
           | N_Discipline_Attribute
           | N_From_Range
           | N_Exclude_Range
           | N_Assign
           | N_Decl_Assign
           | N_Always
           | N_Always_Comb
           | N_Always_Latch
           | N_Always_Ff
           | N_Initial
           | N_Final
           | N_Debug
           | N_Parameter_Value_Type
           | N_Parameter_Value_Expr
           | N_Defparam
           | N_Generate_Region
           | N_Loop_Generate
           | N_If_Generate
           | N_Case_Generate
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block
           | N_Analog
           | N_Default_Skew
           | N_Clocking_Skew
           | N_Control_Terminal
           | N_Input_Terminal
           | N_Inout_Terminal
           | N_Output_Terminal
           | N_Port_Connection
           | N_Wildcard_Connection
           | N_Implicit_Connection
           | N_Default_Connection
           | N_If
           | N_For
           | N_While
           | N_Do_While
           | N_Foreach
           | N_Repeat
           | N_Forever
           | N_Wait
           | N_Wait_Fork
           | N_Trigger
           | N_Disable
           | N_Disable_Fork
           | N_Proc_Assign
           | N_Proc_Deassign
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Unpack_Assign
           | N_Pack_Assign
           | N_Pack_Unpack_Assign
           | N_Assign_Operator
           | N_Force_Assign
           | N_Release
           | N_Case
           | N_Casex
           | N_Casez
           | N_Case_Item
           | N_Default_Case_Item
           | N_Subroutine_Call_Stmt
           | N_Return_Stmt
           | N_Break_Stmt
           | N_Continue_Stmt
           | N_Label_Stmt
           | N_Simple_Immediate_Assert
           | N_Argument
           | N_Contribution
           | N_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Wildcard_Name
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_Number
           | N_Computed_Number
           | N_Bignum
           | N_Unbased_Literal
           | N_Time_Literal
           | N_Step_Literal
           | N_Infinity
           | N_Real_Number
           | N_Scale_Number
           | N_Mintypmax
           | N_Bit_Select
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_Slice_Name
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Slice_Name_Cst
           | N_Member_Select
           | N_String_Literal
           | N_Implicit_Event
           | N_New_Call
           | N_New_Expression
           | N_Dynamic_Array_New
           | N_Parenthesis_Expr
           | N_Type_Cast
           | N_Size_Cast
           | N_Null
           | N_This
           | N_Super
           | N_Default
           | N_Aggregate_Literal
           | N_Aggregate_Literal_Cst
           | N_Aggregate_Element
           | N_Event_Control
           | N_Delay_Control
           | N_Repeat_Control
           | N_Cycle_Delay
           | N_Posedge
           | N_Negedge
           | N_Or
           | N_Delay
           | N_Element
           | N_Value_Range
           | N_Stream_Expression
           | N_Left_Streaming_Expr
           | N_Right_Streaming_Expr
           | N_Left_Streaming_Type
           | N_Right_Streaming_Type
           | N_Concatenation
           | N_Membership
           | N_Replication_Cst
           | N_Cond_Op
           | N_Call
           | N_Array_Method_Call
           | N_Randomize_Call
           | N_System_Call
           | N_Bits_Expr
           | N_Bits_Type
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op
           | N_Post_Increment
           | N_Pre_Increment
           | N_Post_Decrement
           | N_Pre_Decrement
           | N_Access_Call
           | N_Conversion
           | N_Seq_Repeat
           | N_Seq_Plus_Repeat
           | N_Seq_Star_Repeat
           | N_Seq_Star_Concat
           | N_Seq_Plus_Concat
           | N_Seq_Const_Concat
           | N_Seq_Range_Concat
           | N_Seq_Throughout
           | N_Seq_Parenthesis
           | N_Prop_Not
           | N_Prop_Or
           | N_Prop_And
           | N_Prop_Overlap_Imp
           | N_Prop_Non_Overlap_Imp
           | N_Prop_Until
           | N_Ifnone
           | N_Timing_Check
           | N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path
           | N_Path_Element
           | N_Path_Delay3
           | N_Path_Delay6
           | N_Udp_Combinational_Entry
           | N_Udp_Sequential_Entry
           | N_Udp_Level_Symbol
           | N_Udp_Change_Symbol
           | N_Attribute
           | N_Label
           | N_Goto =>
            return Format_Short;
         when N_Virtual_Interface
           | N_Class
           | N_Instantiated_Class
           | N_Class_Instance
           | N_Generic_Class
           | N_Foreign_Module
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Package
           | N_Program_Declaration
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Property_Declaration
           | N_Input
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Localparam
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Module_Instance
           | N_Primitive_Instance
           | N_Interface_Instance
           | N_Program_Instance
           | N_Assert_Property
           | N_Assume_Property
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown
           | N_Seq_Block
           | N_Par_Block
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Path_Delay12
           | N_Member
           | N_Packed_Member =>
            return Format_Medium;
      end case;
   end Get_Format;

   function Get_Parent (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parent (Get_Kind (N)),
                     "no field Parent");
      return Get_Field6 (N);
   end Get_Parent;

   procedure Set_Parent (N : Node; Parent : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parent (Get_Kind (N)),
                     "no field Parent");
      Set_Field6 (N, Parent);
   end Set_Parent;

   function Get_Call_Scope (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Call_Scope (Get_Kind (N)),
                     "no field Call_Scope");
      return Get_Field2 (N);
   end Get_Call_Scope;

   procedure Set_Call_Scope (N : Node; Scope : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Call_Scope (Get_Kind (N)),
                     "no field Call_Scope");
      Set_Field2 (N, Scope);
   end Set_Call_Scope;

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

   function Get_C_Identifier (N : Node) return Name_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_C_Identifier (Get_Kind (N)),
                     "no field C_Identifier");
      return Name_Id'Val (Get_Field4 (N));
   end Get_C_Identifier;

   procedure Set_C_Identifier (N : Node; Id : Name_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_C_Identifier (Get_Kind (N)),
                     "no field C_Identifier");
      Set_Field4 (N, Name_Id'Pos (Id));
   end Set_C_Identifier;

   function Get_Ports_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Ports_Chain (Get_Kind (N)),
                     "no field Ports_Chain");
      return Get_Field7 (N);
   end Get_Ports_Chain;

   procedure Set_Ports_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Ports_Chain (Get_Kind (N)),
                     "no field Ports_Chain");
      Set_Field7 (N, Chain);
   end Set_Ports_Chain;

   function Get_Tf_Ports_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Tf_Ports_Chain (Get_Kind (N)),
                     "no field Tf_Ports_Chain");
      return Get_Field7 (N);
   end Get_Tf_Ports_Chain;

   procedure Set_Tf_Ports_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Tf_Ports_Chain (Get_Kind (N)),
                     "no field Tf_Ports_Chain");
      Set_Field7 (N, Chain);
   end Set_Tf_Ports_Chain;

   function Get_Package_Import_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Package_Import_Chain (Get_Kind (N)),
                     "no field Package_Import_Chain");
      return Get_Field10 (N);
   end Get_Package_Import_Chain;

   procedure Set_Package_Import_Chain (N : Node; Imp : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Package_Import_Chain (Get_Kind (N)),
                     "no field Package_Import_Chain");
      Set_Field10 (N, Imp);
   end Set_Package_Import_Chain;

   function Get_Parameter_Port_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Port_Chain (Get_Kind (N)),
                     "no field Parameter_Port_Chain");
      return Get_Field3 (N);
   end Get_Parameter_Port_Chain;

   procedure Set_Parameter_Port_Chain (N : Node; Port : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Port_Chain (Get_Kind (N)),
                     "no field Parameter_Port_Chain");
      Set_Field3 (N, Port);
   end Set_Parameter_Port_Chain;

   function Get_Parameter (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter (Get_Kind (N)),
                     "no field Parameter");
      return Get_Field5 (N);
   end Get_Parameter;

   procedure Set_Parameter (N : Node; Decl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter (Get_Kind (N)),
                     "no field Parameter");
      Set_Field5 (N, Decl);
   end Set_Parameter;

   function Get_Foreign_Node (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Foreign_Node (Get_Kind (N)),
                     "no field Foreign_Node");
      return Node_To_Int32 (Get_Field4 (N));
   end Get_Foreign_Node;

   procedure Set_Foreign_Node (N : Node; Fn : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Foreign_Node (Get_Kind (N)),
                     "no field Foreign_Node");
      Set_Field4 (N, Int32_To_Node (Fn));
   end Set_Foreign_Node;

   function Get_Descriptions (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Descriptions (Get_Kind (N)),
                     "no field Descriptions");
      return Get_Field3 (N);
   end Get_Descriptions;

   procedure Set_Descriptions (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Descriptions (Get_Kind (N)),
                     "no field Descriptions");
      Set_Field3 (N, Chain);
   end Set_Descriptions;

   function Get_Class_Item_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Item_Chain (Get_Kind (N)),
                     "no field Class_Item_Chain");
      return Get_Field7 (N);
   end Get_Class_Item_Chain;

   procedure Set_Class_Item_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Item_Chain (Get_Kind (N)),
                     "no field Class_Item_Chain");
      Set_Field7 (N, Items);
   end Set_Class_Item_Chain;

   function Get_Package_Item_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Package_Item_Chain (Get_Kind (N)),
                     "no field Package_Item_Chain");
      return Get_Field5 (N);
   end Get_Package_Item_Chain;

   procedure Set_Package_Item_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Package_Item_Chain (Get_Kind (N)),
                     "no field Package_Item_Chain");
      Set_Field5 (N, Items);
   end Set_Package_Item_Chain;

   function Get_Items_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Items_Chain (Get_Kind (N)),
                     "no field Items_Chain");
      return Get_Field8 (N);
   end Get_Items_Chain;

   procedure Set_Items_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Items_Chain (Get_Kind (N)),
                     "no field Items_Chain");
      Set_Field8 (N, Items);
   end Set_Items_Chain;

   function Get_Clocking_Item_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Clocking_Item_Chain (Get_Kind (N)),
                     "no field Clocking_Item_Chain");
      return Get_Field5 (N);
   end Get_Clocking_Item_Chain;

   procedure Set_Clocking_Item_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Clocking_Item_Chain (Get_Kind (N)),
                     "no field Clocking_Item_Chain");
      Set_Field5 (N, Items);
   end Set_Clocking_Item_Chain;

   function Get_Tf_Item_Declaration_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Tf_Item_Declaration_Chain (Get_Kind (N)),
                     "no field Tf_Item_Declaration_Chain");
      return Get_Field9 (N);
   end Get_Tf_Item_Declaration_Chain;

   procedure Set_Tf_Item_Declaration_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Tf_Item_Declaration_Chain (Get_Kind (N)),
                     "no field Tf_Item_Declaration_Chain");
      Set_Field9 (N, Items);
   end Set_Tf_Item_Declaration_Chain;

   function Get_Block_Item_Declaration_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Block_Item_Declaration_Chain (Get_Kind (N)),
                     "no field Block_Item_Declaration_Chain");
      return Get_Field3 (N);
   end Get_Block_Item_Declaration_Chain;

   procedure Set_Block_Item_Declaration_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Block_Item_Declaration_Chain (Get_Kind (N)),
                     "no field Block_Item_Declaration_Chain");
      Set_Field3 (N, Items);
   end Set_Block_Item_Declaration_Chain;

   function Get_Generate_Item_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Generate_Item_Chain (Get_Kind (N)),
                     "no field Generate_Item_Chain");
      return Get_Field4 (N);
   end Get_Generate_Item_Chain;

   procedure Set_Generate_Item_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Generate_Item_Chain (Get_Kind (N)),
                     "no field Generate_Item_Chain");
      Set_Field4 (N, Items);
   end Set_Generate_Item_Chain;

   function Get_Specify_Item_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Specify_Item_Chain (Get_Kind (N)),
                     "no field Specify_Item_Chain");
      return Get_Field5 (N);
   end Get_Specify_Item_Chain;

   procedure Set_Specify_Item_Chain (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Specify_Item_Chain (Get_Kind (N)),
                     "no field Specify_Item_Chain");
      Set_Field5 (N, Items);
   end Set_Specify_Item_Chain;

   function Get_Statements_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Statements_Chain (Get_Kind (N)),
                     "no field Statements_Chain");
      return Get_Field4 (N);
   end Get_Statements_Chain;

   procedure Set_Statements_Chain (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Statements_Chain (Get_Kind (N)),
                     "no field Statements_Chain");
      Set_Field4 (N, Stmt);
   end Set_Statements_Chain;

   function Get_Modport_Ports_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Modport_Ports_Chain (Get_Kind (N)),
                     "no field Modport_Ports_Chain");
      return Get_Field3 (N);
   end Get_Modport_Ports_Chain;

   procedure Set_Modport_Ports_Chain (N : Node; Ports : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Modport_Ports_Chain (Get_Kind (N)),
                     "no field Modport_Ports_Chain");
      Set_Field3 (N, Ports);
   end Set_Modport_Ports_Chain;

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

   function Get_Constraint_Block_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Constraint_Block_Chain (Get_Kind (N)),
                     "no field Constraint_Block_Chain");
      return Get_Field5 (N);
   end Get_Constraint_Block_Chain;

   procedure Set_Constraint_Block_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Constraint_Block_Chain (Get_Kind (N)),
                     "no field Constraint_Block_Chain");
      Set_Field5 (N, Chain);
   end Set_Constraint_Block_Chain;

   function Get_Constraint_Set (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Constraint_Set (Get_Kind (N)),
                     "no field Constraint_Set");
      return Get_Field5 (N);
   end Get_Constraint_Set;

   procedure Set_Constraint_Set (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Constraint_Set (Get_Kind (N)),
                     "no field Constraint_Set");
      Set_Field5 (N, Chain);
   end Set_Constraint_Set;

   function Get_OOB_Prefix (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_OOB_Prefix (Get_Kind (N)),
                     "no field OOB_Prefix");
      return Get_Field10 (N);
   end Get_OOB_Prefix;

   procedure Set_OOB_Prefix (N : Node; Prefix : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_OOB_Prefix (Get_Kind (N)),
                     "no field OOB_Prefix");
      Set_Field10 (N, Prefix);
   end Set_OOB_Prefix;

   function Get_Out_Of_Block_Declaration (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Out_Of_Block_Declaration (Get_Kind (N)),
                     "no field Out_Of_Block_Declaration");
      return Get_Field10 (N);
   end Get_Out_Of_Block_Declaration;

   procedure Set_Out_Of_Block_Declaration (N : Node; Decl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Out_Of_Block_Declaration (Get_Kind (N)),
                     "no field Out_Of_Block_Declaration");
      Set_Field10 (N, Decl);
   end Set_Out_Of_Block_Declaration;

   function Get_Generate_Index (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Generate_Index (Get_Kind (N)),
                     "no field Generate_Index");
      return Node_To_Int32 (Get_Field5 (N));
   end Get_Generate_Index;

   procedure Set_Generate_Index (N : Node; Idx : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Generate_Index (Get_Kind (N)),
                     "no field Generate_Index");
      Set_Field5 (N, Int32_To_Node (Idx));
   end Set_Generate_Index;

   function Get_Return_Variable (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Return_Variable (Get_Kind (N)),
                     "no field Return_Variable");
      return Get_Field8 (N);
   end Get_Return_Variable;

   procedure Set_Return_Variable (N : Node; Var : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Return_Variable (Get_Kind (N)),
                     "no field Return_Variable");
      Set_Field8 (N, Var);
   end Set_Return_Variable;

   function Get_Return_Variable_Ref (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Return_Variable_Ref (Get_Kind (N)),
                     "no field Return_Variable_Ref");
      return Get_Field3 (N);
   end Get_Return_Variable_Ref;

   procedure Set_Return_Variable_Ref (N : Node; Var : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Return_Variable_Ref (Get_Kind (N)),
                     "no field Return_Variable_Ref");
      Set_Field3 (N, Var);
   end Set_Return_Variable_Ref;

   function Get_This_Variable (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_This_Variable (Get_Kind (N)),
                     "no field This_Variable");
      return Get_Field11 (N);
   end Get_This_Variable;

   procedure Set_This_Variable (N : Node; Var : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_This_Variable (Get_Kind (N)),
                     "no field This_Variable");
      Set_Field11 (N, Var);
   end Set_This_Variable;

   function Get_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expression (Get_Kind (N)),
                     "no field Expression");
      return Get_Field4 (N);
   end Get_Expression;

   procedure Set_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expression (Get_Kind (N)),
                     "no field Expression");
      Set_Field4 (N, Expr);
   end Set_Expression;

   function Get_Reject_Limit (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Reject_Limit (Get_Kind (N)),
                     "no field Reject_Limit");
      return Get_Field4 (N);
   end Get_Reject_Limit;

   procedure Set_Reject_Limit (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Reject_Limit (Get_Kind (N)),
                     "no field Reject_Limit");
      Set_Field4 (N, Expr);
   end Set_Reject_Limit;

   function Get_Error_Limit (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Error_Limit (Get_Kind (N)),
                     "no field Error_Limit");
      return Get_Field7 (N);
   end Get_Error_Limit;

   procedure Set_Error_Limit (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Error_Limit (Get_Kind (N)),
                     "no field Error_Limit");
      Set_Field7 (N, Expr);
   end Set_Error_Limit;

   function Get_Sequence (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Sequence (Get_Kind (N)),
                     "no field Sequence");
      return Get_Field4 (N);
   end Get_Sequence;

   procedure Set_Sequence (N : Node; Seq : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Sequence (Get_Kind (N)),
                     "no field Sequence");
      Set_Field4 (N, Seq);
   end Set_Sequence;

   function Get_Init_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Init_Expression (Get_Kind (N)),
                     "no field Init_Expression");
      return Get_Field4 (N);
   end Get_Init_Expression;

   procedure Set_Init_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Init_Expression (Get_Kind (N)),
                     "no field Init_Expression");
      Set_Field4 (N, Expr);
   end Set_Init_Expression;

   function Get_Size_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Size_Expression (Get_Kind (N)),
                     "no field Size_Expression");
      return Get_Field5 (N);
   end Get_Size_Expression;

   procedure Set_Size_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Size_Expression (Get_Kind (N)),
                     "no field Size_Expression");
      Set_Field5 (N, Expr);
   end Set_Size_Expression;

   function Get_Override_Stmt (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Override_Stmt (Get_Kind (N)),
                     "no field Override_Stmt");
      return Get_Field7 (N);
   end Get_Override_Stmt;

   procedure Set_Override_Stmt (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Override_Stmt (Get_Kind (N)),
                     "no field Override_Stmt");
      Set_Field7 (N, Stmt);
   end Set_Override_Stmt;

   function Get_Parameter_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Expression (Get_Kind (N)),
                     "no field Parameter_Expression");
      return Get_Field8 (N);
   end Get_Parameter_Expression;

   procedure Set_Parameter_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Expression (Get_Kind (N)),
                     "no field Parameter_Expression");
      Set_Field8 (N, Expr);
   end Set_Parameter_Expression;

   function Get_Parameter_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Type (Get_Kind (N)),
                     "no field Parameter_Type");
      return Get_Field4 (N);
   end Get_Parameter_Type;

   procedure Set_Parameter_Type (N : Node; Typ : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Type (Get_Kind (N)),
                     "no field Parameter_Type");
      Set_Field4 (N, Typ);
   end Set_Parameter_Type;

   function Get_Value_Range (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Value_Range (Get_Kind (N)),
                     "no field Value_Range");
      return Get_Field10 (N);
   end Get_Value_Range;

   procedure Set_Value_Range (N : Node; Rng : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Value_Range (Get_Kind (N)),
                     "no field Value_Range");
      Set_Field10 (N, Rng);
   end Set_Value_Range;

   function Get_Lsb_Include_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lsb_Include_Flag (Get_Kind (N)),
                     "no field Lsb_Include_Flag");
      return Get_Flag1 (N);
   end Get_Lsb_Include_Flag;

   procedure Set_Lsb_Include_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lsb_Include_Flag (Get_Kind (N)),
                     "no field Lsb_Include_Flag");
      Set_Flag1 (N, Flag);
   end Set_Lsb_Include_Flag;

   function Get_Msb_Include_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Msb_Include_Flag (Get_Kind (N)),
                     "no field Msb_Include_Flag");
      return Get_Flag2 (N);
   end Get_Msb_Include_Flag;

   procedure Set_Msb_Include_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Msb_Include_Flag (Get_Kind (N)),
                     "no field Msb_Include_Flag");
      Set_Flag2 (N, Flag);
   end Set_Msb_Include_Flag;

   function Get_Range (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Range (Get_Kind (N)),
                     "no field Range");
      return Get_Field3 (N);
   end Get_Range;

   procedure Set_Range (N : Node; Rng : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Range (Get_Kind (N)),
                     "no field Range");
      Set_Field3 (N, Rng);
   end Set_Range;

   function Get_Msb (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Msb (Get_Kind (N)),
                     "no field Msb");
      return Get_Field5 (N);
   end Get_Msb;

   procedure Set_Msb (N : Node; Msb : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Msb (Get_Kind (N)),
                     "no field Msb");
      Set_Field5 (N, Msb);
   end Set_Msb;

   function Get_Lsb (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lsb (Get_Kind (N)),
                     "no field Lsb");
      return Get_Field6 (N);
   end Get_Lsb;

   procedure Set_Lsb (N : Node; Lsb : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lsb (Get_Kind (N)),
                     "no field Lsb");
      Set_Field6 (N, Lsb);
   end Set_Lsb;

   function Get_Msb_Cst (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Msb_Cst (Get_Kind (N)),
                     "no field Msb_Cst");
      return Node_To_Int32 (Get_Field5 (N));
   end Get_Msb_Cst;

   procedure Set_Msb_Cst (N : Node; Msb : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Msb_Cst (Get_Kind (N)),
                     "no field Msb_Cst");
      Set_Field5 (N, Int32_To_Node (Msb));
   end Set_Msb_Cst;

   function Get_Lsb_Cst (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lsb_Cst (Get_Kind (N)),
                     "no field Lsb_Cst");
      return Node_To_Int32 (Get_Field6 (N));
   end Get_Lsb_Cst;

   procedure Set_Lsb_Cst (N : Node; Lsb : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lsb_Cst (Get_Kind (N)),
                     "no field Lsb_Cst");
      Set_Field6 (N, Int32_To_Node (Lsb));
   end Set_Lsb_Cst;

   function Get_Base_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Base_Expr (Get_Kind (N)),
                     "no field Base_Expr");
      return Get_Field4 (N);
   end Get_Base_Expr;

   procedure Set_Base_Expr (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Base_Expr (Get_Kind (N)),
                     "no field Base_Expr");
      Set_Field4 (N, Expr);
   end Set_Base_Expr;

   function Get_Width_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Width_Expr (Get_Kind (N)),
                     "no field Width_Expr");
      return Get_Field5 (N);
   end Get_Width_Expr;

   procedure Set_Width_Expr (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Width_Expr (Get_Kind (N)),
                     "no field Width_Expr");
      Set_Field5 (N, Expr);
   end Set_Width_Expr;

   function Get_Width_Cst (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Width_Cst (Get_Kind (N)),
                     "no field Width_Cst");
      return Node_To_Int32 (Get_Field5 (N));
   end Get_Width_Cst;

   procedure Set_Width_Cst (N : Node; Expr : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Width_Cst (Get_Kind (N)),
                     "no field Width_Cst");
      Set_Field5 (N, Int32_To_Node (Expr));
   end Set_Width_Cst;

   function Get_Type_Width (N : Node) return Width_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Width (Get_Kind (N)),
                     "no field Type_Width");
      return Node_To_Width_Type (Get_Field4 (N));
   end Get_Type_Width;

   procedure Set_Type_Width (N : Node; Width : Width_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Width (Get_Kind (N)),
                     "no field Type_Width");
      Set_Field4 (N, Width_Type_To_Node (Width));
   end Set_Type_Width;

   function Get_Type_Size (N : Node) return Tsize_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Size (Get_Kind (N)),
                     "no field Type_Size");
      return Node_To_Tsize_Type (Get_Field4 (N));
   end Get_Type_Size;

   procedure Set_Type_Size (N : Node; Width : Tsize_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Size (Get_Kind (N)),
                     "no field Type_Size");
      Set_Field4 (N, Tsize_Type_To_Node (Width));
   end Set_Type_Size;

   function Get_Stride_Width (N : Node) return Width_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Stride_Width (Get_Kind (N)),
                     "no field Stride_Width");
      return Node_To_Width_Type (Get_Field1 (N));
   end Get_Stride_Width;

   procedure Set_Stride_Width (N : Node; Width : Width_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Stride_Width (Get_Kind (N)),
                     "no field Stride_Width");
      Set_Field1 (N, Width_Type_To_Node (Width));
   end Set_Stride_Width;

   function Get_Stride_Size (N : Node) return Tsize_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Stride_Size (Get_Kind (N)),
                     "no field Stride_Size");
      return Node_To_Tsize_Type (Get_Field1 (N));
   end Get_Stride_Size;

   procedure Set_Stride_Size (N : Node; Width : Tsize_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Stride_Size (Get_Kind (N)),
                     "no field Stride_Size");
      Set_Field1 (N, Tsize_Type_To_Node (Width));
   end Set_Stride_Size;

   function Get_Type_Hash (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Hash (Get_Kind (N)),
                     "no field Type_Hash");
      return Node_To_Uns32 (Get_Field2 (N));
   end Get_Type_Hash;

   procedure Set_Type_Hash (N : Node; Width : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Hash (Get_Kind (N)),
                     "no field Type_Hash");
      Set_Field2 (N, Uns32_To_Node (Width));
   end Set_Type_Hash;

   function Get_Maximum_Size_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Maximum_Size_Expr (Get_Kind (N)),
                     "no field Maximum_Size_Expr");
      return Get_Field1 (N);
   end Get_Maximum_Size_Expr;

   procedure Set_Maximum_Size_Expr (N : Node; Size : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Maximum_Size_Expr (Get_Kind (N)),
                     "no field Maximum_Size_Expr");
      Set_Field1 (N, Size);
   end Set_Maximum_Size_Expr;

   function Get_Maximum_Size_Cst (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Maximum_Size_Cst (Get_Kind (N)),
                     "no field Maximum_Size_Cst");
      return Node_To_Int32 (Get_Field1 (N));
   end Get_Maximum_Size_Cst;

   procedure Set_Maximum_Size_Cst (N : Node; Size : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Maximum_Size_Cst (Get_Kind (N)),
                     "no field Maximum_Size_Cst");
      Set_Field1 (N, Int32_To_Node (Size));
   end Set_Maximum_Size_Cst;

   function Get_Lvalue (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lvalue (Get_Kind (N)),
                     "no field Lvalue");
      return Get_Field1 (N);
   end Get_Lvalue;

   procedure Set_Lvalue (N : Node; Val : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lvalue (Get_Kind (N)),
                     "no field Lvalue");
      Set_Field1 (N, Val);
   end Set_Lvalue;

   function Get_Name (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Name (Get_Kind (N)),
                     "no field Name");
      return Get_Field2 (N);
   end Get_Name;

   procedure Set_Name (N : Node; Ref : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Name (Get_Kind (N)),
                     "no field Name");
      Set_Field2 (N, Ref);
   end Set_Name;

   function Get_Item_Name (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Item_Name (Get_Kind (N)),
                     "no field Item_Name");
      return Get_Field5 (N);
   end Get_Item_Name;

   procedure Set_Item_Name (N : Node; Ref : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Item_Name (Get_Kind (N)),
                     "no field Item_Name");
      Set_Field5 (N, Ref);
   end Set_Item_Name;

   function Get_Pattern_Key (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pattern_Key (Get_Kind (N)),
                     "no field Pattern_Key");
      return Get_Field5 (N);
   end Get_Pattern_Key;

   procedure Set_Pattern_Key (N : Node; Ref : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pattern_Key (Get_Kind (N)),
                     "no field Pattern_Key");
      Set_Field5 (N, Ref);
   end Set_Pattern_Key;

   function Get_Left (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Left (Get_Kind (N)),
                     "no field Left");
      return Get_Field1 (N);
   end Get_Left;

   procedure Set_Left (N : Node; Val : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Left (Get_Kind (N)),
                     "no field Left");
      Set_Field1 (N, Val);
   end Set_Left;

   function Get_Right (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Right (Get_Kind (N)),
                     "no field Right");
      return Get_Field4 (N);
   end Get_Right;

   procedure Set_Right (N : Node; Val : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Right (Get_Kind (N)),
                     "no field Right");
      Set_Field4 (N, Val);
   end Set_Right;

   function Get_Repeat_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Repeat_Expression (Get_Kind (N)),
                     "no field Repeat_Expression");
      return Get_Field5 (N);
   end Get_Repeat_Expression;

   procedure Set_Repeat_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Repeat_Expression (Get_Kind (N)),
                     "no field Repeat_Expression");
      Set_Field5 (N, Expr);
   end Set_Repeat_Expression;

   function Get_Op_Attributes (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Op_Attributes (Get_Kind (N)),
                     "no field Op_Attributes");
      return Get_Field2 (N);
   end Get_Op_Attributes;

   procedure Set_Op_Attributes (N : Node; Attrs : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Op_Attributes (Get_Kind (N)),
                     "no field Op_Attributes");
      Set_Field2 (N, Attrs);
   end Set_Op_Attributes;

   function Get_Attributes_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attributes_Chain (Get_Kind (N)),
                     "no field Attributes_Chain");
      return Get_Field9 (N);
   end Get_Attributes_Chain;

   procedure Set_Attributes_Chain (N : Node; Attrs : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attributes_Chain (Get_Kind (N)),
                     "no field Attributes_Chain");
      Set_Field9 (N, Attrs);
   end Set_Attributes_Chain;

   function Get_Condition (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Condition (Get_Kind (N)),
                     "no field Condition");
      return Get_Field5 (N);
   end Get_Condition;

   procedure Set_Condition (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Condition (Get_Kind (N)),
                     "no field Condition");
      Set_Field5 (N, Expr);
   end Set_Condition;

   function Get_Cond_True (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cond_True (Get_Kind (N)),
                     "no field Cond_True");
      return Get_Field1 (N);
   end Get_Cond_True;

   procedure Set_Cond_True (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cond_True (Get_Kind (N)),
                     "no field Cond_True");
      Set_Field1 (N, Expr);
   end Set_Cond_True;

   function Get_Cond_False (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cond_False (Get_Kind (N)),
                     "no field Cond_False");
      return Get_Field4 (N);
   end Get_Cond_False;

   procedure Set_Cond_False (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cond_False (Get_Kind (N)),
                     "no field Cond_False");
      Set_Field4 (N, Expr);
   end Set_Cond_False;

   function Get_True_Stmt (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_True_Stmt (Get_Kind (N)),
                     "no field True_Stmt");
      return Get_Field3 (N);
   end Get_True_Stmt;

   procedure Set_True_Stmt (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_True_Stmt (Get_Kind (N)),
                     "no field True_Stmt");
      Set_Field3 (N, Stmt);
   end Set_True_Stmt;

   function Get_False_Stmt (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_False_Stmt (Get_Kind (N)),
                     "no field False_Stmt");
      return Get_Field4 (N);
   end Get_False_Stmt;

   procedure Set_False_Stmt (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_False_Stmt (Get_Kind (N)),
                     "no field False_Stmt");
      Set_Field4 (N, Stmt);
   end Set_False_Stmt;

   function Get_Pass_Stmt (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pass_Stmt (Get_Kind (N)),
                     "no field Pass_Stmt");
      return Get_Field4 (N);
   end Get_Pass_Stmt;

   procedure Set_Pass_Stmt (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pass_Stmt (Get_Kind (N)),
                     "no field Pass_Stmt");
      Set_Field4 (N, Stmt);
   end Set_Pass_Stmt;

   function Get_Else_Stmt (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Else_Stmt (Get_Kind (N)),
                     "no field Else_Stmt");
      return Get_Field3 (N);
   end Get_Else_Stmt;

   procedure Set_Else_Stmt (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Else_Stmt (Get_Kind (N)),
                     "no field Else_Stmt");
      Set_Field3 (N, Stmt);
   end Set_Else_Stmt;

   function Get_Clocking_Event (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Clocking_Event (Get_Kind (N)),
                     "no field Clocking_Event");
      return Get_Field9 (N);
   end Get_Clocking_Event;

   procedure Set_Clocking_Event (N : Node; Ev : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Clocking_Event (Get_Kind (N)),
                     "no field Clocking_Event");
      Set_Field9 (N, Ev);
   end Set_Clocking_Event;

   function Get_Disable_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Disable_Expression (Get_Kind (N)),
                     "no field Disable_Expression");
      return Get_Field10 (N);
   end Get_Disable_Expression;

   procedure Set_Disable_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Disable_Expression (Get_Kind (N)),
                     "no field Disable_Expression");
      Set_Field10 (N, Expr);
   end Set_Disable_Expression;

   function Get_Property_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Property_Expression (Get_Kind (N)),
                     "no field Property_Expression");
      return Get_Field11 (N);
   end Get_Property_Expression;

   procedure Set_Property_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Property_Expression (Get_Kind (N)),
                     "no field Property_Expression");
      Set_Field11 (N, Expr);
   end Set_Property_Expression;

   function Get_True_Block (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_True_Block (Get_Kind (N)),
                     "no field True_Block");
      return Get_Field3 (N);
   end Get_True_Block;

   procedure Set_True_Block (N : Node; Val : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_True_Block (Get_Kind (N)),
                     "no field True_Block");
      Set_Field3 (N, Val);
   end Set_True_Block;

   function Get_False_Block (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_False_Block (Get_Kind (N)),
                     "no field False_Block");
      return Get_Field4 (N);
   end Get_False_Block;

   procedure Set_False_Block (N : Node; Val : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_False_Block (Get_Kind (N)),
                     "no field False_Block");
      Set_Field4 (N, Val);
   end Set_False_Block;

   function Get_Statement (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Statement (Get_Kind (N)),
                     "no field Statement");
      return Get_Field1 (N);
   end Get_Statement;

   procedure Set_Statement (N : Node; Stmt : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Statement (Get_Kind (N)),
                     "no field Statement");
      Set_Field1 (N, Stmt);
   end Set_Statement;

   function Get_Foreach_Array (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Foreach_Array (Get_Kind (N)),
                     "no field Foreach_Array");
      return Get_Field3 (N);
   end Get_Foreach_Array;

   procedure Set_Foreach_Array (N : Node; Arr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Foreach_Array (Get_Kind (N)),
                     "no field Foreach_Array");
      Set_Field3 (N, Arr);
   end Set_Foreach_Array;

   function Get_Foreach_Variables (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Foreach_Variables (Get_Kind (N)),
                     "no field Foreach_Variables");
      return Get_Field4 (N);
   end Get_Foreach_Variables;

   procedure Set_Foreach_Variables (N : Node; Vars : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Foreach_Variables (Get_Kind (N)),
                     "no field Foreach_Variables");
      Set_Field4 (N, Vars);
   end Set_Foreach_Variables;

   function Get_Control (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Control (Get_Kind (N)),
                     "no field Control");
      return Get_Field5 (N);
   end Get_Control;

   procedure Set_Control (N : Node; Ctrl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Control (Get_Kind (N)),
                     "no field Control");
      Set_Field5 (N, Ctrl);
   end Set_Control;

   function Get_Replication (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Replication (Get_Kind (N)),
                     "no field Replication");
      return Get_Field1 (N);
   end Get_Replication;

   procedure Set_Replication (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Replication (Get_Kind (N)),
                     "no field Replication");
      Set_Field1 (N, Expr);
   end Set_Replication;

   function Get_Replication_Cst (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Replication_Cst (Get_Kind (N)),
                     "no field Replication_Cst");
      return Node_To_Int32 (Get_Field1 (N));
   end Get_Replication_Cst;

   procedure Set_Replication_Cst (N : Node; Msb : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Replication_Cst (Get_Kind (N)),
                     "no field Replication_Cst");
      Set_Field1 (N, Int32_To_Node (Msb));
   end Set_Replication_Cst;

   function Get_Expressions (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expressions (Get_Kind (N)),
                     "no field Expressions");
      return Get_Field2 (N);
   end Get_Expressions;

   procedure Set_Expressions (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expressions (Get_Kind (N)),
                     "no field Expressions");
      Set_Field2 (N, Expr);
   end Set_Expressions;

   function Get_Elements (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Elements (Get_Kind (N)),
                     "no field Elements");
      return Get_Field2 (N);
   end Get_Elements;

   procedure Set_Elements (N : Node; Els : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Elements (Get_Kind (N)),
                     "no field Elements");
      Set_Field2 (N, Els);
   end Set_Elements;

   function Get_Slice_Size_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Slice_Size_Expr (Get_Kind (N)),
                     "no field Slice_Size_Expr");
      return Get_Field4 (N);
   end Get_Slice_Size_Expr;

   procedure Set_Slice_Size_Expr (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Slice_Size_Expr (Get_Kind (N)),
                     "no field Slice_Size_Expr");
      Set_Field4 (N, Expr);
   end Set_Slice_Size_Expr;

   function Get_Slice_Size_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Slice_Size_Type (Get_Kind (N)),
                     "no field Slice_Size_Type");
      return Get_Field4 (N);
   end Get_Slice_Size_Type;

   procedure Set_Slice_Size_Type (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Slice_Size_Type (Get_Kind (N)),
                     "no field Slice_Size_Type");
      Set_Field4 (N, Expr);
   end Set_Slice_Size_Type;

   function Get_Members (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Members (Get_Kind (N)),
                     "no field Members");
      return Get_Field1 (N);
   end Get_Members;

   procedure Set_Members (N : Node; Els : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Members (Get_Kind (N)),
                     "no field Members");
      Set_Field1 (N, Els);
   end Set_Members;

   function Get_Nbr_Members (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Nbr_Members (Get_Kind (N)),
                     "no field Nbr_Members");
      return Node_To_Int32 (Get_Field2 (N));
   end Get_Nbr_Members;

   procedure Set_Nbr_Members (N : Node; Nbr : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Nbr_Members (Get_Kind (N)),
                     "no field Nbr_Members");
      Set_Field2 (N, Int32_To_Node (Nbr));
   end Set_Nbr_Members;

   function Get_Member_Index (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Member_Index (Get_Kind (N)),
                     "no field Member_Index");
      return Node_To_Int32 (Get_Field7 (N));
   end Get_Member_Index;

   procedure Set_Member_Index (N : Node; Idx : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Member_Index (Get_Kind (N)),
                     "no field Member_Index");
      Set_Field7 (N, Int32_To_Node (Idx));
   end Set_Member_Index;

   function Get_Packed_Member_Offset (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Packed_Member_Offset (Get_Kind (N)),
                     "no field Packed_Member_Offset");
      return Node_To_Uns32 (Get_Field5 (N));
   end Get_Packed_Member_Offset;

   procedure Set_Packed_Member_Offset (N : Node; Idx : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Packed_Member_Offset (Get_Kind (N)),
                     "no field Packed_Member_Offset");
      Set_Field5 (N, Uns32_To_Node (Idx));
   end Set_Packed_Member_Offset;

   function Get_Nature_Items (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Nature_Items (Get_Kind (N)),
                     "no field Nature_Items");
      return Get_Field4 (N);
   end Get_Nature_Items;

   procedure Set_Nature_Items (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Nature_Items (Get_Kind (N)),
                     "no field Nature_Items");
      Set_Field4 (N, Items);
   end Set_Nature_Items;

   function Get_Discipline_Items (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Discipline_Items (Get_Kind (N)),
                     "no field Discipline_Items");
      return Get_Field4 (N);
   end Get_Discipline_Items;

   procedure Set_Discipline_Items (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Discipline_Items (Get_Kind (N)),
                     "no field Discipline_Items");
      Set_Field4 (N, Items);
   end Set_Discipline_Items;

   function Get_Continuous_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Continuous_Flag (Get_Kind (N)),
                     "no field Continuous_Flag");
      return Get_Flag1 (N);
   end Get_Continuous_Flag;

   procedure Set_Continuous_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Continuous_Flag (Get_Kind (N)),
                     "no field Continuous_Flag");
      Set_Flag1 (N, Flag);
   end Set_Continuous_Flag;

   function Get_Potential_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Potential_Flag (Get_Kind (N)),
                     "no field Potential_Flag");
      return Get_Flag1 (N);
   end Get_Potential_Flag;

   procedure Set_Potential_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Potential_Flag (Get_Kind (N)),
                     "no field Potential_Flag");
      Set_Flag1 (N, Flag);
   end Set_Potential_Flag;

   function Get_Nature (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Nature (Get_Kind (N)),
                     "no field Nature");
      return Get_Field4 (N);
   end Get_Nature;

   procedure Set_Nature (N : Node; Nature : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Nature (Get_Kind (N)),
                     "no field Nature");
      Set_Field4 (N, Nature);
   end Set_Nature;

   function Get_Connections (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Connections (Get_Kind (N)),
                     "no field Connections");
      return Get_Field8 (N);
   end Get_Connections;

   procedure Set_Connections (N : Node; Conns : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Connections (Get_Kind (N)),
                     "no field Connections");
      Set_Field8 (N, Conns);
   end Set_Connections;

   function Get_Gate_Terminals (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Gate_Terminals (Get_Kind (N)),
                     "no field Gate_Terminals");
      return Get_Field8 (N);
   end Get_Gate_Terminals;

   procedure Set_Gate_Terminals (N : Node; Terms : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Gate_Terminals (Get_Kind (N)),
                     "no field Gate_Terminals");
      Set_Field8 (N, Terms);
   end Set_Gate_Terminals;

   function Get_Parameter_Values (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Values (Get_Kind (N)),
                     "no field Parameter_Values");
      return Get_Field9 (N);
   end Get_Parameter_Values;

   procedure Set_Parameter_Values (N : Node; Values : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Parameter_Values (Get_Kind (N)),
                     "no field Parameter_Values");
      Set_Field9 (N, Values);
   end Set_Parameter_Values;

   function Get_Case_Items (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Case_Items (Get_Kind (N)),
                     "no field Case_Items");
      return Get_Field1 (N);
   end Get_Case_Items;

   procedure Set_Case_Items (N : Node; Items : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Case_Items (Get_Kind (N)),
                     "no field Case_Items");
      Set_Field1 (N, Items);
   end Set_Case_Items;

   function Get_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay (Get_Kind (N)),
                     "no field Delay");
      return Get_Field6 (N);
   end Get_Delay;

   procedure Set_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay (Get_Kind (N)),
                     "no field Delay");
      Set_Field6 (N, Value);
   end Set_Delay;

   function Get_Net_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Net_Delay (Get_Kind (N)),
                     "no field Net_Delay");
      return Get_Field8 (N);
   end Get_Net_Delay;

   procedure Set_Net_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Net_Delay (Get_Kind (N)),
                     "no field Net_Delay");
      Set_Field8 (N, Value);
   end Set_Net_Delay;

   function Get_Gate_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Gate_Delay (Get_Kind (N)),
                     "no field Gate_Delay");
      return Get_Field4 (N);
   end Get_Gate_Delay;

   procedure Set_Gate_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Gate_Delay (Get_Kind (N)),
                     "no field Gate_Delay");
      Set_Field4 (N, Value);
   end Set_Gate_Delay;

   function Get_Assign_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Assign_Delay (Get_Kind (N)),
                     "no field Assign_Delay");
      return Get_Field3 (N);
   end Get_Assign_Delay;

   procedure Set_Assign_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Assign_Delay (Get_Kind (N)),
                     "no field Assign_Delay");
      Set_Field3 (N, Value);
   end Set_Assign_Delay;

   function Get_Rising_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Rising_Delay (Get_Kind (N)),
                     "no field Rising_Delay");
      return Get_Field1 (N);
   end Get_Rising_Delay;

   procedure Set_Rising_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Rising_Delay (Get_Kind (N)),
                     "no field Rising_Delay");
      Set_Field1 (N, Value);
   end Set_Rising_Delay;

   function Get_Falling_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Falling_Delay (Get_Kind (N)),
                     "no field Falling_Delay");
      return Get_Field2 (N);
   end Get_Falling_Delay;

   procedure Set_Falling_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Falling_Delay (Get_Kind (N)),
                     "no field Falling_Delay");
      Set_Field2 (N, Value);
   end Set_Falling_Delay;

   function Get_Highz_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Highz_Delay (Get_Kind (N)),
                     "no field Highz_Delay");
      return Get_Field3 (N);
   end Get_Highz_Delay;

   procedure Set_Highz_Delay (N : Node; Value : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Highz_Delay (Get_Kind (N)),
                     "no field Highz_Delay");
      Set_Field3 (N, Value);
   end Set_Highz_Delay;

   function Get_For_Initialization (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_For_Initialization (Get_Kind (N)),
                     "no field For_Initialization");
      return Get_Field3 (N);
   end Get_For_Initialization;

   procedure Set_For_Initialization (N : Node; Assign : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_For_Initialization (Get_Kind (N)),
                     "no field For_Initialization");
      Set_Field3 (N, Assign);
   end Set_For_Initialization;

   function Get_Step_Assign (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Step_Assign (Get_Kind (N)),
                     "no field Step_Assign");
      return Get_Field4 (N);
   end Get_Step_Assign;

   procedure Set_Step_Assign (N : Node; Assign : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Step_Assign (Get_Kind (N)),
                     "no field Step_Assign");
      Set_Field4 (N, Assign);
   end Set_Step_Assign;

   function Get_Arguments (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Arguments (Get_Kind (N)),
                     "no field Arguments");
      return Get_Field4 (N);
   end Get_Arguments;

   procedure Set_Arguments (N : Node; Args : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Arguments (Get_Kind (N)),
                     "no field Arguments");
      Set_Field4 (N, Args);
   end Set_Arguments;

   function Get_Iterator_Argument (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Iterator_Argument (Get_Kind (N)),
                     "no field Iterator_Argument");
      return Get_Field5 (N);
   end Get_Iterator_Argument;

   procedure Set_Iterator_Argument (N : Node; Arg : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Iterator_Argument (Get_Kind (N)),
                     "no field Iterator_Argument");
      Set_Field5 (N, Arg);
   end Set_Iterator_Argument;

   function Get_Task (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Task (Get_Kind (N)),
                     "no field Task");
      return Get_Field1 (N);
   end Get_Task;

   procedure Set_Task (N : Node; Args : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Task (Get_Kind (N)),
                     "no field Task");
      Set_Field1 (N, Args);
   end Set_Task;

   function Get_Signed_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Signed_Flag (Get_Kind (N)),
                     "no field Signed_Flag");
      return Get_Flag1 (N);
   end Get_Signed_Flag;

   procedure Set_Signed_Flag (N : Node; Is_Signed : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Signed_Flag (Get_Kind (N)),
                     "no field Signed_Flag");
      Set_Flag1 (N, Is_Signed);
   end Set_Signed_Flag;

   function Get_Scope_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Scope_Flag (Get_Kind (N)),
                     "no field Scope_Flag");
      return Get_Flag1 (N);
   end Get_Scope_Flag;

   procedure Set_Scope_Flag (N : Node; Scope : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Scope_Flag (Get_Kind (N)),
                     "no field Scope_Flag");
      Set_Flag1 (N, Scope);
   end Set_Scope_Flag;

   function Get_Number_Base (N : Node) return Base_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Base (Get_Kind (N)),
                     "no field Number_Base");
      return Base_Type'Val (Get_State1 (N));
   end Get_Number_Base;

   procedure Set_Number_Base (N : Node; B : Base_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Base (Get_Kind (N)),
                     "no field Number_Base");
      Set_State1 (N, Base_Type'Pos (B));
   end Set_Number_Base;

   function Get_Number_Hi_Val (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Hi_Val (Get_Kind (N)),
                     "no field Number_Hi_Val");
      return Node_To_Uns32 (Get_Field1 (N));
   end Get_Number_Hi_Val;

   procedure Set_Number_Hi_Val (N : Node; Val : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Hi_Val (Get_Kind (N)),
                     "no field Number_Hi_Val");
      Set_Field1 (N, Uns32_To_Node (Val));
   end Set_Number_Hi_Val;

   function Get_Number_Lo_Val (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Lo_Val (Get_Kind (N)),
                     "no field Number_Lo_Val");
      return Node_To_Uns32 (Get_Field2 (N));
   end Get_Number_Lo_Val;

   procedure Set_Number_Lo_Val (N : Node; Val : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Lo_Val (Get_Kind (N)),
                     "no field Number_Lo_Val");
      Set_Field2 (N, Uns32_To_Node (Val));
   end Set_Number_Lo_Val;

   function Get_Number_Hi_Zx (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Hi_Zx (Get_Kind (N)),
                     "no field Number_Hi_Zx");
      return Node_To_Uns32 (Get_Field4 (N));
   end Get_Number_Hi_Zx;

   procedure Set_Number_Hi_Zx (N : Node; Zx : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Hi_Zx (Get_Kind (N)),
                     "no field Number_Hi_Zx");
      Set_Field4 (N, Uns32_To_Node (Zx));
   end Set_Number_Hi_Zx;

   function Get_Number_Lo_Zx (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Lo_Zx (Get_Kind (N)),
                     "no field Number_Lo_Zx");
      return Node_To_Uns32 (Get_Field5 (N));
   end Get_Number_Lo_Zx;

   procedure Set_Number_Lo_Zx (N : Node; Zx : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Lo_Zx (Get_Kind (N)),
                     "no field Number_Lo_Zx");
      Set_Field5 (N, Uns32_To_Node (Zx));
   end Set_Number_Lo_Zx;

   function Get_Number_Size (N : Node) return Width_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Size (Get_Kind (N)),
                     "no field Number_Size");
      return Node_To_Width_Type (Get_Field6 (N));
   end Get_Number_Size;

   procedure Set_Number_Size (N : Node; Bn : Width_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Number_Size (Get_Kind (N)),
                     "no field Number_Size");
      Set_Field6 (N, Width_Type_To_Node (Bn));
   end Set_Number_Size;

   function Get_Expr_Origin (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expr_Origin (Get_Kind (N)),
                     "no field Expr_Origin");
      return Get_Field6 (N);
   end Get_Expr_Origin;

   procedure Set_Expr_Origin (N : Node; Orig : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expr_Origin (Get_Kind (N)),
                     "no field Expr_Origin");
      Set_Field6 (N, Orig);
   end Set_Expr_Origin;

   function Get_Bignum_Index (N : Node) return Bn_Index is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Bignum_Index (Get_Kind (N)),
                     "no field Bignum_Index");
      return Node_To_Bn_Index (Get_Field1 (N));
   end Get_Bignum_Index;

   procedure Set_Bignum_Index (N : Node; Idx : Bn_Index) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Bignum_Index (Get_Kind (N)),
                     "no field Bignum_Index");
      Set_Field1 (N, Bn_Index_To_Node (Idx));
   end Set_Bignum_Index;

   function Get_Bignum_Len (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Bignum_Len (Get_Kind (N)),
                     "no field Bignum_Len");
      return Node_To_Uns32 (Get_Field2 (N));
   end Get_Bignum_Len;

   procedure Set_Bignum_Len (N : Node; Len : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Bignum_Len (Get_Kind (N)),
                     "no field Bignum_Len");
      Set_Field2 (N, Uns32_To_Node (Len));
   end Set_Bignum_Len;

   type Fp64_Conv is record
      Field1: Node;
      Field2: Node;
   end record;
   pragma Pack (Fp64_Conv);
   pragma Assert (Fp64_Conv'Size = Fp64'Size);

   function Get_Real_Number (N : Node) return Fp64
   is
      function To_Fp64 is new Ada.Unchecked_Conversion
         (Fp64_Conv, Fp64);
      Conv : Fp64_Conv;
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Real_Number (Get_Kind (N)),
                     "no field Real_Number");
      Conv.Field1 := Get_Field1 (N);
      Conv.Field2 := Get_Field2 (N);
      return To_Fp64 (Conv);
   end Get_Real_Number;

   procedure Set_Real_Number (N : Node; Val : Fp64)
   is
      function To_Fp64_Conv is new Ada.Unchecked_Conversion
         (Fp64, Fp64_Conv);
      Conv : Fp64_Conv;
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Real_Number (Get_Kind (N)),
                     "no field Real_Number");
      Conv := To_Fp64_Conv (Val);
      Set_Field1 (N, Conv.Field1);
      Set_Field2 (N, Conv.Field2);
   end Set_Real_Number;

   function Get_Time_Unit (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Time_Unit (Get_Kind (N)),
                     "no field Time_Unit");
      return Node_To_Int32 (Get_Field4 (N));
   end Get_Time_Unit;

   procedure Set_Time_Unit (N : Node; Val : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Time_Unit (Get_Kind (N)),
                     "no field Time_Unit");
      Set_Field4 (N, Int32_To_Node (Val));
   end Set_Time_Unit;

   function Get_Scale_Factor (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Scale_Factor (Get_Kind (N)),
                     "no field Scale_Factor");
      return Node_To_Int32 (Get_Field4 (N));
   end Get_Scale_Factor;

   procedure Set_Scale_Factor (N : Node; Val : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Scale_Factor (Get_Kind (N)),
                     "no field Scale_Factor");
      Set_Field4 (N, Int32_To_Node (Val));
   end Set_Scale_Factor;

   function Get_Time_Precision (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Time_Precision (Get_Kind (N)),
                     "no field Time_Precision");
      return Node_To_Int32 (Get_Field3 (N));
   end Get_Time_Precision;

   procedure Set_Time_Precision (N : Node; Val : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Time_Precision (Get_Kind (N)),
                     "no field Time_Precision");
      Set_Field3 (N, Int32_To_Node (Val));
   end Set_Time_Precision;

   function Get_Timescale (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Timescale (Get_Kind (N)),
                     "no field Timescale");
      return Get_Field5 (N);
   end Get_Timescale;

   procedure Set_Timescale (N : Node; V : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Timescale (Get_Kind (N)),
                     "no field Timescale");
      Set_Field5 (N, V);
   end Set_Timescale;

   function Get_String_Size (N : Node) return Uns32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String_Size (Get_Kind (N)),
                     "no field String_Size");
      return Node_To_Uns32 (Get_Field6 (N));
   end Get_String_Size;

   procedure Set_String_Size (N : Node; Bn : Uns32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_String_Size (Get_Kind (N)),
                     "no field String_Size");
      Set_Field6 (N, Uns32_To_Node (Bn));
   end Set_String_Size;

   function Get_Data_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Data_Type (Get_Kind (N)),
                     "no field Data_Type");
      return Get_Field3 (N);
   end Get_Data_Type;

   procedure Set_Data_Type (N : Node; Atype : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Data_Type (Get_Kind (N)),
                     "no field Data_Type");
      Set_Field3 (N, Atype);
   end Set_Data_Type;

   function Get_Expr_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expr_Type (Get_Kind (N)),
                     "no field Expr_Type");
      return Get_Field3 (N);
   end Get_Expr_Type;

   procedure Set_Expr_Type (N : Node; Atype : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Expr_Type (Get_Kind (N)),
                     "no field Expr_Type");
      Set_Field3 (N, Atype);
   end Set_Expr_Type;

   function Get_Param_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Param_Type (Get_Kind (N)),
                     "no field Param_Type");
      return Get_Field9 (N);
   end Get_Param_Type;

   procedure Set_Param_Type (N : Node; Atype : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Param_Type (Get_Kind (N)),
                     "no field Param_Type");
      Set_Field9 (N, Atype);
   end Set_Param_Type;

   function Get_Element_Data_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Element_Data_Type (Get_Kind (N)),
                     "no field Element_Data_Type");
      return Get_Field2 (N);
   end Get_Element_Data_Type;

   procedure Set_Element_Data_Type (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Element_Data_Type (Get_Kind (N)),
                     "no field Element_Data_Type");
      Set_Field2 (N, El);
   end Set_Element_Data_Type;

   function Get_Type_Element_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Element_Type (Get_Kind (N)),
                     "no field Type_Element_Type");
      return Get_Field2 (N);
   end Get_Type_Element_Type;

   procedure Set_Type_Element_Type (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Element_Type (Get_Kind (N)),
                     "no field Type_Element_Type");
      Set_Field2 (N, El);
   end Set_Type_Element_Type;

   function Get_Cast_Data_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cast_Data_Type (Get_Kind (N)),
                     "no field Cast_Data_Type");
      return Get_Field2 (N);
   end Get_Cast_Data_Type;

   procedure Set_Cast_Data_Type (N : Node; Atype : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Cast_Data_Type (Get_Kind (N)),
                     "no field Cast_Data_Type");
      Set_Field2 (N, Atype);
   end Set_Cast_Data_Type;

   function Get_Base_Class_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Base_Class_Type (Get_Kind (N)),
                     "no field Base_Class_Type");
      return Get_Field4 (N);
   end Get_Base_Class_Type;

   procedure Set_Base_Class_Type (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Base_Class_Type (Get_Kind (N)),
                     "no field Base_Class_Type");
      Set_Field4 (N, El);
   end Set_Base_Class_Type;

   function Get_Class_Constructor (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Constructor (Get_Kind (N)),
                     "no field Class_Constructor");
      return Get_Field8 (N);
   end Get_Class_Constructor;

   procedure Set_Class_Constructor (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Constructor (Get_Kind (N)),
                     "no field Class_Constructor");
      Set_Field8 (N, El);
   end Set_Class_Constructor;

   function Get_Inheritance_Depth (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Inheritance_Depth (Get_Kind (N)),
                     "no field Inheritance_Depth");
      return Node_To_Int32 (Get_Field9 (N));
   end Get_Inheritance_Depth;

   procedure Set_Inheritance_Depth (N : Node; D : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Inheritance_Depth (Get_Kind (N)),
                     "no field Inheritance_Depth");
      Set_Field9 (N, Int32_To_Node (D));
   end Set_Inheritance_Depth;

   function Get_Enum_Base_Data_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Enum_Base_Data_Type (Get_Kind (N)),
                     "no field Enum_Base_Data_Type");
      return Get_Field5 (N);
   end Get_Enum_Base_Data_Type;

   procedure Set_Enum_Base_Data_Type (N : Node; Typ : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Enum_Base_Data_Type (Get_Kind (N)),
                     "no field Enum_Base_Data_Type");
      Set_Field5 (N, Typ);
   end Set_Enum_Base_Data_Type;

   function Get_Enum_Base_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Enum_Base_Type (Get_Kind (N)),
                     "no field Enum_Base_Type");
      return Get_Field2 (N);
   end Get_Enum_Base_Type;

   procedure Set_Enum_Base_Type (N : Node; Typ : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Enum_Base_Type (Get_Kind (N)),
                     "no field Enum_Base_Type");
      Set_Field2 (N, Typ);
   end Set_Enum_Base_Type;

   function Get_Packed_Base_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Packed_Base_Type (Get_Kind (N)),
                     "no field Packed_Base_Type");
      return Get_Field5 (N);
   end Get_Packed_Base_Type;

   procedure Set_Packed_Base_Type (N : Node; Typ : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Packed_Base_Type (Get_Kind (N)),
                     "no field Packed_Base_Type");
      Set_Field5 (N, Typ);
   end Set_Packed_Base_Type;

   function Get_Default_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Default_Type (Get_Kind (N)),
                     "no field Default_Type");
      return Get_Field3 (N);
   end Get_Default_Type;

   procedure Set_Default_Type (N : Node; Typ : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Default_Type (Get_Kind (N)),
                     "no field Default_Type");
      Set_Field3 (N, Typ);
   end Set_Default_Type;

   function Get_Type_Owner (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Owner (Get_Kind (N)),
                     "no field Type_Owner");
      return Get_Flag3 (N);
   end Get_Type_Owner;

   procedure Set_Type_Owner (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Owner (Get_Kind (N)),
                     "no field Type_Owner");
      Set_Flag3 (N, Flag);
   end Set_Type_Owner;

   function Get_Type_Owner_2 (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Owner_2 (Get_Kind (N)),
                     "no field Type_Owner_2");
      return Get_Flag5 (N);
   end Get_Type_Owner_2;

   procedure Set_Type_Owner_2 (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Owner_2 (Get_Kind (N)),
                     "no field Type_Owner_2");
      Set_Flag5 (N, Flag);
   end Set_Type_Owner_2;

   function Get_Forward_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Forward_Type (Get_Kind (N)),
                     "no field Forward_Type");
      return Get_Field3 (N);
   end Get_Forward_Type;

   procedure Set_Forward_Type (N : Node; Atype : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Forward_Type (Get_Kind (N)),
                     "no field Forward_Type");
      Set_Field3 (N, Atype);
   end Set_Forward_Type;

   function Get_Enum_Names (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Enum_Names (Get_Kind (N)),
                     "no field Enum_Names");
      return Get_Field1 (N);
   end Get_Enum_Names;

   procedure Set_Enum_Names (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Enum_Names (Get_Kind (N)),
                     "no field Enum_Names");
      Set_Field1 (N, El);
   end Set_Enum_Names;

   function Get_Index_Data_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Index_Data_Type (Get_Kind (N)),
                     "no field Index_Data_Type");
      return Get_Field1 (N);
   end Get_Index_Data_Type;

   procedure Set_Index_Data_Type (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Index_Data_Type (Get_Kind (N)),
                     "no field Index_Data_Type");
      Set_Field1 (N, El);
   end Set_Index_Data_Type;

   function Get_Type_Index_Type (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Index_Type (Get_Kind (N)),
                     "no field Type_Index_Type");
      return Get_Field1 (N);
   end Get_Type_Index_Type;

   procedure Set_Type_Index_Type (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Index_Type (Get_Kind (N)),
                     "no field Type_Index_Type");
      Set_Field1 (N, El);
   end Set_Type_Index_Type;

   function Get_Type_Argument (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Argument (Get_Kind (N)),
                     "no field Type_Argument");
      return Get_Field1 (N);
   end Get_Type_Argument;

   procedure Set_Type_Argument (N : Node; El : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Argument (Get_Kind (N)),
                     "no field Type_Argument");
      Set_Field1 (N, El);
   end Set_Type_Argument;

   function Get_Type_Signed (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Signed (Get_Kind (N)),
                     "no field Type_Signed");
      return Get_Flag1 (N);
   end Get_Type_Signed;

   procedure Set_Type_Signed (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Signed (Get_Kind (N)),
                     "no field Type_Signed");
      Set_Flag1 (N, Flag);
   end Set_Type_Signed;

   function Get_Subroutine (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Subroutine (Get_Kind (N)),
                     "no field Subroutine");
      return Get_Field1 (N);
   end Get_Subroutine;

   procedure Set_Subroutine (N : Node; Sub : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Subroutine (Get_Kind (N)),
                     "no field Subroutine");
      Set_Field1 (N, Sub);
   end Set_Subroutine;

   function Get_Object (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Object (Get_Kind (N)),
                     "no field Object");
      return Get_Field2 (N);
   end Get_Object;

   procedure Set_Object (N : Node; Obj : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Object (Get_Kind (N)),
                     "no field Object");
      Set_Field2 (N, Obj);
   end Set_Object;

   function Get_With_Expression (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_With_Expression (Get_Kind (N)),
                     "no field With_Expression");
      return Get_Field6 (N);
   end Get_With_Expression;

   procedure Set_With_Expression (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_With_Expression (Get_Kind (N)),
                     "no field With_Expression");
      Set_Field6 (N, Expr);
   end Set_With_Expression;

   function Get_Drive_Strength (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Drive_Strength (Get_Kind (N)),
                     "no field Drive_Strength");
      return Node_To_Int32 (Get_Field5 (N));
   end Get_Drive_Strength;

   procedure Set_Drive_Strength (N : Node; Strength : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Drive_Strength (Get_Kind (N)),
                     "no field Drive_Strength");
      Set_Field5 (N, Int32_To_Node (Strength));
   end Set_Drive_Strength;

   function Get_Net_Drive_Strength (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Net_Drive_Strength (Get_Kind (N)),
                     "no field Net_Drive_Strength");
      return Node_To_Int32 (Get_Field9 (N));
   end Get_Net_Drive_Strength;

   procedure Set_Net_Drive_Strength (N : Node; Strength : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Net_Drive_Strength (Get_Kind (N)),
                     "no field Net_Drive_Strength");
      Set_Field9 (N, Int32_To_Node (Strength));
   end Set_Net_Drive_Strength;

   function Get_Charge_Strength (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Charge_Strength (Get_Kind (N)),
                     "no field Charge_Strength");
      return Node_To_Int32 (Get_Field9 (N));
   end Get_Charge_Strength;

   procedure Set_Charge_Strength (N : Node; Strength : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Charge_Strength (Get_Kind (N)),
                     "no field Charge_Strength");
      Set_Field9 (N, Int32_To_Node (Strength));
   end Set_Charge_Strength;

   function Get_Module (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Module (Get_Kind (N)),
                     "no field Module");
      return Get_Field7 (N);
   end Get_Module;

   procedure Set_Module (N : Node; M : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Module (Get_Kind (N)),
                     "no field Module");
      Set_Field7 (N, M);
   end Set_Module;

   function Get_Class_Name (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Name (Get_Kind (N)),
                     "no field Class_Name");
      return Get_Field5 (N);
   end Get_Class_Name;

   procedure Set_Class_Name (N : Node; C : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Name (Get_Kind (N)),
                     "no field Class_Name");
      Set_Field5 (N, C);
   end Set_Class_Name;

   function Get_Interface (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Interface (Get_Kind (N)),
                     "no field Interface");
      return Get_Field3 (N);
   end Get_Interface;

   procedure Set_Interface (N : Node; C : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Interface (Get_Kind (N)),
                     "no field Interface");
      Set_Field3 (N, C);
   end Set_Interface;

   function Get_Interface_Name (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Interface_Name (Get_Kind (N)),
                     "no field Interface_Name");
      return Get_Field7 (N);
   end Get_Interface_Name;

   procedure Set_Interface_Name (N : Node; Name : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Interface_Name (Get_Kind (N)),
                     "no field Interface_Name");
      Set_Field7 (N, Name);
   end Set_Interface_Name;

   function Get_Instance (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance (Get_Kind (N)),
                     "no field Instance");
      return Get_Field4 (N);
   end Get_Instance;

   procedure Set_Instance (N : Node; Inst : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance (Get_Kind (N)),
                     "no field Instance");
      Set_Field4 (N, Inst);
   end Set_Instance;

   function Get_Instance_Ref (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance_Ref (Get_Kind (N)),
                     "no field Instance_Ref");
      return Get_Field4 (N);
   end Get_Instance_Ref;

   procedure Set_Instance_Ref (N : Node; Inst : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instance_Ref (Get_Kind (N)),
                     "no field Instance_Ref");
      Set_Field4 (N, Inst);
   end Set_Instance_Ref;

   function Get_Port (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Port (Get_Kind (N)),
                     "no field Port");
      return Get_Field3 (N);
   end Get_Port;

   procedure Set_Port (N : Node; Port : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Port (Get_Kind (N)),
                     "no field Port");
      Set_Field3 (N, Port);
   end Set_Port;

   function Get_Collapse_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Collapse_Flag (Get_Kind (N)),
                     "no field Collapse_Flag");
      return Get_Flag1 (N);
   end Get_Collapse_Flag;

   procedure Set_Collapse_Flag (N : Node; Val : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Collapse_Flag (Get_Kind (N)),
                     "no field Collapse_Flag");
      Set_Flag1 (N, Val);
   end Set_Collapse_Flag;

   function Get_Unary_Op (N : Node) return Unary_Ops is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Unary_Op (Get_Kind (N)),
                     "no field Unary_Op");
      return Unary_Ops'Val (Get_Field1 (N));
   end Get_Unary_Op;

   procedure Set_Unary_Op (N : Node; Op : Unary_Ops) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Unary_Op (Get_Kind (N)),
                     "no field Unary_Op");
      Set_Field1 (N, Unary_Ops'Pos (Op));
   end Set_Unary_Op;

   function Get_Binary_Op (N : Node) return Binary_Ops is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Binary_Op (Get_Kind (N)),
                     "no field Binary_Op");
      return Binary_Ops'Val (Get_Field5 (N));
   end Get_Binary_Op;

   procedure Set_Binary_Op (N : Node; Op : Binary_Ops) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Binary_Op (Get_Kind (N)),
                     "no field Binary_Op");
      Set_Field5 (N, Binary_Ops'Pos (Op));
   end Set_Binary_Op;

   function Get_Conversion_Op (N : Node) return Conv_Ops is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Conversion_Op (Get_Kind (N)),
                     "no field Conversion_Op");
      return Conv_Ops'Val (Get_Field1 (N));
   end Get_Conversion_Op;

   procedure Set_Conversion_Op (N : Node; Op : Conv_Ops) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Conversion_Op (Get_Kind (N)),
                     "no field Conversion_Op");
      Set_Field1 (N, Conv_Ops'Pos (Op));
   end Set_Conversion_Op;

   function Get_Declaration (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Declaration (Get_Kind (N)),
                     "no field Declaration");
      return Get_Field4 (N);
   end Get_Declaration;

   procedure Set_Declaration (N : Node; Decl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Declaration (Get_Kind (N)),
                     "no field Declaration");
      Set_Field4 (N, Decl);
   end Set_Declaration;

   function Get_Redeclaration (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Redeclaration (Get_Kind (N)),
                     "no field Redeclaration");
      return Get_Field4 (N);
   end Get_Redeclaration;

   procedure Set_Redeclaration (N : Node; Decl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Redeclaration (Get_Kind (N)),
                     "no field Redeclaration");
      Set_Field4 (N, Decl);
   end Set_Redeclaration;

   function Get_This_Declaration (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_This_Declaration (Get_Kind (N)),
                     "no field This_Declaration");
      return Get_Field2 (N);
   end Get_This_Declaration;

   procedure Set_This_Declaration (N : Node; Decl : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_This_Declaration (Get_Kind (N)),
                     "no field This_Declaration");
      Set_Field2 (N, Decl);
   end Set_This_Declaration;

   function Get_Default_Value (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Default_Value (Get_Kind (N)),
                     "no field Default_Value");
      return Get_Field7 (N);
   end Get_Default_Value;

   procedure Set_Default_Value (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Default_Value (Get_Kind (N)),
                     "no field Default_Value");
      Set_Field7 (N, Expr);
   end Set_Default_Value;

   function Get_Instantiated_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instantiated_Flag (Get_Kind (N)),
                     "no field Instantiated_Flag");
      return Get_Flag1 (N);
   end Get_Instantiated_Flag;

   procedure Set_Instantiated_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Instantiated_Flag (Get_Kind (N)),
                     "no field Instantiated_Flag");
      Set_Flag1 (N, Flag);
   end Set_Instantiated_Flag;

   function Get_Ansi_Port_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Ansi_Port_Flag (Get_Kind (N)),
                     "no field Ansi_Port_Flag");
      return Get_Flag4 (N);
   end Get_Ansi_Port_Flag;

   procedure Set_Ansi_Port_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Ansi_Port_Flag (Get_Kind (N)),
                     "no field Ansi_Port_Flag");
      Set_Flag4 (N, Flag);
   end Set_Ansi_Port_Flag;

   function Get_Event (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Event (Get_Kind (N)),
                     "no field Event");
      return Get_Field4 (N);
   end Get_Event;

   procedure Set_Event (N : Node; Event : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Event (Get_Kind (N)),
                     "no field Event");
      Set_Field4 (N, Event);
   end Set_Event;

   function Get_Min_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Min_Expr (Get_Kind (N)),
                     "no field Min_Expr");
      return Get_Field1 (N);
   end Get_Min_Expr;

   procedure Set_Min_Expr (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Min_Expr (Get_Kind (N)),
                     "no field Min_Expr");
      Set_Field1 (N, Expr);
   end Set_Min_Expr;

   function Get_Typ_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Typ_Expr (Get_Kind (N)),
                     "no field Typ_Expr");
      return Get_Field2 (N);
   end Get_Typ_Expr;

   procedure Set_Typ_Expr (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Typ_Expr (Get_Kind (N)),
                     "no field Typ_Expr");
      Set_Field2 (N, Expr);
   end Set_Typ_Expr;

   function Get_Max_Expr (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Max_Expr (Get_Kind (N)),
                     "no field Max_Expr");
      return Get_Field4 (N);
   end Get_Max_Expr;

   procedure Set_Max_Expr (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Max_Expr (Get_Kind (N)),
                     "no field Max_Expr");
      Set_Field4 (N, Expr);
   end Set_Max_Expr;

   function Get_Udp_Port_Declaration_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Port_Declaration_Chain (Get_Kind (N)),
                     "no field Udp_Port_Declaration_Chain");
      return Get_Field3 (N);
   end Get_Udp_Port_Declaration_Chain;

   procedure Set_Udp_Port_Declaration_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Port_Declaration_Chain (Get_Kind (N)),
                     "no field Udp_Port_Declaration_Chain");
      Set_Field3 (N, Chain);
   end Set_Udp_Port_Declaration_Chain;

   function Get_Udp_Kind (N : Node) return Udp_Kind is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Kind (Get_Kind (N)),
                     "no field Udp_Kind");
      return Udp_Kind'Val (Get_State1 (N));
   end Get_Udp_Kind;

   procedure Set_Udp_Kind (N : Node; K : Udp_Kind) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Kind (Get_Kind (N)),
                     "no field Udp_Kind");
      Set_State1 (N, Udp_Kind'Pos (K));
   end Set_Udp_Kind;

   function Get_Udp_Initial (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Initial (Get_Kind (N)),
                     "no field Udp_Initial");
      return Get_Field4 (N);
   end Get_Udp_Initial;

   procedure Set_Udp_Initial (N : Node; Expr : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Initial (Get_Kind (N)),
                     "no field Udp_Initial");
      Set_Field4 (N, Expr);
   end Set_Udp_Initial;

   function Get_Udp_Entries_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Entries_Chain (Get_Kind (N)),
                     "no field Udp_Entries_Chain");
      return Get_Field5 (N);
   end Get_Udp_Entries_Chain;

   procedure Set_Udp_Entries_Chain (N : Node; C : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Udp_Entries_Chain (Get_Kind (N)),
                     "no field Udp_Entries_Chain");
      Set_Field5 (N, C);
   end Set_Udp_Entries_Chain;

   function Get_Input_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Input_Chain (Get_Kind (N)),
                     "no field Input_Chain");
      return Get_Field1 (N);
   end Get_Input_Chain;

   procedure Set_Input_Chain (N : Node; C : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Input_Chain (Get_Kind (N)),
                     "no field Input_Chain");
      Set_Field1 (N, C);
   end Set_Input_Chain;

   function Get_Output_Symbol (N : Node) return Udp_Symbol is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Output_Symbol (Get_Kind (N)),
                     "no field Output_Symbol");
      return Udp_Symbol'Val (Get_Field3 (N));
   end Get_Output_Symbol;

   procedure Set_Output_Symbol (N : Node; S : Udp_Symbol) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Output_Symbol (Get_Kind (N)),
                     "no field Output_Symbol");
      Set_Field3 (N, Udp_Symbol'Pos (S));
   end Set_Output_Symbol;

   function Get_Current_State (N : Node) return Udp_Symbol is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Current_State (Get_Kind (N)),
                     "no field Current_State");
      return Udp_Symbol'Val (Get_Field3 (N));
   end Get_Current_State;

   procedure Set_Current_State (N : Node; S : Udp_Symbol) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Current_State (Get_Kind (N)),
                     "no field Current_State");
      Set_Field3 (N, Udp_Symbol'Pos (S));
   end Set_Current_State;

   function Get_Next_State (N : Node) return Udp_Symbol is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Next_State (Get_Kind (N)),
                     "no field Next_State");
      return Udp_Symbol'Val (Get_Field4 (N));
   end Get_Next_State;

   procedure Set_Next_State (N : Node; S : Udp_Symbol) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Next_State (Get_Kind (N)),
                     "no field Next_State");
      Set_Field4 (N, Udp_Symbol'Pos (S));
   end Set_Next_State;

   function Get_Symbol (N : Node) return Udp_Symbol is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Symbol (Get_Kind (N)),
                     "no field Symbol");
      return Udp_Symbol'Val (Get_Field1 (N));
   end Get_Symbol;

   procedure Set_Symbol (N : Node; S : Udp_Symbol) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Symbol (Get_Kind (N)),
                     "no field Symbol");
      Set_Field1 (N, Udp_Symbol'Pos (S));
   end Set_Symbol;

   function Get_From_Symbol (N : Node) return Udp_Symbol is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_From_Symbol (Get_Kind (N)),
                     "no field From_Symbol");
      return Udp_Symbol'Val (Get_Field1 (N));
   end Get_From_Symbol;

   procedure Set_From_Symbol (N : Node; S : Udp_Symbol) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_From_Symbol (Get_Kind (N)),
                     "no field From_Symbol");
      Set_Field1 (N, Udp_Symbol'Pos (S));
   end Set_From_Symbol;

   function Get_To_Symbol (N : Node) return Udp_Symbol is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_To_Symbol (Get_Kind (N)),
                     "no field To_Symbol");
      return Udp_Symbol'Val (Get_Field3 (N));
   end Get_To_Symbol;

   procedure Set_To_Symbol (N : Node; S : Udp_Symbol) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_To_Symbol (Get_Kind (N)),
                     "no field To_Symbol");
      Set_Field3 (N, Udp_Symbol'Pos (S));
   end Set_To_Symbol;

   function Get_Specify_Input (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Specify_Input (Get_Kind (N)),
                     "no field Specify_Input");
      return Get_Field1 (N);
   end Get_Specify_Input;

   procedure Set_Specify_Input (N : Node; Input : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Specify_Input (Get_Kind (N)),
                     "no field Specify_Input");
      Set_Field1 (N, Input);
   end Set_Specify_Input;

   function Get_Specify_Output (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Specify_Output (Get_Kind (N)),
                     "no field Specify_Output");
      return Get_Field3 (N);
   end Get_Specify_Output;

   procedure Set_Specify_Output (N : Node; Output : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Specify_Output (Get_Kind (N)),
                     "no field Specify_Output");
      Set_Field3 (N, Output);
   end Set_Specify_Output;

   function Get_Path_Delay (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Path_Delay (Get_Kind (N)),
                     "no field Path_Delay");
      return Get_Field4 (N);
   end Get_Path_Delay;

   procedure Set_Path_Delay (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Path_Delay (Get_Kind (N)),
                     "no field Path_Delay");
      Set_Field4 (N, Dly);
   end Set_Path_Delay;

   function Get_Data_Source (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Data_Source (Get_Kind (N)),
                     "no field Data_Source");
      return Get_Field5 (N);
   end Get_Data_Source;

   procedure Set_Data_Source (N : Node; Data : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Data_Source (Get_Kind (N)),
                     "no field Data_Source");
      Set_Field5 (N, Data);
   end Set_Data_Source;

   function Get_Polarity (N : Node) return Polarity_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Polarity (Get_Kind (N)),
                     "no field Polarity");
      return Polarity_Type'Val (Get_State1 (N));
   end Get_Polarity;

   procedure Set_Polarity (N : Node; Polarity : Polarity_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Polarity (Get_Kind (N)),
                     "no field Polarity");
      Set_State1 (N, Polarity_Type'Pos (Polarity));
   end Set_Polarity;

   function Get_Delay_Rise (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Rise (Get_Kind (N)),
                     "no field Delay_Rise");
      return Get_Field1 (N);
   end Get_Delay_Rise;

   procedure Set_Delay_Rise (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Rise (Get_Kind (N)),
                     "no field Delay_Rise");
      Set_Field1 (N, Dly);
   end Set_Delay_Rise;

   function Get_Delay_Fall (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Fall (Get_Kind (N)),
                     "no field Delay_Fall");
      return Get_Field2 (N);
   end Get_Delay_Fall;

   procedure Set_Delay_Fall (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Fall (Get_Kind (N)),
                     "no field Delay_Fall");
      Set_Field2 (N, Dly);
   end Set_Delay_Fall;

   function Get_Delay_Z (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Z (Get_Kind (N)),
                     "no field Delay_Z");
      return Get_Field3 (N);
   end Get_Delay_Z;

   procedure Set_Delay_Z (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Z (Get_Kind (N)),
                     "no field Delay_Z");
      Set_Field3 (N, Dly);
   end Set_Delay_Z;

   function Get_Delay_01 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_01 (Get_Kind (N)),
                     "no field Delay_01");
      return Get_Field1 (N);
   end Get_Delay_01;

   procedure Set_Delay_01 (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_01 (Get_Kind (N)),
                     "no field Delay_01");
      Set_Field1 (N, Dly);
   end Set_Delay_01;

   function Get_Delay_10 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_10 (Get_Kind (N)),
                     "no field Delay_10");
      return Get_Field2 (N);
   end Get_Delay_10;

   procedure Set_Delay_10 (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_10 (Get_Kind (N)),
                     "no field Delay_10");
      Set_Field2 (N, Dly);
   end Set_Delay_10;

   function Get_Delay_0z (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_0z (Get_Kind (N)),
                     "no field Delay_0z");
      return Get_Field3 (N);
   end Get_Delay_0z;

   procedure Set_Delay_0z (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_0z (Get_Kind (N)),
                     "no field Delay_0z");
      Set_Field3 (N, Dly);
   end Set_Delay_0z;

   function Get_Delay_z1 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_z1 (Get_Kind (N)),
                     "no field Delay_z1");
      return Get_Field4 (N);
   end Get_Delay_z1;

   procedure Set_Delay_z1 (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_z1 (Get_Kind (N)),
                     "no field Delay_z1");
      Set_Field4 (N, Dly);
   end Set_Delay_z1;

   function Get_Delay_1z (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_1z (Get_Kind (N)),
                     "no field Delay_1z");
      return Get_Field5 (N);
   end Get_Delay_1z;

   procedure Set_Delay_1z (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_1z (Get_Kind (N)),
                     "no field Delay_1z");
      Set_Field5 (N, Dly);
   end Set_Delay_1z;

   function Get_Delay_z0 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_z0 (Get_Kind (N)),
                     "no field Delay_z0");
      return Get_Field6 (N);
   end Get_Delay_z0;

   procedure Set_Delay_z0 (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_z0 (Get_Kind (N)),
                     "no field Delay_z0");
      Set_Field6 (N, Dly);
   end Set_Delay_z0;

   function Get_Delay_0x (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_0x (Get_Kind (N)),
                     "no field Delay_0x");
      return Get_Field7 (N);
   end Get_Delay_0x;

   procedure Set_Delay_0x (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_0x (Get_Kind (N)),
                     "no field Delay_0x");
      Set_Field7 (N, Dly);
   end Set_Delay_0x;

   function Get_Delay_x1 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_x1 (Get_Kind (N)),
                     "no field Delay_x1");
      return Get_Field8 (N);
   end Get_Delay_x1;

   procedure Set_Delay_x1 (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_x1 (Get_Kind (N)),
                     "no field Delay_x1");
      Set_Field8 (N, Dly);
   end Set_Delay_x1;

   function Get_Delay_1x (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_1x (Get_Kind (N)),
                     "no field Delay_1x");
      return Get_Field9 (N);
   end Get_Delay_1x;

   procedure Set_Delay_1x (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_1x (Get_Kind (N)),
                     "no field Delay_1x");
      Set_Field9 (N, Dly);
   end Set_Delay_1x;

   function Get_Delay_x0 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_x0 (Get_Kind (N)),
                     "no field Delay_x0");
      return Get_Field10 (N);
   end Get_Delay_x0;

   procedure Set_Delay_x0 (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_x0 (Get_Kind (N)),
                     "no field Delay_x0");
      Set_Field10 (N, Dly);
   end Set_Delay_x0;

   function Get_Delay_xz (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_xz (Get_Kind (N)),
                     "no field Delay_xz");
      return Get_Field11 (N);
   end Get_Delay_xz;

   procedure Set_Delay_xz (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_xz (Get_Kind (N)),
                     "no field Delay_xz");
      Set_Field11 (N, Dly);
   end Set_Delay_xz;

   function Get_Delay_zx (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_zx (Get_Kind (N)),
                     "no field Delay_zx");
      return Get_Field12 (N);
   end Get_Delay_zx;

   procedure Set_Delay_zx (N : Node; Dly : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_zx (Get_Kind (N)),
                     "no field Delay_zx");
      Set_Field12 (N, Dly);
   end Set_Delay_zx;

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

   function Get_Label (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label (Get_Kind (N)),
                     "no field Label");
      return Get_Field1 (N);
   end Get_Label;

   procedure Set_Label (N : Node; L : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label (Get_Kind (N)),
                     "no field Label");
      Set_Field1 (N, L);
   end Set_Label;

   function Get_Label_Number (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label_Number (Get_Kind (N)),
                     "no field Label_Number");
      return Int32'Val (Get_Field1 (N));
   end Get_Label_Number;

   procedure Set_Label_Number (N : Node; Val : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label_Number (Get_Kind (N)),
                     "no field Label_Number");
      Set_Field1 (N, Int32'Pos (Val));
   end Set_Label_Number;

   function Get_Label_Chain (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label_Chain (Get_Kind (N)),
                     "no field Label_Chain");
      return Get_Field3 (N);
   end Get_Label_Chain;

   procedure Set_Label_Chain (N : Node; Chain : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label_Chain (Get_Kind (N)),
                     "no field Label_Chain");
      Set_Field3 (N, Chain);
   end Set_Label_Chain;

   function Get_Label_Use (N : Node) return Int32 is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label_Use (Get_Kind (N)),
                     "no field Label_Use");
      return Int32'Val (Get_Field4 (N));
   end Get_Label_Use;

   procedure Set_Label_Use (N : Node; Val : Int32) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Label_Use (Get_Kind (N)),
                     "no field Label_Use");
      Set_Field4 (N, Int32'Pos (Val));
   end Set_Label_Use;

   function Get_Suspend_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Suspend_Flag (Get_Kind (N)),
                     "no field Suspend_Flag");
      return Get_Flag1 (N);
   end Get_Suspend_Flag;

   procedure Set_Suspend_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Suspend_Flag (Get_Kind (N)),
                     "no field Suspend_Flag");
      Set_Flag1 (N, Flag);
   end Set_Suspend_Flag;

   function Get_Same_Case_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Same_Case_Flag (Get_Kind (N)),
                     "no field Same_Case_Flag");
      return Get_Flag1 (N);
   end Get_Same_Case_Flag;

   procedure Set_Same_Case_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Same_Case_Flag (Get_Kind (N)),
                     "no field Same_Case_Flag");
      Set_Flag1 (N, Flag);
   end Set_Same_Case_Flag;

   function Get_Obj_Id (N : Node) return Obj_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Obj_Id (Get_Kind (N)),
                     "no field Obj_Id");
      return Obj_Id'Val (Get_Field5 (N));
   end Get_Obj_Id;

   procedure Set_Obj_Id (N : Node; Id : Obj_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Obj_Id (Get_Kind (N)),
                     "no field Obj_Id");
      Set_Field5 (N, Obj_Id'Pos (Id));
   end Set_Obj_Id;

   function Get_Scope_Id (N : Node) return Scope_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Scope_Id (Get_Kind (N)),
                     "no field Scope_Id");
      return Scope_Id'Val (Get_Field5 (N));
   end Get_Scope_Id;

   procedure Set_Scope_Id (N : Node; Id : Scope_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Scope_Id (Get_Kind (N)),
                     "no field Scope_Id");
      Set_Field5 (N, Scope_Id'Pos (Id));
   end Set_Scope_Id;

   function Get_Process_Id (N : Node) return Proc_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Process_Id (Get_Kind (N)),
                     "no field Process_Id");
      return Proc_Id'Val (Get_Field5 (N));
   end Get_Process_Id;

   procedure Set_Process_Id (N : Node; Id : Proc_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Process_Id (Get_Kind (N)),
                     "no field Process_Id");
      Set_Field5 (N, Proc_Id'Pos (Id));
   end Set_Process_Id;

   function Get_Sys_Tf_Id (N : Node) return Sys_Tf_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Sys_Tf_Id (Get_Kind (N)),
                     "no field Sys_Tf_Id");
      return Sys_Tf_Id'Val (Get_Field5 (N));
   end Get_Sys_Tf_Id;

   procedure Set_Sys_Tf_Id (N : Node; Id : Sys_Tf_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Sys_Tf_Id (Get_Kind (N)),
                     "no field Sys_Tf_Id");
      Set_Field5 (N, Sys_Tf_Id'Pos (Id));
   end Set_Sys_Tf_Id;

   function Get_Lit_Id (N : Node) return Lit_Id is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lit_Id (Get_Kind (N)),
                     "no field Lit_Id");
      return Lit_Id'Val (Get_Field5 (N));
   end Get_Lit_Id;

   procedure Set_Lit_Id (N : Node; Id : Lit_Id) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lit_Id (Get_Kind (N)),
                     "no field Lit_Id");
      Set_Field5 (N, Lit_Id'Pos (Id));
   end Set_Lit_Id;

   function Get_Generate_Block (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Generate_Block (Get_Kind (N)),
                     "no field Generate_Block");
      return Get_Field1 (N);
   end Get_Generate_Block;

   procedure Set_Generate_Block (N : Node; Conn : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Generate_Block (Get_Kind (N)),
                     "no field Generate_Block");
      Set_Field1 (N, Conn);
   end Set_Generate_Block;

   function Get_Input_Skew (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Input_Skew (Get_Kind (N)),
                     "no field Input_Skew");
      return Get_Field3 (N);
   end Get_Input_Skew;

   procedure Set_Input_Skew (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Input_Skew (Get_Kind (N)),
                     "no field Input_Skew");
      Set_Field3 (N, S);
   end Set_Input_Skew;

   function Get_Output_Skew (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Output_Skew (Get_Kind (N)),
                     "no field Output_Skew");
      return Get_Field5 (N);
   end Get_Output_Skew;

   procedure Set_Output_Skew (N : Node; S : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Output_Skew (Get_Kind (N)),
                     "no field Output_Skew");
      Set_Field5 (N, S);
   end Set_Output_Skew;

   function Get_Delay_Control (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Control (Get_Kind (N)),
                     "no field Delay_Control");
      return Get_Field1 (N);
   end Get_Delay_Control;

   procedure Set_Delay_Control (N : Node; D : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Delay_Control (Get_Kind (N)),
                     "no field Delay_Control");
      Set_Field1 (N, D);
   end Set_Delay_Control;

   function Get_Attribute_Item (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attribute_Item (Get_Kind (N)),
                     "no field Attribute_Item");
      return Get_Field3 (N);
   end Get_Attribute_Item;

   procedure Set_Attribute_Item (N : Node; Item : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attribute_Item (Get_Kind (N)),
                     "no field Attribute_Item");
      Set_Field3 (N, Item);
   end Set_Attribute_Item;

   function Get_Has_Identifier_List (Decl : Node) return Boolean is
   begin
      pragma Assert (Decl /= Null_Node);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (Decl)),
                     "no field Has_Identifier_List");
      return Get_Flag1 (Decl);
   end Get_Has_Identifier_List;

   procedure Set_Has_Identifier_List (Decl : Node; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Node);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (Decl)),
                     "no field Has_Identifier_List");
      Set_Flag1 (Decl, Flag);
   end Set_Has_Identifier_List;

   function Get_Has_Sign (Decl : Node) return Boolean is
   begin
      pragma Assert (Decl /= Null_Node);
      pragma Assert (Has_Has_Sign (Get_Kind (Decl)),
                     "no field Has_Sign");
      return Get_Flag4 (Decl);
   end Get_Has_Sign;

   procedure Set_Has_Sign (Decl : Node; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Node);
      pragma Assert (Has_Has_Sign (Get_Kind (Decl)),
                     "no field Has_Sign");
      Set_Flag4 (Decl, Flag);
   end Set_Has_Sign;

   function Get_Connected_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Connected_Flag (Get_Kind (N)),
                     "no field Connected_Flag");
      return Get_Flag4 (N);
   end Get_Connected_Flag;

   procedure Set_Connected_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Connected_Flag (Get_Kind (N)),
                     "no field Connected_Flag");
      Set_Flag4 (N, Flag);
   end Set_Connected_Flag;

   function Get_Complete_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Complete_Flag (Get_Kind (N)),
                     "no field Complete_Flag");
      return Get_Flag2 (N);
   end Get_Complete_Flag;

   procedure Set_Complete_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Complete_Flag (Get_Kind (N)),
                     "no field Complete_Flag");
      Set_Flag2 (N, Flag);
   end Set_Complete_Flag;

   function Get_Implicit_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Implicit_Flag (Get_Kind (N)),
                     "no field Implicit_Flag");
      return Get_Flag2 (N);
   end Get_Implicit_Flag;

   procedure Set_Implicit_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Implicit_Flag (Get_Kind (N)),
                     "no field Implicit_Flag");
      Set_Flag2 (N, Flag);
   end Set_Implicit_Flag;

   function Get_Redeclaration_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Redeclaration_Flag (Get_Kind (N)),
                     "no field Redeclaration_Flag");
      return Get_Flag5 (N);
   end Get_Redeclaration_Flag;

   procedure Set_Redeclaration_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Redeclaration_Flag (Get_Kind (N)),
                     "no field Redeclaration_Flag");
      Set_Flag5 (N, Flag);
   end Set_Redeclaration_Flag;

   function Get_Is_Automatic (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Is_Automatic (Get_Kind (N)),
                     "no field Is_Automatic");
      return Get_Flag10 (N);
   end Get_Is_Automatic;

   procedure Set_Is_Automatic (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Is_Automatic (Get_Kind (N)),
                     "no field Is_Automatic");
      Set_Flag10 (N, Flag);
   end Set_Is_Automatic;

   function Get_Lifetime (N : Node) return Lifetime_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lifetime (Get_Kind (N)),
                     "no field Lifetime");
      return Boolean_To_Lifetime_Type (Get_Flag6 (N));
   end Get_Lifetime;

   procedure Set_Lifetime (N : Node; Live : Lifetime_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Lifetime (Get_Kind (N)),
                     "no field Lifetime");
      Set_Flag6 (N, Lifetime_Type_To_Boolean (Live));
   end Set_Lifetime;

   function Get_Has_Lifetime (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Lifetime (Get_Kind (N)),
                     "no field Has_Lifetime");
      return Get_Flag7 (N);
   end Get_Has_Lifetime;

   procedure Set_Has_Lifetime (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Lifetime (Get_Kind (N)),
                     "no field Has_Lifetime");
      Set_Flag7 (N, Flag);
   end Set_Has_Lifetime;

   function Get_Has_End_Name (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_End_Name (Get_Kind (N)),
                     "no field Has_End_Name");
      return Get_Flag5 (N);
   end Get_Has_End_Name;

   procedure Set_Has_End_Name (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_End_Name (Get_Kind (N)),
                     "no field Has_End_Name");
      Set_Flag5 (N, Flag);
   end Set_Has_End_Name;

   function Get_Call (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Call (Get_Kind (N)),
                     "no field Call");
      return Get_Field1 (N);
   end Get_Call;

   procedure Set_Call (N : Node; Call : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Call (Get_Kind (N)),
                     "no field Call");
      Set_Field1 (N, Call);
   end Set_Call;

   function Get_Timeunit (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Timeunit (Get_Kind (N)),
                     "no field Timeunit");
      return Get_Field3 (N);
   end Get_Timeunit;

   procedure Set_Timeunit (N : Node; Time : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Timeunit (Get_Kind (N)),
                     "no field Timeunit");
      Set_Field3 (N, Time);
   end Set_Timeunit;

   function Get_Timeprecision (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Timeprecision (Get_Kind (N)),
                     "no field Timeprecision");
      return Get_Field4 (N);
   end Get_Timeprecision;

   procedure Set_Timeprecision (N : Node; Time : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Timeprecision (Get_Kind (N)),
                     "no field Timeprecision");
      Set_Field4 (N, Time);
   end Set_Timeprecision;

   function Get_Error_Origin (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Error_Origin (Get_Kind (N)),
                     "no field Error_Origin");
      return Get_Field1 (N);
   end Get_Error_Origin;

   procedure Set_Error_Origin (N : Node; Orig : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Error_Origin (Get_Kind (N)),
                     "no field Error_Origin");
      Set_Field1 (N, Orig);
   end Set_Error_Origin;

   function Get_Has_Void_Cast (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Void_Cast (Get_Kind (N)),
                     "no field Has_Void_Cast");
      return Get_Flag1 (N);
   end Get_Has_Void_Cast;

   procedure Set_Has_Void_Cast (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Void_Cast (Get_Kind (N)),
                     "no field Has_Void_Cast");
      Set_Flag1 (N, Flag);
   end Set_Has_Void_Cast;

   function Get_Is_Const (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Is_Const (Get_Kind (N)),
                     "no field Is_Const");
      return Get_Flag2 (N);
   end Get_Is_Const;

   procedure Set_Is_Const (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Is_Const (Get_Kind (N)),
                     "no field Is_Const");
      Set_Flag2 (N, Flag);
   end Set_Is_Const;

   function Get_Has_Var (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Var (Get_Kind (N)),
                     "no field Has_Var");
      return Get_Flag4 (N);
   end Get_Has_Var;

   procedure Set_Has_Var (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Var (Get_Kind (N)),
                     "no field Has_Var");
      Set_Flag4 (N, Flag);
   end Set_Has_Var;

   function Get_Has_Type (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Type (Get_Kind (N)),
                     "no field Has_Type");
      return Get_Flag4 (N);
   end Get_Has_Type;

   procedure Set_Has_Type (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Type (Get_Kind (N)),
                     "no field Has_Type");
      Set_Flag4 (N, Flag);
   end Set_Has_Type;

   function Get_Has_Direction (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Direction (Get_Kind (N)),
                     "no field Has_Direction");
      return Get_Flag5 (N);
   end Get_Has_Direction;

   procedure Set_Has_Direction (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Direction (Get_Kind (N)),
                     "no field Has_Direction");
      Set_Flag5 (N, Flag);
   end Set_Has_Direction;

   function Get_Has_Parenthesis (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Parenthesis (Get_Kind (N)),
                     "no field Has_Parenthesis");
      return Get_Flag1 (N);
   end Get_Has_Parenthesis;

   procedure Set_Has_Parenthesis (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Parenthesis (Get_Kind (N)),
                     "no field Has_Parenthesis");
      Set_Flag1 (N, Flag);
   end Set_Has_Parenthesis;

   function Get_Has_Argument (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Argument (Get_Kind (N)),
                     "no field Has_Argument");
      return Get_Flag2 (N);
   end Get_Has_Argument;

   procedure Set_Has_Argument (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Argument (Get_Kind (N)),
                     "no field Has_Argument");
      Set_Flag2 (N, Flag);
   end Set_Has_Argument;

   function Get_Fully_Analyzed_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Fully_Analyzed_Flag (Get_Kind (N)),
                     "no field Fully_Analyzed_Flag");
      return Get_Flag8 (N);
   end Get_Fully_Analyzed_Flag;

   procedure Set_Fully_Analyzed_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Fully_Analyzed_Flag (Get_Kind (N)),
                     "no field Fully_Analyzed_Flag");
      Set_Flag8 (N, Flag);
   end Set_Fully_Analyzed_Flag;

   function Get_Resolved_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Resolved_Flag (Get_Kind (N)),
                     "no field Resolved_Flag");
      return Get_Flag8 (N);
   end Get_Resolved_Flag;

   procedure Set_Resolved_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Resolved_Flag (Get_Kind (N)),
                     "no field Resolved_Flag");
      Set_Flag8 (N, Flag);
   end Set_Resolved_Flag;

   function Get_Mark_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Mark_Flag (Get_Kind (N)),
                     "no field Mark_Flag");
      return Get_Flag9 (N);
   end Get_Mark_Flag;

   procedure Set_Mark_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Mark_Flag (Get_Kind (N)),
                     "no field Mark_Flag");
      Set_Flag9 (N, Flag);
   end Set_Mark_Flag;

   function Get_Is_Constant (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Is_Constant (Get_Kind (N)),
                     "no field Is_Constant");
      return Get_Flag4 (N);
   end Get_Is_Constant;

   procedure Set_Is_Constant (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Is_Constant (Get_Kind (N)),
                     "no field Is_Constant");
      Set_Flag4 (N, Flag);
   end Set_Is_Constant;

   function Get_Static_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Static_Flag (Get_Kind (N)),
                     "no field Static_Flag");
      return Get_Flag14 (N);
   end Get_Static_Flag;

   procedure Set_Static_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Static_Flag (Get_Kind (N)),
                     "no field Static_Flag");
      Set_Flag14 (N, Flag);
   end Set_Static_Flag;

   function Get_Has_Attribute (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Attribute (Get_Kind (N)),
                     "no field Has_Attribute");
      return Get_Flag19 (N);
   end Get_Has_Attribute;

   procedure Set_Has_Attribute (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Attribute (Get_Kind (N)),
                     "no field Has_Attribute");
      Set_Flag19 (N, Flag);
   end Set_Has_Attribute;

   function Get_Attribute_Full (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attribute_Full (Get_Kind (N)),
                     "no field Attribute_Full");
      return Get_Flag1 (N);
   end Get_Attribute_Full;

   procedure Set_Attribute_Full (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attribute_Full (Get_Kind (N)),
                     "no field Attribute_Full");
      Set_Flag1 (N, Flag);
   end Set_Attribute_Full;

   function Get_Attribute_Parallel (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attribute_Parallel (Get_Kind (N)),
                     "no field Attribute_Parallel");
      return Get_Flag2 (N);
   end Get_Attribute_Parallel;

   procedure Set_Attribute_Parallel (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Attribute_Parallel (Get_Kind (N)),
                     "no field Attribute_Parallel");
      Set_Flag2 (N, Flag);
   end Set_Attribute_Parallel;

   function Get_Other_Attributes (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Other_Attributes (Get_Kind (N)),
                     "no field Other_Attributes");
      return Get_Flag3 (N);
   end Get_Other_Attributes;

   procedure Set_Other_Attributes (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Other_Attributes (Get_Kind (N)),
                     "no field Other_Attributes");
      Set_Flag3 (N, Flag);
   end Set_Other_Attributes;

   function Get_Pure_Property (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pure_Property (Get_Kind (N)),
                     "no field Pure_Property");
      return Get_Flag1 (N);
   end Get_Pure_Property;

   procedure Set_Pure_Property (N : Node; P : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pure_Property (Get_Kind (N)),
                     "no field Pure_Property");
      Set_Flag1 (N, P);
   end Set_Pure_Property;

   function Get_Context_Property (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Context_Property (Get_Kind (N)),
                     "no field Context_Property");
      return Get_Flag2 (N);
   end Get_Context_Property;

   procedure Set_Context_Property (N : Node; P : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Context_Property (Get_Kind (N)),
                     "no field Context_Property");
      Set_Flag2 (N, P);
   end Set_Context_Property;

   function Get_Has_Extern_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Extern_Flag (Get_Kind (N)),
                     "no field Has_Extern_Flag");
      return Get_Flag11 (N);
   end Get_Has_Extern_Flag;

   procedure Set_Has_Extern_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Extern_Flag (Get_Kind (N)),
                     "no field Has_Extern_Flag");
      Set_Flag11 (N, Flag);
   end Set_Has_Extern_Flag;

   function Get_Virtual_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Virtual_Flag (Get_Kind (N)),
                     "no field Virtual_Flag");
      return Get_Flag12 (N);
   end Get_Virtual_Flag;

   procedure Set_Virtual_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Virtual_Flag (Get_Kind (N)),
                     "no field Virtual_Flag");
      Set_Flag12 (N, Flag);
   end Set_Virtual_Flag;

   function Get_Pure_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pure_Flag (Get_Kind (N)),
                     "no field Pure_Flag");
      return Get_Flag1 (N);
   end Get_Pure_Flag;

   procedure Set_Pure_Flag (N : Node; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Pure_Flag (Get_Kind (N)),
                     "no field Pure_Flag");
      Set_Flag1 (N, Flag);
   end Set_Pure_Flag;

   function Get_Join_Option (N : Node) return Join_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Join_Option (Get_Kind (N)),
                     "no field Join_Option");
      return Join_Type'Val (Get_State1 (N));
   end Get_Join_Option;

   procedure Set_Join_Option (N : Node; Opt : Join_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Join_Option (Get_Kind (N)),
                     "no field Join_Option");
      Set_State1 (N, Join_Type'Pos (Opt));
   end Set_Join_Option;

   function Get_Edge_Identifier (N : Node) return Edge_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Edge_Identifier (Get_Kind (N)),
                     "no field Edge_Identifier");
      return Edge_Type'Val (Get_State1 (N));
   end Get_Edge_Identifier;

   procedure Set_Edge_Identifier (N : Node; Edge : Edge_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Edge_Identifier (Get_Kind (N)),
                     "no field Edge_Identifier");
      Set_State1 (N, Edge_Type'Pos (Edge));
   end Set_Edge_Identifier;

   function Get_DPI_Spec (N : Node) return DPI_Spec_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_DPI_Spec (Get_Kind (N)),
                     "no field DPI_Spec");
      return DPI_Spec_Type'Val (Get_State1 (N));
   end Get_DPI_Spec;

   procedure Set_DPI_Spec (N : Node; Spec : DPI_Spec_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_DPI_Spec (Get_Kind (N)),
                     "no field DPI_Spec");
      Set_State1 (N, DPI_Spec_Type'Pos (Spec));
   end Set_DPI_Spec;

   function Get_Visibility (N : Node) return Visibility_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Visibility (Get_Kind (N)),
                     "no field Visibility");
      return Visibility_Type'Val (Get_State1 (N));
   end Get_Visibility;

   procedure Set_Visibility (N : Node; Vis : Visibility_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Visibility (Get_Kind (N)),
                     "no field Visibility");
      Set_State1 (N, Visibility_Type'Pos (Vis));
   end Set_Visibility;

   function Get_Class_Visibility (N : Node) return Visibility_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Visibility (Get_Kind (N)),
                     "no field Class_Visibility");
      return Visibility_Type'Val (Get_State1 (N));
   end Get_Class_Visibility;

   procedure Set_Class_Visibility (N : Node; Vis : Visibility_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Class_Visibility (Get_Kind (N)),
                     "no field Class_Visibility");
      Set_State1 (N, Visibility_Type'Pos (Vis));
   end Set_Class_Visibility;

   function Get_Has_Visibility (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Visibility (Get_Kind (N)),
                     "no field Has_Visibility");
      return Get_Flag11 (N);
   end Get_Has_Visibility;

   procedure Set_Has_Visibility (N : Node; F : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Has_Visibility (Get_Kind (N)),
                     "no field Has_Visibility");
      Set_Flag11 (N, F);
   end Set_Has_Visibility;

   function Get_Violation (N : Node) return Violation_Type is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Violation (Get_Kind (N)),
                     "no field Violation");
      return Violation_Type'Val (Get_State1 (N));
   end Get_Violation;

   procedure Set_Violation (N : Node; V : Violation_Type) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Violation (Get_Kind (N)),
                     "no field Violation");
      Set_State1 (N, Violation_Type'Pos (V));
   end Set_Violation;

   function Get_Random_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Random_Flag (Get_Kind (N)),
                     "no field Random_Flag");
      return Get_Flag12 (N);
   end Get_Random_Flag;

   procedure Set_Random_Flag (N : Node; F : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Random_Flag (Get_Kind (N)),
                     "no field Random_Flag");
      Set_Flag12 (N, F);
   end Set_Random_Flag;

   function Get_Randc_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Randc_Flag (Get_Kind (N)),
                     "no field Randc_Flag");
      return Get_Flag13 (N);
   end Get_Randc_Flag;

   procedure Set_Randc_Flag (N : Node; F : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Randc_Flag (Get_Kind (N)),
                     "no field Randc_Flag");
      Set_Flag13 (N, F);
   end Set_Randc_Flag;

   function Get_Size_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Size_Flag (Get_Kind (N)),
                     "no field Size_Flag");
      return Get_Flag2 (N);
   end Get_Size_Flag;

   procedure Set_Size_Flag (N : Node; F : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Size_Flag (Get_Kind (N)),
                     "no field Size_Flag");
      Set_Flag2 (N, F);
   end Set_Size_Flag;

   function Get_Type_Analyzed_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Analyzed_Flag (Get_Kind (N)),
                     "no field Type_Analyzed_Flag");
      return Get_Flag1 (N);
   end Get_Type_Analyzed_Flag;

   procedure Set_Type_Analyzed_Flag (N : Node; F : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Type_Analyzed_Flag (Get_Kind (N)),
                     "no field Type_Analyzed_Flag");
      Set_Flag1 (N, F);
   end Set_Type_Analyzed_Flag;

   function Get_Forward_Typedef_Flag (N : Node) return Boolean is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Forward_Typedef_Flag (Get_Kind (N)),
                     "no field Forward_Typedef_Flag");
      return Get_Flag4 (N);
   end Get_Forward_Typedef_Flag;

   procedure Set_Forward_Typedef_Flag (N : Node; F : Boolean) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Forward_Typedef_Flag (Get_Kind (N)),
                     "no field Forward_Typedef_Flag");
      Set_Flag4 (N, F);
   end Set_Forward_Typedef_Flag;

   function Get_Access (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Access (Get_Kind (N)),
                     "no field Access");
      return Get_Field1 (N);
   end Get_Access;

   procedure Set_Access (N : Node; Acc : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Access (Get_Kind (N)),
                     "no field Access");
      Set_Field1 (N, Acc);
   end Set_Access;

   function Get_Arg1 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Arg1 (Get_Kind (N)),
                     "no field Arg1");
      return Get_Field4 (N);
   end Get_Arg1;

   procedure Set_Arg1 (N : Node; Arg : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Arg1 (Get_Kind (N)),
                     "no field Arg1");
      Set_Field4 (N, Arg);
   end Set_Arg1;

   function Get_Arg2 (N : Node) return Node is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Arg2 (Get_Kind (N)),
                     "no field Arg2");
      return Get_Field5 (N);
   end Get_Arg2;

   procedure Set_Arg2 (N : Node; Arg : Node) is
   begin
      pragma Assert (N /= Null_Node);
      pragma Assert (Has_Arg2 (Get_Kind (N)),
                     "no field Arg2");
      Set_Field5 (N, Arg);
   end Set_Arg2;


end Verilog.Nodes;
