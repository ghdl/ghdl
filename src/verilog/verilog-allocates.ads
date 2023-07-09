--  Object layout for verilog
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
with Types; use Types;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Types; use Verilog.Types;
with Verilog.Vpi; use Verilog.Vpi;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Allocates is
   --  Allocate resources: variables, wires, processes.
   --  Layout records, routines, and classes.  Create sensivity lists.

   type Frame_Link_Type is record
      Origin : Node;
      Pc : Node;
      Frame : Frame_Ptr;
   end record;

   Frame_Link_Size : constant Storage_Index := Frame_Link_Type'Size / 8;
   Frame_Link_Align : constant Storage_Index := Frame_Link_Type'Alignment;
   Frame_Link_Offset : constant Storage_Index := 0;

   type Frame_Link_Ptr is access all Frame_Link_Type;

   function To_Frame_Link_Ptr is new Ada.Unchecked_Conversion
     (Frame_Ptr, Frame_Link_Ptr);

   Global_Frame : Frame_Ptr;

   type Process_Kind is (Process_User,
                         Process_Assign, Process_Implicit_Assign,
                         Process_Gate,
                         Process_Conn_Input, Process_Conn_Output,
                         Process_Conn_Default);
   type Process_Type (Kind : Process_Kind);
   type Process_Acc is access Process_Type;

   type Process_Type (Kind : Process_Kind) is record
      --  True if process is scheduled in the active (or inactive) queue.
      --  This is to avoid to insert it twice.
      Is_Scheduled : Boolean;

      --  Statement for this process.
      Stmt : Node;

      --  Wakeup time.
      Wakeup : Uns32;

      --  Activated processes are chained using this field.
      Enabled_Chain : Process_Acc;

      --  Number of references through Update_El.
      Nref : Uns32;

      case Kind is
         when Process_User =>
            --  For delay control.
            Pause : Uns32;
            Count : Int32;
            Link : Frame_Link_Type;
         when Process_Assign
           | Process_Implicit_Assign
           | Process_Gate
           | Process_Conn_Input
           | Process_Conn_Output
           | Process_Conn_Default =>
            null;
      end case;
   end record;

   type Update_El;
   type Update_El_Acc is access Update_El;

   type Edge_Process_Type is record
      Kind : Nkinds_Edge;
      Expr : Node;
      Prev : Logic_Type;
      Updates : Update_El_Acc;
   end record;

   type Edge_Process_Acc is access Edge_Process_Type;

   type Update_Kind is (Update_Process, Update_Vpi, Update_Edge);

   type Update_Type;
   type Update_Acc is access Update_Type;

   type Update_Uncons_Array is array (Bit_Offset range <>) of Update_Acc;

   type Update_Array_Type (Last : Bit_Offset) is record
      Arr : Update_Uncons_Array (0 .. Last);
   end record;

   type Update_Array_Acc is access Update_Array_Type;

   --  Element of the update list.  The update list contains statement that
   --  are sensitive to an update of a variable.
   type Update_El (Kind : Update_Kind := Update_Process) is record
      --  Next element in the update list for the variable.
      Next : Update_El_Acc;

      case Kind is
         when Update_Process =>
            --  Process to activate.
            Proc : Process_Acc;

         when Update_Vpi =>
            Cb : Vpi.P_Cb_Data;

         when Update_Edge =>
            Edge : Edge_Process_Acc;
      end case;
   end record;

   --  The fundamental types.
   type Value_Kind is (Val_None, Val_Logic, Val_Vector, Val_Real, Val_Array);

   type Update_Type (Kind : Value_Kind := Val_None) is record
      Parent : Update_Acc;

      List : Update_El_Acc;

      case Kind is
         when Val_None =>
            null;
         when Val_Logic
           | Val_Real =>
            --  No sub-list for atomic types.
            null;
         when Val_Vector
           | Val_Array =>
            Arr : Update_Array_Acc;
      end case;
   end record;

   --  Add UPD in the update list of REF (a var/net).
   procedure Add_Updates (Ref : Node; El : Update_El_Acc);

   function Get_Var_Data (Frame : Frame_Ptr; Var : Node) return Data_Ptr;

   function Get_Var_Update (Var : Node) return Update_Acc;
   procedure Set_Var_Update (Var : Node; Upd : Update_Acc);

   function Get_Sub_Frame (Frame : Frame_Ptr; N : Node) return Frame_Ptr;

   function Get_Unpacked_Member_Offset (Member : Node) return Storage_Index;

   function Allocate_Frame (N : Node) return Frame_Ptr;
   procedure Deallocate_Frame (Frame : in out Frame_Ptr);

   procedure Allocate_Type (N : Node);

   --  Raw initialization of a class object.
   --  Does not handle constructors or inheritance.
   procedure Init_Class_Scope (Cls : Node; Frame : Frame_Ptr);

   function Allocate_Proc (Proc : Node) return Process_Acc;

   procedure Init (Dest : Data_Ptr; Atype : Node);

   --  Initialize DECL with x/0.
   procedure Clear_Var (Frame : Frame_Ptr; Decl : Node; Decl_Type : Node);

   --  Initialize DECL with default expression (or clear if none).
   procedure Init_Var (Frame : Frame_Ptr; Decl : Node);

   --  Walk the design, and allocate variables, nets...
   procedure Allocate_Resources (Units : Node; Root : Node);

   procedure Disp_Value (Value : Data_Ptr; Vtype : Node);

   --  Debug procedure: disp all vars, with value and updates.
   procedure Disp_All_Vars_Update;

   procedure Disp_All_Vars (With_Memories : Boolean);

   --  Size and alignment for SV type TYP.
   function Get_Storage_Size (Typ : Node) return Storage_Index;
   function Get_Storage_Align (Typ : Node) return Storage_Index;

   function Align_Storage_Size (Size : Storage_Index; Align : Storage_Index)
                               return Storage_Index;

   --  For parameters.
   procedure Allocate_Parameter (Param : Node; Expr : Node);
   function Get_Parameter_Data (Param : Node) return Data_Ptr;
end Verilog.Allocates;
