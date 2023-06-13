--  Verilog semantic analyzer (simulation)
--  Copyright (C) 2023 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Verilog.Types; use Verilog.Types;
with Verilog.Allocates; use Verilog.Allocates;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Simulation is
   --  Get current simulation time.
   function Get_Current_Time return Uns32;

   procedure Activate_Process (Proc : Process_Acc);

   procedure Execute_Statements
     (Link : in out Frame_Link_Type; Proc : Process_Acc);

   --  Exported for VPI.
   procedure Blocking_Assign_Lvalue
     (Frame : Frame_Ptr; Tgt : Node; Val : Data_Ptr; Etype : Node);

   procedure Assign_Vector (Dest : Data_Ptr;
                            Dest_Offset : Bit_Offset;
                            Dest_Width : Width_Type;
                            Dest_Type : Node;
                            Update : Update_Acc;
                            Val : Data_Ptr;
                            Voffset : Bit_Offset);

   --  Display values (reg and nets) initially and after each cycles.
   Flag_Trace_Values : Boolean := False;

   --  Include memories while displaying values.
   Flag_Trace_Memories : Boolean := False;

   Flag_Trace_Time : Boolean := False;

   Flag_Trace_Exec : Boolean := False;

   --  For $settrace/$cleartrace
   Flag_Trace : Boolean := False;

   procedure Run (Root : Node);
end Verilog.Simulation;
