--  Simulation of VHDL
--  Copyright (C) 2022 Tristan Gingold
--
--  This file is part of GHDL.
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
with Tables;
with Areapools;

with Vhdl.Nodes; use Vhdl.Nodes;

with Grt.Vhdl_Types; use Grt.Vhdl_Types;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

with Simul.Vhdl_Elab; use Simul.Vhdl_Elab;

with Grt.Signals;

package Simul.Vhdl_Simul is
   Break_Time : Std_Time;

   Trace_Simulation : Boolean := False;

   Flag_Interractive : Boolean := False;
   Flag_Debug_Elab : Boolean := False;

   Trace_Residues : Boolean := False;

   type Process_Kind is (Kind_Process, Kind_PSL);

   type Boolean_Vector is array (Nat32 range <>) of Boolean;
   type Boolean_Vector_Acc is access Boolean_Vector;

   -- State associed with each process.
   type Process_State_Type (Kind : Process_Kind := Kind_Process) is record
      --  True if the process has a suspend state.
      --  (Used for procedure calls)
      Has_State : Boolean;

      --  The process instance.
      Top_Instance : Synth_Instance_Acc := null;
      Proc : Node := Null_Node;

      Idx : Process_Index_Type;

      -- The stack of the process.
      Instance : Synth_Instance_Acc := null;

      case Kind is
         when Kind_Process =>
            --  Memory pool to allocate objects from.
            Pool : Areapools.Areapool_Acc;
         when Kind_PSL =>
            Done : Boolean;
            States: Boolean_Vector_Acc;
      end case;
   end record;
   type Process_State_Acc is access all Process_State_Type;

   type Process_State_Array is
      array (Process_Index_Type range <>) of aliased Process_State_Type;
   type Process_State_Array_Acc is access Process_State_Array;

   --  Array containing all processes.
   Processes_State: Process_State_Array_Acc;

   Current_Process: Process_State_Acc;

   -- If true, disp current time in assert message.
   Disp_Time_Before_Values: Boolean := False;

   Simulation_Finished : exception;

   procedure Simulation;

   --  Low level functions, for debugger.
   function Sig_Index (Base : Memory_Ptr; Idx : Uns32) return Memory_Ptr;
   function Read_Sig (Mem : Memory_Ptr) return Grt.Signals.Ghdl_Signal_Ptr;


   --  Tables visible to the debugger.

   type Scalar_Quantity_Record is record
      --  Index in Y or Yp vector.
      Idx : Integer;
      --  If there is a 'Dot, the corresponding entry.
      Deriv : Scalar_Quantity_Index;
      --  If there is a 'Integ, the corresponding entry.
      Integ : Scalar_Quantity_Index;
   end record;

   package Scalar_Quantities_Table is new Tables
     (Table_Component_Type => Scalar_Quantity_Record,
      Table_Index_Type => Scalar_Quantity_Index,
      Table_Low_Bound => No_Scalar_Quantity + 1,
      Table_Initial => 128);

   type Contrib_Index_Type is new Uns32;

   type Scalar_Terminal_Record is record
      --  Index in Y vector
      Ref_Idx : Integer;
      --  Number of contributions.
      Nbr_Contrib : Natural;
      First_Contrib : Contrib_Index_Type;
   end record;

   package Scalar_Terminals_Table is new Tables
     (Table_Component_Type => Scalar_Terminal_Record,
      Table_Index_Type => Scalar_Terminal_Index,
      Table_Low_Bound => No_Scalar_Terminal + 1,
      Table_Initial => 64);

   type Augmentation_Kind is
     (
      Aug_Noise,
      Aug_Spectrum,
      Aug_Dot,
      Aug_Integ,
      Aug_Delayed
     );

   pragma Unreferenced (Aug_Spectrum, Aug_Integ, Aug_Delayed);

   type Augmentation_Entry (Kind : Augmentation_Kind := Aug_Noise) is record
      Q : Scalar_Quantity_Index;
   end record;

   package Augmentations_Set is new Tables
     (Table_Component_Type => Augmentation_Entry,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 64);

   Nbr_Solver_Variables : Natural := 0;

end Simul.Vhdl_Simul;
