--  Elaboration for VHDL simulation
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

with Grt.Vhdl_Types; use Grt.Vhdl_Types;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Simul.Vhdl_Elab is
   procedure Gather_Processes (Top : Synth_Instance_Acc);
   procedure Compute_Sources;
   procedure Elab_Processes;

   --  For the debugger.
   Top_Instance : Synth_Instance_Acc;

   --  Change the meaning of W (width) in T for simulation.
   procedure Convert_Type_Width (T : Type_Acc);

   type Process_Index_Type is new Nat32;
   type Driver_Index_Type is new Nat32;
   subtype Sensitivity_Index_Type is Driver_Index_Type;
   type Disconnect_Index_Type is new Nat32;

   No_Process_Index : constant Process_Index_Type := 0;
   No_Driver_Index : constant Driver_Index_Type := 0;
   No_Sensitivity_Index : constant Sensitivity_Index_Type := 0;
   No_Disconnect_Index : constant Disconnect_Index_Type := 0;

   type Proc_Record_Type is record
      Proc : Node;
      Inst : Synth_Instance_Acc;
      Drivers : Driver_Index_Type;
      Sensitivity : Sensitivity_Index_Type;
   end record;

   --  Table of all processes (explicit or implicit).
   package Processes_Table is new Tables
     (Table_Component_Type => Proc_Record_Type,
      Table_Index_Type => Process_Index_Type,
      Table_Low_Bound => No_Process_Index + 1,
      Table_Initial => 128);

   type Simultaneous_Record is record
      Stmt : Node;
      Inst : Synth_Instance_Acc;
   end record;

   type Simultaneous_Index_Type is new Nat32;

   --  Table of simple simultaneous statements.  Those are always considered.
   package Simultaneous_Table is new Tables
     (Table_Component_Type => Simultaneous_Record,
      Table_Index_Type => Simultaneous_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   --  Table of complex simultaneous statements.
   --  The simple (or procedural) simultaneous statements are extracted.
   package Complex_Simultaneous_Table is new Tables
     (Table_Component_Type => Simultaneous_Record,
      Table_Index_Type => Simultaneous_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   type Connect_Index_Type is new Nat32;
   No_Connect_Index : constant Connect_Index_Type := 0;

   type Sub_Signal_Type is record
      Base : Signal_Index_Type;
      Offs : Value_Offsets;
      Typ : Type_Acc;
   end record;

   --  Connections.  For each associations (block/component/entry), the
   --  elaborator adds an entry in that table.
   type Connect_Entry is record
      Formal : Sub_Signal_Type;
      --  Next connection for the formal.
      Formal_Link : Connect_Index_Type;

      Actual : Sub_Signal_Type;
      --  Next connection for the actual.
      Actual_Link : Connect_Index_Type;

      --  If true, the connection is collapsed: formal is the same (or a
      --  part) as the actual.
      Collapsed : Boolean;

      Assoc : Node;
      Assoc_Inst : Synth_Instance_Acc;
   end record;

   package Connect_Table is new Tables
     (Table_Component_Type => Connect_Entry,
      Table_Index_Type => Connect_Index_Type,
      Table_Low_Bound => No_Connect_Index + 1,
      Table_Initial => 32);

   --  Signals.

   --  Number of drivers and out connections for each scalar element.
   type Nbr_Sources_Type is record
      --  Number of processes that drive the signal.
      Nbr_Drivers : Uns32;

      --  Number of sources due to connections.
      Nbr_Conns : Uns32;

      --  Total number of sources, including sources of collapsed signals.
      Total : Uns32;

      --  Used only while computing the number of drivers: process for the
      --  last driver.
      Last_Proc : Process_Index_Type;
   end record;

   type Nbr_Sources_Array is array (Uns32 range <>) of Nbr_Sources_Type;
   type Nbr_Sources_Arr_Acc is access Nbr_Sources_Array;

   type Signal_Kind is (Signal_User,
                        Signal_Quiet, Signal_Stable,
                        Signal_Transaction,
                        Signal_Delayed,
                        Signal_Above,
                        Signal_Guard,
                        Signal_None);

   type Signal_Entry (Kind : Signal_Kind := Signal_User) is record
      Decl : Iir;
      Inst : Synth_Instance_Acc;
      Typ : Type_Acc;
      --  Initial value.
      Val_Init : Memory_Ptr;
      --  Current value.  In case of collapsed signal, this is the initial
      --  value of the collapsed_by signal.
      Val : Memory_Ptr;
      Sig : Memory_Ptr;

      --  Processes sensitized by this signal.
      Sensitivity : Sensitivity_Index_Type;

      --  This signal is collapsed by Collapsed_By, if set.
      --  Collapsed_Offs are the offset in Collapsed_By signal.
      Collapsed_By : Signal_Index_Type;
      Collapsed_Offs : Value_Offsets;

      --  Connections.  Non-user signals can only be actuals.
      Connect : Connect_Index_Type;

      Has_Active : Boolean;

      case Kind is
         when Signal_User =>
            Drivers : Driver_Index_Type;
            Disconnect : Disconnect_Index_Type;
            Nbr_Sources : Nbr_Sources_Arr_Acc;
         when Signal_Quiet | Signal_Stable | Signal_Delayed
           | Signal_Transaction =>
            Time : Std_Time;
            Pfx : Sub_Signal_Type;
         when Signal_Above =>
            null;
         when Signal_Guard =>
            null;
         when Signal_None =>
            null;
      end case;
   end record;

   package Signals_Table is new Tables
     (Table_Component_Type => Signal_Entry,
      Table_Index_Type => Signal_Index_Type,
      Table_Low_Bound => No_Signal_Index + 1,
      Table_Initial => 128);

   type Driver_Entry is record
      --  The signal having a driver.
      Sig : Sub_Signal_Type;
      --  Previous driver for the same signal.
      Prev_Sig : Driver_Index_Type;

      --  The process driving this signal.
      Proc : Process_Index_Type;
      --  Previous driver for the same process.
      Prev_Proc : Driver_Index_Type;
   end record;

   package Drivers_Table is new Tables
     (Table_Component_Type => Driver_Entry,
      Table_Index_Type => Driver_Index_Type,
      Table_Low_Bound => No_Driver_Index + 1,
      Table_Initial => 128);

   subtype Sensitivity_Entry is Driver_Entry;

   package Sensitivity_Table is new Tables
     (Table_Component_Type => Driver_Entry,
      Table_Index_Type => Sensitivity_Index_Type,
      Table_Low_Bound => No_Sensitivity_Index + 1,
      Table_Initial => 128);

   type Disconnect_Entry is record
      Sig : Sub_Signal_Type;
      Prev : Disconnect_Index_Type;
      Val : Std_Time;
   end record;

   package Disconnect_Table is new Tables
     (Table_Component_Type => Disconnect_Entry,
      Table_Index_Type => Disconnect_Index_Type,
      Table_Low_Bound => No_Disconnect_Index + 1,
      Table_Initial => 8);

   type Scalar_Quantity_Index is new Uns32;
   No_Scalar_Quantity : constant Scalar_Quantity_Index := 0;

   type Quantity_Entry is record
      --  Any quantity: free, branch, dot...
      Decl : Iir;
      Inst : Synth_Instance_Acc;
      Typ : Type_Acc;
      Val : Memory_Ptr;
      --  Index in the scalar table.
      Sq_Idx : Scalar_Quantity_Index;
      --  For across quantity, we need the terminals to compute the value
      --  For a through quantity, we need the terminals to compute the contrib
   end record;

   package Quantity_Table is new Tables
     (Table_Component_Type => Quantity_Entry,
      Table_Index_Type => Quantity_Index_Type,
      Table_Low_Bound => No_Quantity_Index + 1,
      Table_Initial => 128);

   type Scalar_Terminal_Index is new Uns32;
   No_Scalar_Terminal : constant Scalar_Terminal_Index := 0;

   type Terminal_Entry is record
      Decl : Iir;
      Inst : Synth_Instance_Acc;
      Across_Typ : Type_Acc;
      Through_Typ : Type_Acc;
      --  The reference value.
      Ref_Val : Memory_Ptr;
      --  Index in the scalar quantity table for the reference value.
      Ref_Idx : Scalar_Quantity_Index;
      --  Index in the scalar terminal table for the contribution.
      Term_Idx : Scalar_Terminal_Index;
   end record;

   package Terminal_Table is new Tables
     (Table_Component_Type => Terminal_Entry,
      Table_Index_Type => Terminal_Index_Type,
      Table_Low_Bound => No_Terminal_Index + 1,
      Table_Initial => 32);
end Simul.Vhdl_Elab;
