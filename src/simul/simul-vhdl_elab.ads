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

with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Simul.Vhdl_Elab is
   procedure Gather_Processes (Top : Synth_Instance_Acc);
   procedure Elab_Processes;

   --  For the debugger.
   Top_Instance : Synth_Instance_Acc;

   --  For each signals:
   --  * drivers (process + area), sources
   --  * sensitivity
   --  * waveform assignments
   --  * decomposition level: none, vectors, full.
   --  * force/release
   --  * need to track activity
   --  * need to track events
   procedure Elab_Drivers;

   type Process_Index_Type is new Nat32;
   type Driver_Index_Type is new Nat32;
   subtype Sensitivity_Index_Type is Driver_Index_Type;

   No_Process_Index : constant Process_Index_Type := 0;
   No_Driver_Index : constant Driver_Index_Type := 0;
   No_Sensitivity_Index : constant Sensitivity_Index_Type := 0;

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

   package Simultaneous_Table is new Tables
     (Table_Component_Type => Simultaneous_Record,
      Table_Index_Type => Simultaneous_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   type Connect_Index_Type is new Nat32;
   No_Connect_Index : constant Connect_Index_Type := 0;

   type Connect_Endpoint is record
      Base : Signal_Index_Type;
      Offs : Value_Offsets;
      Typ : Type_Acc;
   end record;

   --  Connections.  For each associations (block/component/entry), the
   --  elaborator adds an entry in that table.
   type Connect_Entry is record
      Formal : Connect_Endpoint;
      --  Next connection for the formal.
      Formal_Link : Connect_Index_Type;

      Actual : Connect_Endpoint;
      --  Next connection for the actual.
      Actual_Link : Connect_Index_Type;

      --  Whether it is a source for the actual or/and the actual.
      --  The correct word is 'source'.
      Drive_Formal : Boolean;
      Drive_Actual : Boolean;

      --  If true, the connection is fully collapsed: formal is the same
      --  signal as actual.
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

   type Signal_Entry (Kind : Mode_Signal_Type := Mode_Signal) is record
      Decl : Iir;
      Inst : Synth_Instance_Acc;
      Typ : Type_Acc;
      Val : Memory_Ptr;
      Sig : Memory_Ptr;

      --  Processes sensitized by this signal.
      Sensitivity : Sensitivity_Index_Type;

      --  This signal is identical to Collapsed_By, if set.
      Collapsed_By : Signal_Index_Type;

      case Kind is
         when Mode_Signal_User =>
            Drivers : Driver_Index_Type;
            Connect : Connect_Index_Type;
         when Mode_Quiet | Mode_Stable | Mode_Delayed
           | Mode_Transaction =>
            Time : Std_Time;
            Prefix : Memory_Ptr;
         when Mode_Above =>
            null;
         when Mode_Guard =>
            null;
         when Mode_Conv_In | Mode_Conv_Out | Mode_End =>
            --  Unused.
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
      Sig : Signal_Index_Type;
      Off : Value_Offsets;
      Typ : Type_Acc;
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

   type Scalar_Quantity_Index is new Uns32;
   No_Scalar_Quantity : constant Scalar_Quantity_Index := 0;

   type Quantity_Entry is record
      Decl : Iir;
      Inst : Synth_Instance_Acc;
      Typ : Type_Acc;
      Val : Memory_Ptr;
      --  Index in the scalar table.
      Idx : Scalar_Quantity_Index;
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
