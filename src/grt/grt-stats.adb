--  GHDL Run Time (GRT) - statistics.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System; use System;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio; use Grt.Astdio;
with Grt.Signals;
with Grt.Processes;
with Grt.Types; use Grt.Types;
with Grt.Disp;

package body Grt.Stats is
   type Clock_T is new Integer;

   type Time_Stats is record
      Wall : Clock_T;
      User : Clock_T;
      Sys : Clock_T;
   end record;

   --  Number of CLOCK_T per second.
   One_Second : Clock_T;


   --  Get number of seconds per CLOCK_T.
   function Get_Clk_Tck return Clock_T;
   pragma Import (C, Get_Clk_Tck, "grt_get_clk_tck");

   --  Get wall, user and system times.
   --  This is a binding to times(2).
   procedure Get_Times (Wall : Address; User : Address; Sys : Address);
   pragma Import (C, Get_Times, "grt_get_times");

   procedure Get_Stats (Stats : out Time_Stats)
   is
   begin
      Get_Times (Stats.Wall'Address, Stats.User'Address, Stats.Sys'Address);
   end Get_Stats;

   function "-" (L : Time_Stats; R : Time_Stats) return Time_Stats
   is
   begin
      return Time_Stats'(Wall => L.Wall - R.Wall,
                         User => L.User - R.User,
                         Sys => L.Sys - R.Sys);
   end "-";

   function "+" (L : Time_Stats; R : Time_Stats) return Time_Stats
   is
   begin
      return Time_Stats'(Wall => L.Wall + R.Wall,
                         User => L.User + R.User,
                         Sys => L.Sys + R.Sys);
   end "+";

   procedure Put (Stream : FILEs; Val : Clock_T)
   is
      procedure Fprintf_Clock (Stream : FILEs; A, B : Clock_T);
      pragma Import (C, Fprintf_Clock, "__ghdl_fprintf_clock");

      Sec : Clock_T;
      Ms : Clock_T;
   begin
      Sec := Val / One_Second;

      --  Avoid overflow.
      Ms := ((Val mod One_Second) * 1000) / One_Second;

      Fprintf_Clock (Stream, Sec, Ms);
   end Put;

   procedure Put (Stream : FILEs; T : Time_Stats) is
   begin
      Put (Stream, "wall: ");
      Put (Stream, T.Wall);
      Put (Stream, "  user: ");
      Put (Stream, T.User);
      Put (Stream, "  sys: ");
      Put (Stream, T.Sys);
   end Put;

   type Counter_Kind is (Counter_Elab, Counter_Order,
                         Counter_Process, Counter_Update,
                         Counter_Next, Counter_Resume);

   type Counter_Array is array (Counter_Kind) of Time_Stats;
   Counters : Counter_Array := (others => (0, 0, 0));

   Init_Time : Time_Stats;
   Last_Counter : Counter_Kind;
   Last_Time : Time_Stats;

--     --  Stats at origin.
--     Start_Time : Time_Stats;
--     End_Elab_Time : Time_Stats;
--     End_Order_Time : Time_Stats;

--     Start_Proc_Time : Time_Stats;
--     Proc_Times : Time_Stats;

--     Start_Update_Time : Time_Stats;
--     Update_Times : Time_Stats;

--     Start_Next_Time_Time : Time_Stats;
--     Next_Time_Times : Time_Stats;

--     Start_Resume_Time : Time_Stats;
--     Resume_Times : Time_Stats;

--     Running_Time : Time_Stats;
--     Simu_Time : Time_Stats;

   procedure Start_Elaboration is
   begin
      One_Second := Get_Clk_Tck;

      Get_Stats (Init_Time);
      Last_Time := Init_Time;
      Last_Counter := Counter_Elab;
   end Start_Elaboration;

   procedure Change_Counter (Cnt : Counter_Kind)
   is
      New_Time : Time_Stats;
   begin
      Get_Stats (New_Time);
      Counters (Last_Counter) := Counters (Last_Counter)
        + (New_Time - Last_Time);
      Last_Time := New_Time;
      Last_Counter := Cnt;
   end Change_Counter;

   procedure Start_Order is
   begin
      Change_Counter (Counter_Order);
   end Start_Order;

   procedure Start_Processes is
   begin
      Change_Counter (Counter_Process);
   end Start_Processes;

   procedure Start_Update is
   begin
      Change_Counter (Counter_Update);
   end Start_Update;

   procedure Start_Next_Time is
   begin
      Change_Counter (Counter_Next);
   end Start_Next_Time;

   procedure Start_Resume is
   begin
      Change_Counter (Counter_Resume);
   end Start_Resume;

   procedure End_Simulation is
   begin
      Change_Counter (Last_Counter);
   end End_Simulation;

   procedure Disp_Signals_Stats
   is
      use Grt.Signals;
      Nbr_No_Drivers : Ghdl_I32;
      Nbr_Resolv : Ghdl_I32;
      Nbr_Multi_Src : Ghdl_I32;
      Nbr_Active : Ghdl_I32;
      Nbr_Drivers : Ghdl_I32;
      Nbr_Direct_Drivers : Ghdl_I32;

      type Propagation_Kind_Array is array (Propagation_Kind_Type) of Ghdl_I32;
      Propag_Count : Propagation_Kind_Array;

      type Mode_Array is array (Mode_Type) of Ghdl_I32;
      Mode_Counts : Mode_Array;

      type Mode_Name_Type is array (Mode_Type) of String (1 .. 4);
      Mode_Names : constant Mode_Name_Type := (Mode_B1 => "B1: ",
                                               Mode_E8 => "E8: ",
                                               Mode_E32 => "E32:",
                                               Mode_I32 => "I32:",
                                               Mode_I64 => "I64:",
                                               Mode_F64 => "F64:");
   begin
      Put (stdout, "Number of simple signals: ");
      Put_I32 (stdout, Ghdl_I32 (Sig_Table.Last - Sig_Table.First + 1));
      New_Line;
      Put (stdout, "Number of signals with projected wave: ");
      Put_I32 (stdout, Get_Nbr_Future);
      New_Line;

      Nbr_No_Drivers := 0;
      Nbr_Resolv := 0;
      Nbr_Multi_Src := 0;
      Nbr_Active := 0;
      Nbr_Drivers := 0;
      Nbr_Direct_Drivers := 0;
      Mode_Counts := (others => 0);
      for I in Sig_Table.First .. Sig_Table.Last loop
         declare
            Sig : Ghdl_Signal_Ptr;
            Trans : Transaction_Acc;
         begin
            Sig := Sig_Table.Table (I);
            if Sig.S.Mode_Sig in Mode_Signal_User then
               if Sig.S.Nbr_Drivers = 0 then
                  Nbr_No_Drivers := Nbr_No_Drivers + 1;
               end if;
               if Sig.S.Nbr_Drivers + Sig.Nbr_Ports > 1 then
                  Nbr_Multi_Src := Nbr_Multi_Src + 1;
               end if;
               if Sig.S.Resolv /= null then
                  Nbr_Resolv := Nbr_Resolv + 1;
               end if;
               Nbr_Drivers := Nbr_Drivers + Ghdl_I32 (Sig.S.Nbr_Drivers);
               for J in 1 .. Sig.S.Nbr_Drivers loop
                  Trans := Sig.S.Drivers (J - 1).Last_Trans;
                  if Trans /= null and then Trans.Kind = Trans_Direct then
                     Nbr_Direct_Drivers := Nbr_Direct_Drivers + 1;
                  end if;
               end loop;
            end if;
            Mode_Counts (Sig.Mode) := Mode_Counts (Sig.Mode) + 1;
            if Sig.Has_Active then
               Nbr_Active := Nbr_Active + 1;
            end if;
         end;
      end loop;
      Put (stdout, "Number of non-driven simple signals: ");
      Put_I32 (stdout, Nbr_No_Drivers);
      New_Line;
      Put (stdout, "Number of resolved simple signals: ");
      Put_I32 (stdout, Nbr_Resolv);
      New_Line;
      Put (stdout, "Number of multi-sourced signals: ");
      Put_I32 (stdout, Nbr_Multi_Src);
      New_Line;
      Put (stdout, "Number of signals whose activity is managed: ");
      Put_I32 (stdout, Nbr_Active);
      New_Line;
      Put (stdout, "Number of drivers: ");
      Put_I32 (stdout, Nbr_Drivers);
      New_Line;
      Put (stdout, "Number of direct drivers: ");
      Put_I32 (stdout, Nbr_Direct_Drivers);
      New_Line;
      Put (stdout, "Number of signals per mode:");
      New_Line;
      for I in Mode_Type loop
         Put (stdout, "  ");
         Put (stdout, Mode_Names (I));
         Put (stdout, "  ");
         Put_I32 (stdout, Mode_Counts (I));
         New_Line;
      end loop;
      New_Line;

      Propag_Count := (others => 0);
      for I in Propagation.First .. Propagation.Last loop
         Propag_Count (Propagation.Table (I).Kind) :=
           Propag_Count (Propagation.Table (I).Kind) + 1;
      end loop;

      Put (stdout, "Propagation table length: ");
      Put_I32 (stdout, Ghdl_I32 (Grt.Signals.Propagation.Last));
      New_Line;
      Put (stdout, "Propagation table count:");
      New_Line;
      for I in Propagation_Kind_Type loop
         if Propag_Count (I) /= 0 then
            Put (stdout, "  ");
            Grt.Disp.Disp_Propagation_Kind (I);
            Put (stdout, ": ");
            Put_I32 (stdout, Propag_Count (I));
            New_Line;
         end if;
      end loop;
   end Disp_Signals_Stats;

   --  Disp all statistics.
   procedure Disp_Stats
   is
      N : Ghdl_I64;
   begin
      Put (stdout, "total:          ");
      Put (stdout, Last_Time - Init_Time);
      New_Line (stdout);
      Put (stdout, " elab:          ");
      Put (stdout, Counters (Counter_Elab));
      New_Line (stdout);
      Put (stdout, " internal elab: ");
      Put (stdout, Counters (Counter_Order));
      New_Line (stdout);
      Put (stdout, " cycle (sum):   ");
      Put (stdout, Counters (Counter_Process) + Counters (Counter_Resume)
           + Counters (Counter_Update) + Counters (Counter_Next));
      New_Line (stdout);
      Put (stdout, "  processes:    ");
      Put (stdout, Counters (Counter_Process));
      New_Line (stdout);
      Put (stdout, "  resume:       ");
      Put (stdout, Counters (Counter_Resume));
      New_Line (stdout);
      Put (stdout, "  update:       ");
      Put (stdout, Counters (Counter_Update));
      New_Line (stdout);
      Put (stdout, "  next compute: ");
      Put (stdout, Counters (Counter_Next));
      New_Line (stdout);

      Disp_Signals_Stats;

      Put (stdout, "Number of delta cycles: ");
      Put_I32 (stdout, Ghdl_I32 (Processes.Nbr_Delta_Cycles));
      New_Line;
      Put (stdout, "Number of non-delta cycles: ");
      Put_I32 (stdout, Ghdl_I32 (Processes.Nbr_Cycles));
      New_Line;

      Put (stdout, "Nbr of events: ");
      Put_I32 (stdout, Signals.Nbr_Events);
      New_Line;
      Put (stdout, "Nbr of active: ");
      Put_I32 (stdout, Signals.Nbr_Active);
      New_Line;

      Put (stdout, "Number of processes: ");
      Put_I32 (stdout, Ghdl_I32 (Grt.Processes.Get_Nbr_Processes));
      New_Line;
      Put (stdout, "Number of sensitized processes: ");
      Put_I32 (stdout, Ghdl_I32 (Grt.Processes.Get_Nbr_Sensitized_Processes));
      New_Line;
      Put (stdout, "Number of resumed processes: ");
      Put_I64 (stdout, Ghdl_I64 (Grt.Processes.Get_Nbr_Resumed_Processes));
      New_Line;
      Put (stdout, "Average number of resumed processes per cycle: ");
      N := Processes.Nbr_Delta_Cycles + Processes.Nbr_Cycles;
      if N = 0 then
         Put (stdout, "-");
      else
         Put_I64 (stdout, Ghdl_I64 (Processes.Get_Nbr_Resumed_Processes) / N);
      end if;
      New_Line;
   end Disp_Stats;
end Grt.Stats;
