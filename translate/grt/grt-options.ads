--  GHDL Run Time (GRT) -  command line options.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Grt.Types; use Grt.Types;
with Grt.Lib; use Grt.Lib;

package Grt.Options is
   pragma Preelaborate (Grt.Options);

   --  Name of the program, set by argv[0].
   --  Must be set before calling DECODE.
   Progname : Ghdl_C_String;

   --  Arguments.
   --  This mimics argc/argv of 'main'.
   --  These must be set before calling DECODE.
   Argc : Integer;

   type Argv_Array_Type is array (Natural) of Ghdl_C_String;
   type Argv_Type is access Argv_Array_Type;

   Argv : Argv_Type;

   --  Last option decoded.
   --  Following arguments are reserved for the program.
   Last_Opt : Integer;

   --  Consistent flags used for analysis.
   --  Format is "VVitr", where:
   --    'VV' is the version (87, 93 or 08).
   --    'i' is the integer size ('i' for 32 bits, 'I' for 64 bits).
   --    't' is the time size ('t' for 32 bits, 'T' for 64 bits).
   --    'r' is the resolution ('?' for to be set by the user, '-' for any).
   Flag_String : constant String (1 .. 5);
   pragma Import (C, Flag_String, "__ghdl_flag_string");

   --  Display options help.
   --  Should not be called directly.
   procedure Help;

   --  Decode command line options.
   --  If STOP is true, there nothing must happen (set by --help).
   procedure Decode (Stop : out Boolean);

   --  Set by --disp-time (and --trace-signals, --trace-processes) to display
   --  time and deltas.
   Disp_Time : Boolean := False;

   --  Set by --trace-signals, to display signals after each cycle.
   Trace_Signals : Boolean := False;

   --  Set by --trace-processes, to display process name before being run.
   Trace_Processes : Boolean := False;

   --  Set by --disp-sig-types, to display signals and they types.
   Disp_Sig_Types : Boolean := False;

   Disp_Sources : Boolean := False;
   Disp_Signals_Map : Boolean := False;
   Disp_Signals_Table : Boolean := False;
   Disp_Sensitivity : Boolean := False;

   --  Set by --disp-order to diplay evaluation order of signals.
   Disp_Signals_Order : Boolean := False;

   --  Set by --stats to display statistics.
   Flag_Stats : Boolean := False;

   --  Set by --checks to do internal checks.
   Checks : Boolean := False;

   --  Level at which an assert stop the simulation.
   Severity_Level : Integer := Failure_Severity;

   --  How assertions are handled.
   type Assert_Handling is
     (Enable_Asserts,
      Disable_Asserts_At_Time_0,
      Disable_Asserts);

   --  Handling of assertions from IEEE library.
   Ieee_Asserts : Assert_Handling := Enable_Asserts;

   --  Set by --stop-time=XXX to stop the simulation at or just after XXX.
   --  (unit is fs in fact).
   Stop_Time : Std_Time := Std_Time'Last;

   --  Set by --stop-delta=XXX to stop the simulation after XXX delta cycles.
   Stop_Delta : Natural := 5000;

   --  The default stack size for non-sensitized processes.
   Stack_Size : Natural := 8 * 1024;

   --  The maximum stack size for non-sensitized processes.
   Stack_Max_Size : Natural := 128 * 1024;

   --  Set by --no-run
   --  If set, do not simulate, only elaborate.
   Flag_No_Run : Boolean := False;

   type Activity_Mode is (Activity_All, Activity_Minimal, Activity_None);
   Flag_Activity : Activity_Mode := Activity_Minimal;

   --  Set by --thread=
   --  Number of threads used to do the simulation.
   --  1 mean no additionnal threads, 0 means as many threads as number of
   --  CPUs.
   Nbr_Threads : Natural := 1;

   --  Set the time resolution.
   --  Only call this subprogram if you are allowed to set the time resolution.
   procedure Set_Time_Resolution (Res : Character);
private
   pragma Export (C, Stack_Size);
   pragma Export (C, Stack_Max_Size);
   pragma Export (C, Nbr_Threads, "grt_nbr_threads");
end Grt.Options;
