--  GHDL Run Time (GRT) -  command line options.
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
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Severity;

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

   --  Time resolution extracted from Flag_String, in multiple of -3:
   --  0: sec
   --  1: ms
   --  2: us
   --  3: ns
   --  4: ps
   --  5: fs
   subtype Natural_Time_Scale is Natural range 0 .. 5;
   Time_Resolution_Scale : Natural_Time_Scale;

   --  Set Time_Resolution_Scale from Flag_String.
   procedure Set_Time_Resolution;

   --  Display options help.
   --  Should not be called directly.
   procedure Help;

   --  Status from Decode_Option.
   type Decode_Option_Status is
     (
      --  Last option, next arguments aren't options.
      Decode_Option_Last,

      --  For options like --help: program shouldn't run.
      Decode_Option_Stop,

      --  Option was successfuly decoded.
      Decode_Option_Ok);

   --  Decode option Option and set Status.
   procedure Decode_Option
     (Option : String; Status : out Decode_Option_Status);

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

   --  For --assert-level
   --  Level at which an assert stop the simulation.
   Severity_Level : Integer := Grt.Severity.Failure_Severity;

   --  Level at which an assert displays a backtrace.
   Backtrace_Severity : Integer := Grt.Severity.None_Severity;

   --  How assertions are handled.
   type Assert_Handling is
     (Enable_Asserts,
      Disable_Asserts_At_Time_0,
      Disable_Asserts);

   --  Handling of assertions from IEEE library.
   Ieee_Asserts : Assert_Handling := Enable_Asserts;

   --  Handling of assertions (except from IEEE library).
   Asserts_Policy : Assert_Handling := Enable_Asserts;

   --  Set by --stop-delta=XXX to stop the simulation after XXX delta cycles.
   Stop_Delta : Natural := 5000;

   --  Set by --stop-time=X to stop the simulation at time X.
   Stop_Time : Std_Time := Std_Time'Last;

   --  Set by --no-run
   --  If set, do not simulate, only elaborate.
   Flag_No_Run : Boolean := False;

   type Activity_Mode is (Activity_All, Activity_Minimal, Activity_None);
   Flag_Activity : Activity_Mode := Activity_Minimal;

   --  If true, the simulation should be stopped.
   Break_Simulation : Boolean;

   --  Set by --thread=
   --  Number of threads used to do the simulation.
   --  1 mean no additionnal threads, 0 means as many threads as number of
   --  CPUs.
   Nbr_Threads : Natural := 1;

   -- If true, writes are made without buffering on a file opened in write_mode
   -- or append_mode (TEXTIO)
   Unbuffered_Writes : Boolean := False;

   --  Set maximum dynamic stack allocation.
   Max_Stack_Allocation : Ghdl_Index_Type := 128 * 1024;

   -- Report all uncovered cover points at the end of simulation
   Flag_Psl_Report_Uncovered : Boolean := False;

   --  Helper: extract time from STR (a number followed by a unit, without
   --  spaces; the number is optionnal).  In case of error, display an error
   --  message and returns -1.
   function Parse_Time (Str : String) return Std_Time;

   --  Simply linked list of generic override (option -gIDENT=VALUE).
   type Generic_Override_Type;
   type Generic_Override_Acc is access Generic_Override_Type;

   type Generic_Override_Type is record
      --  Name of the generic (lower case).
      Name : String_Access;

      --  Value.
      Value : String_Access;

      --  Simply linked list.
      Next : Generic_Override_Acc;
   end record;

   First_Generic_Override : Generic_Override_Acc;
   Last_Generic_Override : Generic_Override_Acc;
private
   pragma Export (C, Nbr_Threads, "grt_nbr_threads");
end Grt.Options;
