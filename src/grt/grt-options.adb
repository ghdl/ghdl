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
with Interfaces; use Interfaces;
with Grt.Strings; use Grt.Strings;
with Grt.Errors; use Grt.Errors;
with Grt.Severity; use Grt.Severity;
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio;
with Grt.Hooks;
with Grt.Wave_Opt.File;

package body Grt.Options is

   procedure Set_Time_Resolution is
   begin
      case Flag_String (5) is
         when 'f' | '-' =>
            Time_Resolution_Scale := 5;
         when 'p' =>
            Time_Resolution_Scale := 4;
         when 'n' =>
            Time_Resolution_Scale := 3;
         when 'u' =>
            Time_Resolution_Scale := 2;
         when 'm' =>
            Time_Resolution_Scale := 1;
         when 's' =>
            Time_Resolution_Scale := 0;
         when others =>
            Error ("unhandled time resolution");
      end case;
   end Set_Time_Resolution;

   procedure Help
   is
      use Grt.Astdio;
      procedure P (Str : String) renames Put_Line;
      Prog_Name : Ghdl_C_String;
   begin
      if Argc > 0 then
         Prog_Name := Argv (0);
         Put ("Usage: ");
         Put (Prog_Name (1 .. strlen (Prog_Name)));
         Put (" [OPTIONS]");
         New_Line;
      end if;

      P ("Options are:");
      P (" --help, -h        disp this help");
      P (" --assert-level=LEVEL   stop simulation if assert at LEVEL");
      P ("       LEVEL is note,warning,error,failure,none");
      P (" --backtrace-severity=LEVEL  display a backtrace for assertions");
      P (" --ieee-asserts=POLICY  enable or disable asserts from IEEE");
      P ("       POLICY is enable, disable, disable-at-0");
      P (" --asserts=POLICY  enable or disable asserts");
      P (" --stop-time=X     stop the simulation at time X");
      P ("       X is expressed as a time value, without spaces: 1ns, ps...");
      P (" --stop-delta=X    stop the simulation cycle after X delta");
      P (" --expect-failure  invert exit status");
      P (" --max-stack-alloc=X  error if variables are larger than X KB");
      P (" --no-run          do not simulate, only elaborate");
      P (" --unbuffered      disable buffering on stdout, stderr and");
      P ("                   files opened in write or append mode (TEXTIO).");
      P (" --read-wave-opt=FILENAME  read a wave option file.");
      P (" --write-wave-opt=FILENAME  write a wave option file.");
      P (" --psl-report-uncovered Reports all uncovered PSL cover points as");
      P ("                        warning at the end of simulation");
      --  P (" --threads=N       use N threads for simulation");
      P ("Additional features:");
      P (" --has-feature=X   test presence of feature X");
      P (" --list-features   display the list of features");
      Grt.Hooks.Call_Help_Hooks;
      P ("trace options:");
      P (" --disp-time       disp time as simulation advances");
      P (" --trace-signals   disp signals after each cycle");
      P (" --trace-processes disp process name before each cycle");
      P (" --stats           display run-time statistics");
      P ("debug options:");
      P (" --disp-order      disp signals order");
      P (" --disp-sources    disp sources while displaying signals");
      P (" --disp-sig-types  disp signal types");
      P (" --disp-signals-map    disp map bw declared sigs and internal sigs");
      P (" --disp-signals-table  disp internal signals");
      P (" --checks          do internal checks after each process run");
      P (" --activity=LEVEL  watch activity of LEVEL signals");
      P ("       LEVEL is all, min (default) or none (unsafe)");
   end Help;

   --  Extract from STR a number.
   --  First, all leading blanks are skipped.
   --  Then, all next digits are eaten.
   --  The position of the first non digit or one past the upper bound is
   --  returned into POS.
   --  If there is no digits, OK is set to false, else to true.
   procedure Extract_Integer
     (Str : String;
      Ok : out Boolean;
      Result : out Integer_64;
      Pos : out Natural)
   is
   begin
      Pos := Str'First;
      --  Skip blanks.
      while Pos <= Str'Last and then Str (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;
      Ok := False;
      Result := 0;
      loop
         exit when Pos > Str'Last or else Str (Pos) not in '0' .. '9';
         Ok := True;
         Result := Result * 10
           + (Character'Pos (Str (Pos)) - Character'Pos ('0'));
         Pos := Pos + 1;
      end loop;
   end Extract_Integer;

   function Parse_Time (Str : String) return Std_Time
   is
      Ok : Boolean;
      Pos : Natural;
      Time : Integer_64;
      Unit : String (1 .. 3);
      Scale : Natural_Time_Scale;
   begin
      Extract_Integer (Str, Ok, Time, Pos);
      if not Ok then
         Time := 1;
      end if;

      --  Check unit length and convert it to lower case.
      if Str'Last = Pos + 1 then
         Unit (3) := ' ';
      elsif Str'Last = Pos + 2 then
         Unit (3) := To_Lower (Str (Pos + 2));
      else
         Error_S ("bad unit for '");
         Diag_C (Str);
         Error_E ("'");
         return -1;
      end if;
      Unit (1) := To_Lower (Str (Pos));
      Unit (2) := To_Lower (Str (Pos + 1));

      if Unit = "fs " then
         Scale := 5;
      elsif Unit = "ps " then
         Scale := 4;
      elsif Unit = "ns " then
         Scale := 3;
      elsif Unit = "us " then
         Scale := 2;
      elsif Unit = "ms " then
         Scale := 1;
      elsif Unit = "sec" then
         Scale := 0;
      else
         Error_S ("bad unit name for '");
         Diag_C (Str);
         Error_E ("'");
         return -1;
      end if;
      if Scale > Time_Resolution_Scale then
         Error_S ("unit for '");
         Diag_C (Str);
         Error_E ("' is less than time resolution");
         return -1;
      end if;
      while Scale < Time_Resolution_Scale loop
         Time := Time * 1000;
         Scale := Scale + 1;
      end loop;
      return Std_Time (Time);
   end Parse_Time;

   function Parse_Severity (Opt_Name : String; Arg : String) return Integer is
   begin
      if Arg = "note" then
         return Note_Severity;
      elsif Arg = "warning" then
         return Warning_Severity;
      elsif Arg = "error" then
         return Error_Severity;
      elsif Arg = "failure" then
         return Failure_Severity;
      elsif Arg = "none" then
         return 4;
      else
         Error_S ("bad argument for ");
         Diag_C (Opt_Name);
         Error_E (" option, try --help");
         return -1;
      end if;
   end Parse_Severity;

   function Parse_Policy (Opt_Name : String; Arg : String)
                          return Assert_Handling is
   begin
      if Arg = "disable" then
         return Disable_Asserts;
      elsif Arg = "enable" then
         return Enable_Asserts;
      elsif Arg = "disable-at-0" then
         return Disable_Asserts_At_Time_0;
      else
         Error_S ("bad argument for ");
         Diag_C (Opt_Name);
         Error_E (" option, try --help");
         return Enable_Asserts;
      end if;
   end Parse_Policy;

   procedure Decode_Option
     (Option : String; Status : out Decode_Option_Status)
   is
      pragma Assert (Option'First = 1);
      Len : constant Natural := Option'Last;
   begin
      Status := Decode_Option_Ok;
      if Option = "--" then
         Status := Decode_Option_Last;
      elsif Option (1) = '+' then
         --  For VPI/VHPI - plusargs.
         null;
      elsif Option = "--help" or else Option = "-h" then
         Help;
         Status := Decode_Option_Stop;
      elsif Option = "--list-features" then
         Grt.Hooks.Display_Hooks_Desc;
         Status := Decode_Option_Stop;
      elsif Len > 14 and then Option (1 .. 14) = "--has-feature=" then
         if Grt.Hooks.Has_Feature (Option (15 .. Len)) then
            Grt.Errors.Exit_Status := 0;
         else
            Grt.Errors.Exit_Status := 1;
         end if;
         Status := Decode_Option_Stop;
      elsif Option = "--disp-time" then
         Disp_Time := True;
      elsif Option = "--trace-signals" then
         Trace_Signals := True;
         Disp_Time := True;
      elsif Option = "--trace-processes" then
         Trace_Processes := True;
         Disp_Time := True;
      elsif Option = "--disp-order" then
         Disp_Signals_Order := True;
      elsif Option = "--checks" then
         Checks := True;
      elsif Option = "--disp-sources" then
         Disp_Sources := True;
      elsif Option = "--disp-sig-types" then
         Disp_Sig_Types := True;
      elsif Option = "--disp-signals-map" then
         Disp_Signals_Map := True;
      elsif Option = "--disp-signals-table" then
         Disp_Signals_Table := True;
      elsif Option = "--disp-sensitivity" then
         Disp_Sensitivity := True;
      elsif Option = "--stats" then
         Flag_Stats := True;
      elsif Option = "--no-run" then
         Flag_No_Run := True;
      elsif Len > 12 and then Option (1 .. 12) = "--stop-time=" then
         Stop_Time := Parse_Time (Option (13 .. Len));
         if Stop_Time = -1 then
            --  In case of error...
            return;
         end if;
      elsif Len > 13 and then Option (1 .. 13) = "--stop-delta=" then
         declare
            Ok : Boolean;
            Pos : Natural;
            Time : Integer_64;
         begin
            Extract_Integer (Option (14 .. Len), Ok, Time, Pos);
            if not Ok or else Pos <= Len then
               Error_S ("bad value in '");
               Diag_C (Option);
               Error_E ("'");
            else
               if Time > Integer_64 (Integer'Last) then
                  Stop_Delta := Integer'Last;
               else
                  Stop_Delta := Integer (Time);
               end if;
            end if;
         end;
      elsif Len > 15 and then Option (1 .. 15) = "--assert-level=" then
         declare
            Level : Integer;
         begin
            Level := Parse_Severity ("--assert-level", Option (16 .. Len));
            if Level >= 0 then
               Severity_Level := Level;
            end if;
         end;
      elsif Len > 21 and then Option (1 .. 21) = "--backtrace-severity=" then
         declare
            Level : Integer;
         begin
            Level := Parse_Severity
              ("--backtrace-severity", Option (22 .. Len));
            if Level >= 0 then
               Backtrace_Severity := Level;
            end if;
         end;
      elsif Len > 15 and then Option (1 .. 15) = "--ieee-asserts=" then
         Ieee_Asserts := Parse_Policy ("--ieee-asserts", Option (16 .. Len));
      elsif Len > 10 and then Option (1 .. 10) = "--asserts=" then
         Asserts_Policy := Parse_Policy ("--asserts", Option (11 .. Len));
         Ieee_Asserts := Asserts_Policy;
      elsif Option = "--expect-failure" then
         Expect_Failure := True;
      elsif Len >= 13 and then Option (1 .. 13) = "--stack-size=" then
         Warning ("option --stack-size is deprecated");
      elsif Len >= 17 and then Option (1 .. 17) = "--stack-max-size=" then
         Warning ("option --stack-max-size is deprecated");
      elsif Len >= 18 and then Option (1 .. 18) = "--max-stack-alloc=" then
         declare
            Ok : Boolean;
            Pos : Natural;
            Val : Integer_64;
         begin
            Extract_Integer (Option (19 .. Len), Ok, Val, Pos);
            if not Ok or else Pos <= Len then
               Error_S ("bad value in '");
               Diag_C (Option);
               Error_E ("'");
            else
               Max_Stack_Allocation := Ghdl_Index_Type (Val * 1024);
            end if;
         end;
      elsif Len >= 11 and then Option (1 .. 11) = "--activity=" then
         if Option (12 .. Len) = "none" then
            Flag_Activity := Activity_None;
         elsif Option (12 .. Len) = "min" then
            Flag_Activity := Activity_Minimal;
         elsif Option (12 .. Len) = "all" then
            Flag_Activity := Activity_All;
         else
            Error ("bad argument for --activity, try --help");
         end if;
      elsif Len > 10 and then Option (1 .. 10) = "--threads=" then
         declare
            Ok : Boolean;
            Pos : Natural;
            Val : Integer_64;
         begin
            Extract_Integer (Option (11 .. Len), Ok, Val, Pos);
            if not Ok or else Pos <= Len then
               Error_S ("bad value in '");
               Diag_C (Option);
               Error_E ("'");
            else
               Nbr_Threads := Integer (Val);
            end if;
         end;
      elsif Len > 4 and then Option (1 .. 2) = "-g" then
         if Option (3) = '=' then
            Error_S ("missing generic name in '");
            Diag_C (Option);
            Error_E ("'");
            return;
         end if;
         declare
            Eq_Pos : Natural;
            Over : Generic_Override_Acc;
            Name : String_Access;
         begin
            if Option (3) = '\' then
               --  Extended identifier (not yet handled).
               raise Program_Error;
            else
               --  Search for '='.
               Eq_Pos := 0;
               for I in 3 .. Option'Last loop
                  if Option (I) = '=' then
                     Eq_Pos := I;
                     exit;
                  end if;
               end loop;
               if Eq_Pos = 0 then
                  Error_S ("missing '=' after generic name in '");
                  Diag_C (Option);
                  Error_E ("'");
               end if;
               Name := new String (1 .. Eq_Pos - 3);
               for I in 3 .. Eq_Pos - 1 loop
                  Name (I - 2) := To_Lower (Option (I));
               end loop;
            end if;
            Over := new Generic_Override_Type'
              (Name => Name,
               Value => new String'(Option (Eq_Pos + 1 .. Option'Last)),
               Next => null);
            --  Append.
            if Last_Generic_Override /= null then
               Last_Generic_Override.Next := Over;
            else
               First_Generic_Override := Over;
            end if;
            Last_Generic_Override := Over;
         end;
      elsif Option = "--unbuffered" then
         Unbuffered_Writes := True;
         setbuf (stdout, NULL_voids);
         setbuf (stderr, NULL_voids);
      elsif Len >= 16 and then Option (1 .. 16) = "--read-wave-opt="
      then
         Wave_Opt.File.Start
           (Option (17 .. Option'Last), To_Be_Created => False);
      elsif Len >= 17 and then Option (1 .. 17) = "--write-wave-opt="
      then
         Wave_Opt.File.Start
           (Option (18 .. Option'Last), To_Be_Created => True);
      elsif Option = "--psl-report-uncovered" then
         Flag_Psl_Report_Uncovered := True;
      elsif not Grt.Hooks.Call_Option_Hooks (Option) then
         Error_S ("unknown run option '");
         Diag_C (Option);
         Error_E ("', try --help");
      end if;
   end Decode_Option;

   procedure Decode (Stop : out Boolean)
   is
      Arg : Ghdl_C_String;
      Len : Natural;
      Status : Decode_Option_Status;
   begin
      --  Must be done before decoding options.
      Set_Time_Resolution;

      Stop := False;
      Last_Opt := Argc - 1;
      for I in 1 .. Argc - 1 loop
         Arg := Argv (I);
         Len := strlen (Arg);
         declare
            Argument : constant String := Arg (1 .. Len);
         begin
            Decode_Option (Argument, Status);
            case Status is
               when Decode_Option_Last =>
                  Last_Opt := I;
                  exit;
               when Decode_Option_Stop =>
                  Stop := True;
               when Decode_Option_Ok =>
                  null;
            end case;
         end;
      end loop;
   end Decode;
end Grt.Options;
