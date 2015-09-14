--  GHDL Run Time (GRT) -  command line options.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Interfaces; use Interfaces;
with Grt.Errors; use Grt.Errors;
with Grt.Astdio;
with Grt.Hooks;

package body Grt.Options is

   Std_Standard_Time_Fs : Std_Time;
   Std_Standard_Time_Ps : Std_Time;
   Std_Standard_Time_Ns : Std_Time;
   Std_Standard_Time_Us : Std_Time;
   Std_Standard_Time_Ms : Std_Time;
   Std_Standard_Time_Sec : Std_Time;
   Std_Standard_Time_Min : Std_Time;
   Std_Standard_Time_Hr : Std_Time;
   pragma Export (C, Std_Standard_Time_Fs, "std__standard__time__BT__fs");
   pragma Weak_External (Std_Standard_Time_Fs);
   pragma Export (C, Std_Standard_Time_Ps, "std__standard__time__BT__ps");
   pragma Weak_External (Std_Standard_Time_Ps);
   pragma Export (C, Std_Standard_Time_Ns, "std__standard__time__BT__ns");
   pragma Weak_External (Std_Standard_Time_Ns);
   pragma Export (C, Std_Standard_Time_Us, "std__standard__time__BT__us");
   pragma Weak_External (Std_Standard_Time_Us);
   pragma Export (C, Std_Standard_Time_Ms, "std__standard__time__BT__ms");
   pragma Weak_External (Std_Standard_Time_Ms);
   pragma Export (C, Std_Standard_Time_Sec, "std__standard__time__BT__sec");
   pragma Weak_External (Std_Standard_Time_Sec);
   pragma Export (C, Std_Standard_Time_Min, "std__standard__time__BT__min");
   pragma Weak_External (Std_Standard_Time_Min);
   pragma Export (C, Std_Standard_Time_Hr, "std__standard__time__BT__hr");
   pragma Weak_External (Std_Standard_Time_Hr);

   procedure Set_Time_Resolution (Res : Character)
   is
   begin
      Std_Standard_Time_Hr := 0;
      case Res is
         when 'f' =>
            Std_Standard_Time_Fs := 1;
            Std_Standard_Time_Ps := 1000;
            Std_Standard_Time_Ns := 1000_000;
            Std_Standard_Time_Us := 1000_000_000;
            Std_Standard_Time_Ms := Std_Time'Last;
            Std_Standard_Time_Sec := Std_Time'Last;
            Std_Standard_Time_Min := Std_Time'Last;
            Std_Standard_Time_Hr := Std_Time'Last;
         when 'p' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 1;
            Std_Standard_Time_Ns := 1000;
            Std_Standard_Time_Us := 1000_000;
            Std_Standard_Time_Ms := 1000_000_000;
            Std_Standard_Time_Sec := Std_Time'Last;
            Std_Standard_Time_Min := Std_Time'Last;
            Std_Standard_Time_Hr := Std_Time'Last;
         when 'n' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 0;
            Std_Standard_Time_Ns := 1;
            Std_Standard_Time_Us := 1000;
            Std_Standard_Time_Ms := 1000_000;
            Std_Standard_Time_Sec := 1000_000_000;
            Std_Standard_Time_Min := Std_Time'Last;
            Std_Standard_Time_Hr := Std_Time'Last;
         when 'u' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 0;
            Std_Standard_Time_Ns := 0;
            Std_Standard_Time_Us := 1;
            Std_Standard_Time_Ms := 1000;
            Std_Standard_Time_Sec := 1000_000;
            Std_Standard_Time_Min := 60_000_000;
            Std_Standard_Time_Hr := Std_Time'Last;
         when 'm' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 0;
            Std_Standard_Time_Ns := 0;
            Std_Standard_Time_Us := 0;
            Std_Standard_Time_Ms := 1;
            Std_Standard_Time_Sec := 1000;
            Std_Standard_Time_Min := 60_000;
            Std_Standard_Time_Hr := 3600_000;
         when 's' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 0;
            Std_Standard_Time_Ns := 0;
            Std_Standard_Time_Us := 0;
            Std_Standard_Time_Ms := 0;
            Std_Standard_Time_Sec := 1;
            Std_Standard_Time_Min := 60;
            Std_Standard_Time_Hr := 3600;
         when 'M' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 0;
            Std_Standard_Time_Ns := 0;
            Std_Standard_Time_Us := 0;
            Std_Standard_Time_Ms := 0;
            Std_Standard_Time_Sec := 0;
            Std_Standard_Time_Min := 1;
            Std_Standard_Time_Hr := 60;
         when 'h' =>
            Std_Standard_Time_Fs := 0;
            Std_Standard_Time_Ps := 0;
            Std_Standard_Time_Ns := 0;
            Std_Standard_Time_Us := 0;
            Std_Standard_Time_Ms := 0;
            Std_Standard_Time_Sec := 0;
            Std_Standard_Time_Min := 0;
            Std_Standard_Time_Hr := 1;
         when others =>
            Error ("bad time resolution");
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
      P (" --ieee-asserts=POLICY  enable or disable asserts from IEEE");
      P ("       POLICY is enable,disable,disable-at-0");
      P (" --stop-time=X     stop the simulation at time X");
      P ("       X is expressed as a time value, without spaces: 1ns, ps...");
      P (" --stop-delta=X    stop the simulation cycle after X delta");
      P (" --expect-failure  invert exit status");
      P (" --no-run          do not simulate, only elaborate");
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

   function To_Lower (C : Character) return Character is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Character'Pos (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

   procedure Decode_Option
     (Option : String; Status : out Decode_Option_Status)
   is
      pragma Assert (Option'First = 1);
      Len : constant Natural := Option'Last;
   begin
      Status := Decode_Option_Ok;
      if Option = "--" then
         Status := Decode_Option_Last;
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
      elsif Len > 18 and then Option (1 .. 18) = "--time-resolution=" then
         declare
            Res : Character;
            Unit : String (1 .. 3);
         begin
            Res := '?';
            if Len >= 20 then
               Unit (1) := To_Lower (Option (19));
               Unit (2) := To_Lower (Option (20));
               if Len = 20 then
                  if Unit (1 .. 2) = "fs" then
                     Res := 'f';
                  elsif Unit (1 .. 2) = "ps" then
                     Res := 'p';
                  elsif Unit (1 .. 2) = "ns" then
                     Res := 'n';
                  elsif Unit (1 .. 2) = "us" then
                     Res := 'u';
                  elsif Unit (1 .. 2) = "ms" then
                     Res := 'm';
                  elsif Unit (1 .. 2) = "hr" then
                     Res := 'h';
                  end if;
               elsif Len = 21 then
                  Unit (3) := To_Lower (Option (21));
                  if Unit = "min" then
                     Res := 'M';
                  elsif Unit = "sec" then
                     Res := 's';
                  end if;
               end if;
            end if;
            if Res = '?' then
               Error_C ("bad unit for '");
               Error_C (Option);
               Error_E ("'");
            else
               if Flag_String (5) = '-' then
                  Error ("time resolution is ignored");
               elsif Flag_String (5) = '?' then
                  if Stop_Time /= Std_Time'Last then
                     Error ("time resolution must be set "
                              & "before --stop-time");
                  else
                     Set_Time_Resolution (Res);
                  end if;
               elsif Flag_String (5) /= Res then
                  Error ("time resolution is fixed during analysis");
               end if;
            end if;
         end;
      elsif Len > 12 and then Option (1 .. 12) = "--stop-time=" then
         declare
            Ok : Boolean;
            Pos : Natural;
            Time : Integer_64;
            Unit : String (1 .. 3);
         begin
            Extract_Integer (Option (13 .. Len), Ok, Time, Pos);
            if not Ok then
               Time := 1;
            end if;
            if (Len - Pos + 1) not in 2 .. 3 then
               Error_C ("bad unit for '");
               Error_C (Option);
               Error_E ("'");
               return;
            end if;
            Unit (1) := To_Lower (Option (Pos));
            Unit (2) := To_Lower (Option (Pos + 1));
            if Len = Pos + 2 then
               Unit (3) := To_Lower (Option (Pos + 2));
            else
               Unit (3) := ' ';
            end if;
            if Unit = "fs " then
               null;
            elsif Unit = "ps " then
               Time := Time * (10 ** 3);
            elsif Unit = "ns " then
               Time := Time * (10 ** 6);
            elsif Unit = "us " then
               Time := Time * (10 ** 9);
            elsif Unit = "ms " then
               Time := Time * (10 ** 12);
            elsif Unit = "sec" then
               Time := Time * (10 ** 15);
            elsif Unit = "min" then
               Time := Time * (10 ** 15) * 60;
            elsif Unit = "hr " then
               Time := Time * (10 ** 15) * 3600;
            else
               Error_C ("bad unit name for '");
               Error_C (Option);
               Error_E ("'");
            end if;
            Stop_Time := Std_Time (Time);
         end;
      elsif Len > 13 and then Option (1 .. 13) = "--stop-delta=" then
         declare
            Ok : Boolean;
            Pos : Natural;
            Time : Integer_64;
         begin
            Extract_Integer (Option (14 .. Len), Ok, Time, Pos);
            if not Ok or else Pos <= Len then
               Error_C ("bad value in '");
               Error_C (Option);
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
         if Option (16 .. Len) = "note" then
            Severity_Level := Note_Severity;
         elsif Option (16 .. Len) = "warning" then
            Severity_Level := Warning_Severity;
         elsif Option (16 .. Len) = "error" then
            Severity_Level := Error_Severity;
         elsif Option (16 .. Len) = "failure" then
            Severity_Level := Failure_Severity;
         elsif Option (16 .. Len) = "none" then
            Severity_Level := 4;
         else
            Error ("bad argument for --assert-level option, try --help");
         end if;
      elsif Len > 15 and then Option (1 .. 15) = "--ieee-asserts=" then
         if Option (16 .. Len) = "disable" then
            Ieee_Asserts := Disable_Asserts;
         elsif Option (16 .. Len) = "enable" then
            Ieee_Asserts := Enable_Asserts;
         elsif Option (16 .. Len) = "disable-at-0" then
            Ieee_Asserts := Disable_Asserts_At_Time_0;
         else
            Error ("bad argument for --ieee-asserts option, try --help");
         end if;
      elsif Option = "--expect-failure" then
         Expect_Failure := True;
      elsif Len >= 13 and then Option (1 .. 13) = "--stack-size=" then
         Warning ("option --stack-size is deprecated");
      elsif Len >= 17 and then Option (1 .. 17) = "--stack-max-size=" then
         Warning ("option --stack-max-size is deprecated");
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
               Error_C ("bad value in '");
               Error_C (Option);
               Error_E ("'");
            else
               Nbr_Threads := Integer (Val);
            end if;
         end;
      elsif Len > 4 and then Option (1 .. 2) = "-g" then
         if Option (3) = '=' then
            Error_C ("missing generic name in '");
            Error_C (Option);
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
                  Error_C ("missing '=' after generic name in '");
                  Error_C (Option);
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
      elsif not Grt.Hooks.Call_Option_Hooks (Option) then
         Error_C ("unknown option '");
         Error_C (Option);
         Error_E ("', try --help");
      end if;
   end Decode_Option;

   procedure Decode (Stop : out Boolean)
   is
      Arg : Ghdl_C_String;
      Len : Natural;
      Status : Decode_Option_Status;
   begin
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
