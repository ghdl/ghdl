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
      P (" --disp-tree[=KIND] disp the design hierarchy after elaboration");
      P ("       KIND is inst, proc, port (default)");
      P (" --expect-failure  invert exit status");
      P (" --stack-size=X    set the stack size of non-sensitized processes");
      P (" --stack-max-size=X  set the maximum stack size");
      P (" --no-run          do not simulate, only elaborate");
      --  P (" --threads=N       use N threads for simulation");
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

   function Extract_Size (Str : String; Option_Name : String) return Natural
   is
      Ok : Boolean;
      Val : Integer_64;
      Pos : Natural;
   begin
      Extract_Integer (Str, Ok, Val, Pos);
      if not Ok then
         Val := 1;
      end if;
      if Pos > Str'Last then
         --  No suffix.
         return Natural (Val);
      end if;
      if Pos = Str'Last
        or else (Pos + 1 = Str'Last
                 and then (Str (Pos + 1) = 'b' or Str (Pos + 1) = 'o'))
      then
         if Str (Pos) = 'k' or Str (Pos) = 'K' then
            return Natural (Val) * 1024;
         elsif Str (Pos) = 'm' or Str (Pos) = 'M' then
            return Natural (Val) * 1024 * 1024;
         end if;
      end if;
      Error_C ("bad memory unit for option ");
      Error_E (Option_Name);
   end Extract_Size;

   function To_Lower (C : Character) return Character is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Character'Pos (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

   procedure Decode (Stop : out Boolean)
   is
      Arg : Ghdl_C_String;
      Len : Natural;
   begin
      Stop := False;
      Last_Opt := Argc - 1;
      for I in 1 .. Argc - 1 loop
         Arg := Argv (I);
         Len := strlen (Arg);
         declare
            Argument : constant String := Arg (1 .. Len);
         begin
            if Argument = "--" then
               Last_Opt := I;
               exit;
            elsif Argument = "--help" or else Argument = "-h" then
               Help;
               Stop := True;
            elsif Argument = "--disp-time" then
               Disp_Time := True;
            elsif Argument = "--trace-signals" then
               Trace_Signals := True;
               Disp_Time := True;
            elsif Argument = "--trace-processes" then
               Trace_Processes := True;
               Disp_Time := True;
            elsif Argument = "--disp-order" then
               Disp_Signals_Order := True;
            elsif Argument = "--checks" then
               Checks := True;
            elsif Argument = "--disp-sources" then
               Disp_Sources := True;
            elsif Argument = "--disp-sig-types" then
               Disp_Sig_Types := True;
            elsif Argument = "--disp-signals-map" then
               Disp_Signals_Map := True;
            elsif Argument = "--disp-signals-table" then
               Disp_Signals_Table := True;
            elsif Argument = "--disp-sensitivity" then
               Disp_Sensitivity := True;
            elsif Argument = "--stats" then
               Flag_Stats := True;
            elsif Argument = "--no-run" then
               Flag_No_Run := True;
            elsif Len > 18 and then Argument (1 .. 18) = "--time-resolution="
            then
               declare
                  Res : Character;
                  Unit : String (1 .. 3);
               begin
                  Res := '?';
                  if Len >= 20 then
                     Unit (1) := To_Lower (Argument (19));
                     Unit (2) := To_Lower (Argument (20));
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
                        Unit (3) := To_Lower (Argument (21));
                        if Unit = "min" then
                           Res := 'M';
                        elsif Unit = "sec" then
                           Res := 's';
                        end if;
                     end if;
                  end if;
                  if Res = '?' then
                     Error_C ("bad unit for '");
                     Error_C (Argument);
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
            elsif Len > 12 and then Argument (1 .. 12) = "--stop-time=" then
               declare
                  Ok : Boolean;
                  Pos : Natural;
                  Time : Integer_64;
                  Unit : String (1 .. 3);
               begin
                  Extract_Integer (Argument (13 .. Len), Ok, Time, Pos);
                  if not Ok then
                     Time := 1;
                  end if;
                  if (Len - Pos + 1) not in 2 .. 3 then
                     Error_C ("bad unit for '");
                     Error_C (Argument);
                     Error_E ("'");
                     return;
                  end if;
                  Unit (1) := To_Lower (Argument (Pos));
                  Unit (2) := To_Lower (Argument (Pos + 1));
                  if Len = Pos + 2 then
                     Unit (3) := To_Lower (Argument (Pos + 2));
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
                     Error_C (Argument);
                     Error_E ("'");
                  end if;
                  Stop_Time := Std_Time (Time);
               end;
            elsif Len > 13 and then Argument (1 .. 13) = "--stop-delta=" then
               declare
                  Ok : Boolean;
                  Pos : Natural;
                  Time : Integer_64;
               begin
                  Extract_Integer (Argument (14 .. Len), Ok, Time, Pos);
                  if not Ok or else Pos <= Len then
                     Error_C ("bad value in '");
                     Error_C (Argument);
                     Error_E ("'");
                  else
                     if Time > Integer_64 (Integer'Last) then
                        Stop_Delta := Integer'Last;
                     else
                        Stop_Delta := Integer (Time);
                     end if;
                  end if;
               end;
            elsif Len > 15 and then Argument (1 .. 15) = "--assert-level=" then
               if Argument (16 .. Len) = "note" then
                  Severity_Level := Note_Severity;
               elsif Argument (16 .. Len) = "warning" then
                  Severity_Level := Warning_Severity;
               elsif Argument (16 .. Len) = "error" then
                  Severity_Level := Error_Severity;
               elsif Argument (16 .. Len) = "failure" then
                  Severity_Level := Failure_Severity;
               elsif Argument (16 .. Len) = "none" then
                  Severity_Level := 4;
               else
                  Error ("bad argument for --assert-level option, try --help");
               end if;
            elsif Len > 15 and then Argument (1 .. 15) = "--ieee-asserts=" then
               if Argument (16 .. Len) = "disable" then
                  Ieee_Asserts := Disable_Asserts;
               elsif Argument (16 .. Len) = "enable" then
                  Ieee_Asserts := Enable_Asserts;
               elsif Argument (16 .. Len) = "disable-at-0" then
                  Ieee_Asserts := Disable_Asserts_At_Time_0;
               else
                  Error ("bad argument for --ieee-asserts option, try --help");
               end if;
            elsif Argument = "--expect-failure" then
               Expect_Failure := True;
            elsif Len >= 13 and then Argument (1 .. 13) = "--stack-size=" then
               Stack_Size := Extract_Size
                 (Argument (14 .. Len), "--stack-size");
               if Stack_Size > Stack_Max_Size then
                  Stack_Max_Size := Stack_Size;
               end if;
            elsif Len >= 17 and then Argument (1 .. 17) = "--stack-max-size="
            then
               Stack_Max_Size := Extract_Size
                 (Argument (18 .. Len), "--stack-size");
               if Stack_Size > Stack_Max_Size then
                  Stack_Size := Stack_Max_Size;
               end if;
            elsif Len >= 11 and then Argument (1 .. 11) = "--activity="
            then
               if Argument (12 .. Len) = "none" then
                  Flag_Activity := Activity_None;
               elsif Argument (12 .. Len) = "min" then
                  Flag_Activity := Activity_Minimal;
               elsif Argument (12 .. Len) = "all" then
                  Flag_Activity := Activity_All;
               else
                  Error ("bad argument for --activity, try --help");
               end if;
            elsif Len > 10 and then Argument (1 .. 10) = "--threads=" then
               declare
                  Ok : Boolean;
                  Pos : Natural;
                  Val : Integer_64;
               begin
                  Extract_Integer (Argument (11 .. Len), Ok, Val, Pos);
                  if not Ok or else Pos <= Len then
                     Error_C ("bad value in '");
                     Error_C (Argument);
                     Error_E ("'");
                  else
                     Nbr_Threads := Integer (Val);
                  end if;
               end;
            elsif not Grt.Hooks.Call_Option_Hooks (Argument) then
               Error_C ("unknown option '");
               Error_C (Argument);
               Error_E ("', try --help");
            end if;
         end;
      end loop;
   end Decode;
end Grt.Options;
