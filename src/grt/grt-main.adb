--  GHDL Run Time (GRT) -  entry point.
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
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Stdio;
with Grt.Errors; use Grt.Errors;
with Grt.Processes;
with Grt.Signals;
with Grt.Options; use Grt.Options;
with Grt.Stats;
with Grt.Hooks;
with Grt.Modules;
with Grt.Change_Generics;

--  The following packages are not referenced in this package.
--  These are subprograms called only from GHDL generated code.
--  They are with'ed in order to be present in the binary.
pragma Warnings (Off);
with Grt.Files;
with Grt.Types;
with Grt.Lib;
with Grt.Shadow_Ieee;
with Grt.Images;
with Grt.Values;
with Grt.Names;
pragma Warnings (On);

package body Grt.Main is
   procedure Ghdl_Elaborate;
   pragma Import (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   --  Wrapper around elaboration just to return 0.
   function Ghdl_Elaborate_Wrapper return Integer is
   begin
      Ghdl_Elaborate;
      return 0;
   end Ghdl_Elaborate_Wrapper;

   procedure Ghdl_Init_Top_Generics is
   begin
      Grt.Change_Generics.Change_All_Generics;
      Grt.Change_Generics.Check_Required_Generic_Override;
   end Ghdl_Init_Top_Generics;

   procedure Disp_Stats_Hook (Code : Integer);
   pragma Convention (C, Disp_Stats_Hook);

   procedure Disp_Stats_Hook (Code : Integer)
   is
      pragma Unreferenced (Code);
   begin
      Stats.End_Simulation;
      Stats.Disp_Stats;
   end Disp_Stats_Hook;

   procedure Check_Flag_String
   is
      Err : Boolean;
   begin
      --  The conditions may be statically known.
      pragma Warnings (Off);

      Err := False;
      if (Std_Integer'Size = 32 and Flag_String (3) /= 'i')
        or else (Std_Integer'Size = 64 and Flag_String (3) /= 'I')
      then
         Err := True;
      end if;
      if (Std_Time'Size = 32 and Flag_String (4) /= 't')
        or else (Std_Time'Size = 64 and Flag_String (4) /= 'T')
      then
         Err := True;
      end if;

      pragma Warnings (On);

      if Err then
         Grt.Errors.Error
           ("GRT is not consistent with the flags used for your design");
      end if;
   end Check_Flag_String;

   Default_Progname : constant String := "ghdl" & NUL;

   --  Initialization: decode options, but no elaboration.
   --  Return False in case of error.
   procedure Run_Options (Progname : Ghdl_C_String;
                          Argc : Integer;
                          Argv : Grt.Options.Argv_Type) is
   begin
      if Progname = null then
         Grt.Options.Progname := To_Ghdl_C_String (Default_Progname'Address);
      else
         Grt.Options.Progname := Progname;
      end if;
      Grt.Options.Argc := Argc;
      Grt.Options.Argv := Argv;
   end Run_Options;

   function Run_Elab return C_Boolean
   is
      Stop : Boolean;
   begin
      --  Set stream for error messages
      Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);

      --  Register modules.
      --  They may insert hooks.
      Grt.Modules.Register_Modules;

      --  Decode options.
      Grt.Options.Decode (Stop);

      --  Early stop (for options such as --help).
      if Stop then
         return False;
      end if;

      --  Check coherency between GRT and GHDL generated code.
      Check_Flag_String;

      --  Internal initializations.
      Grt.Hooks.Call_Init_Hooks;

      Grt.Processes.Init;

      Grt.Signals.Init;

      if Flag_Stats then
         Stats.Start_Elaboration;
      end if;

      --  Elaboration.  Run through longjump to catch errors.
      if Run_Through_Longjump (Ghdl_Elaborate_Wrapper'Access) < 0 then
         Grt.Errors.Error ("error during elaboration");
         return False;
      end if;

      --  Can continue.
      return True;
   end Run_Elab;

   function Run_Simul return Integer is
   begin
      if Flag_No_Run then
         return 0;
      end if;

      return Run_Through_Longjump (Grt.Processes.Simulation'Access);
   end Run_Simul;

   procedure Run_Finish (Status : Integer) is
   begin
      Grt.Processes.Simulation_Finish;

      Grt.Hooks.Call_Finish_Hooks;

      if Flag_Stats then
         Disp_Stats_Hook (0);
      end if;

      if Expect_Failure then
         if Status >= 0 then
            Expect_Failure := False;
            Error_NF ("error expected, but none occurred");
         end if;
      else
         if Status < 0 then
            Error_NF ("simulation failed");
         end if;
      end if;
   end Run_Finish;

   procedure Run
   is
      Ok : C_Boolean;
      Status : Integer;
   begin
      Ok := Run_Elab;
      if not Ok then
         return;
      end if;

      Status := Run_Simul;

      Run_Finish (Status);
   end Run;

end Grt.Main;
