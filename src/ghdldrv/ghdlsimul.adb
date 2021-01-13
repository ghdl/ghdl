--  GHDL driver - simulator commands.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

with Ada.Command_Line;

with Ghdllocal;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Simple_IO;
with Types;
with Flags;
with Vhdl.Std_Package;
with Vhdl.Canon;
with Vhdl.Configuration;
with Vhdl.Annotations;
with Simul.Elaboration;
with Simul.Simulation.Main;
with Simul.Debugger;
with Simul.Execution;

with Ghdlcomp; use Ghdlcomp;

with Grt.Types;
with Grt.Options;
with Grt.Errors;
with Grt.Stdio;
with Grtlink;

package body Ghdlsimul is

   --  FIXME: reuse simulation.top_config
   Top_Conf : Iir;

   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      Common_Compile_Init (Analyze_Only);
      if Analyze_Only then
         return;
      end if;

      Vhdl.Canon.Canon_Flag_Add_Labels := True;
      Vhdl.Canon.Canon_Flag_Sequentials_Stmts := True;
      Vhdl.Canon.Canon_Flag_Expressions := True;
      Vhdl.Canon.Canon_Flag_All_Sensitivity := True;
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
      use Vhdl.Configuration;
   begin
      Common_Compile_Elab (Cmd_Name, Args, Opt_Arg, Top_Conf);

      --  Annotate all units.
      Vhdl.Annotations.Annotate (Vhdl.Std_Package.Std_Standard_Unit);
      for I in Design_Units.First .. Design_Units.Last loop
         Vhdl.Annotations.Annotate (Design_Units.Table (I));
      end loop;
   end Compile_Elab;

   --  Set options.
   procedure Set_Run_Options (Args : Argument_List)
   is
      use Grt.Options;
      use Types;
      Arg : String_Access;
      Status : Decode_Option_Status;
      Argv0 : String_Acc;
   begin
      --  Set progname (used for grt error messages)
      Argv0 := new String'(Ada.Command_Line.Command_Name & ASCII.Nul);
      Grt.Options.Progname := Grt.Types.To_Ghdl_C_String (Argv0.all'Address);
      Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);

      Grtlink.Flag_String := Flags.Flag_String;
      Grt.Options.Set_Time_Resolution;

      for I in Args'Range loop
         Arg := Args (I);
         if Arg.all = "--disp-tree" then
            Simul.Simulation.Disp_Tree := True;
         elsif Arg.all = "--expect-failure" then
            Decode_Option (Arg.all, Status);
            pragma Assert (Status = Decode_Option_Ok);
         elsif Arg.all = "--trace-elab" then
            Simul.Elaboration.Trace_Elaboration := True;
         elsif Arg.all = "--trace-drivers" then
            Simul.Elaboration.Trace_Drivers := True;
         elsif Arg.all = "--trace-simu" then
            Simul.Simulation.Trace_Simulation := True;
         elsif Arg.all = "--trace-stmt" then
            Simul.Execution.Trace_Statements := True;
         elsif Arg.all = "--stats" then
            Simul.Simulation.Disp_Stats := True;
         elsif Arg.all = "-i" then
            Simul.Debugger.Flag_Debugger := True;
            Simul.Debugger.Flag_Interractive := True;
         else
            Decode_Option (Arg.all, Status);
            case Status is
               when Decode_Option_Last =>
                  exit;
               when Decode_Option_Stop =>
                  Grt.Options.Flag_No_Run := True;
               when Decode_Option_Ok =>
                  null;
            end case;
            --  Ghdlmain.Error ("unknown run options '" & Arg.all & "'");
            --  raise Option_Error;
         end if;
      end loop;
   end Set_Run_Options;

   procedure Run
   is
      use Ada.Command_Line;
   begin
      if Grt.Options.Flag_No_Run then
         --  Some options like --has-feature set the exit status.
         Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
         return;
      end if;

      Simul.Simulation.Main.Simulation_Entity (Top_Conf);

      Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
   begin
      if Option = "--debug" or Option = "-g" then
         Simul.Debugger.Flag_Debugger := True;
      else
         return False;
      end if;
      return True;
   end Decode_Option;

   procedure Disp_Long_Help is
   begin
      Simple_IO.Put_Line (" --debug        Run with debugger");
   end Disp_Long_Help;

   function Get_Top_Config return Iir is
   begin
      return Top_Conf;
   end Get_Top_Config;

   procedure Set_Hooks is
   begin
      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Decode_Option'Access,
                         Disp_Long_Help'Access);
   end Set_Hooks;

   procedure Register_Commands is
   begin
      Set_Hooks;
      Ghdlcomp.Register_Commands;
   end Register_Commands;

   procedure Compile_Init is
   begin
      Ghdllocal.Compile_Init;
      Set_Hooks;
   end Compile_Init;
end Ghdlsimul;
