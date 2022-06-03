--  GHDL driver - Simulation commands.
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
with System; use System;

with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Interfaces;
with Interfaces.C;

with Ghdllocal; use Ghdllocal;
with Simple_IO; use Simple_IO;

with Flags;
with Errorout;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Std_Package;
with Vhdl.Canon;
with Vhdl.Annotations;

with Grt.Options;
with Grt.Types;
with Grt.Errors;

with Ghdlcomp; use Ghdlcomp;
with Grtlink;

--  For Elaborate.
with Elab.Vhdl_Context;
with Elab.Vhdl_Debug;
with Elab.Vhdl_Insts;
with Elab.Debugger;

with Synth.Flags;
with Simul.Vhdl_Elab;
with Simul.Vhdl_Simul;

package body Ghdlsimul is
   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      Common_Compile_Init (Analyze_Only);
      if Analyze_Only then
         return;
      end if;

      --  FIXME: add a flag to force unnesting.
      --  Translation.Flag_Unnest_Subprograms := True;

      --  The design is always analyzed in whole.
      Flags.Flag_Whole_Analyze := True;
      Vhdl.Canon.Canon_Flag_Add_Labels := True;
      Vhdl.Canon.Canon_Flag_Add_Suspend_State := True;

      Vhdl.Annotations.Flag_Synthesis := True;

      --  Do not canon concurrent statements.
      Vhdl.Canon.Canon_Flag_Concurrent_Stmts := False;
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
      use Elab.Vhdl_Context;
      Config : Node;
      Lib_Unit : Node;
      Inst : Synth_Instance_Acc;
   begin
      Common_Compile_Elab (Cmd_Name, Args, False, Opt_Arg, Config);

      Lib_Unit := Get_Library_Unit (Config);
      pragma Assert (Get_Kind (Lib_Unit) /= Iir_Kind_Foreign_Module);
      Inst := Elab.Vhdl_Insts.Elab_Top_Unit (Lib_Unit);

      if Errorout.Nbr_Errors > 0 then
         raise Errorout.Compilation_Error;
      end if;

--      if Inst = null then
--         if Cmd.Expect_Failure then
--            return;
--         else
--            raise Errorout.Compilation_Error;
--         end if;
--      end if;

      Simul.Vhdl_Elab.Gather_Processes (Inst);
      Simul.Vhdl_Elab.Elab_Processes;

      if False then
         Elab.Vhdl_Debug.Disp_Hierarchy (Inst, False, True);
      end if;
   end Compile_Elab;

   --  Set options.
   --  This is a little bit over-kill: from C to Ada and then again to C...
   procedure Set_Run_Options (Args : Argument_List)
   is
      use Interfaces.C;
      use Grt.Options;
      use Grt.Types;

      function Malloc (Size : size_t) return Argv_Type;
      pragma Import (C, Malloc);

      function Strdup (Str : String) return Ghdl_C_String;
      pragma Import (C, Strdup);
--        is
--           T : Grt.Types.String_Access;
--        begin
--           T := new String'(Str & Ghdllocal.Nul);
--           return To_Ghdl_C_String (T.all'Address);
--        end Strdup;
   begin
      Argc := 1 + Args'Length;
      Argv := Malloc
        (size_t (Argc * (Ghdl_C_String'Size / System.Storage_Unit)));
      Argv (0) := Strdup (Ada.Command_Line.Command_Name & Ghdllocal.Nul);
      Progname := Argv (0);
      for I in Args'Range loop
         Argv (1 + I - Args'First) := Strdup (Args (I).all & Ghdllocal.Nul);
      end loop;
   end Set_Run_Options;

   procedure Ghdl_Elaborate;
   pragma Export (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   type Elaborate_Acc is access procedure;
   pragma Convention (C, Elaborate_Acc);
   Elaborate_Proc : Elaborate_Acc := null;

   procedure Ghdl_Elaborate is
   begin
      Elaborate_Proc.all;
   end Ghdl_Elaborate;

   procedure Run
   is
      use Ada.Command_Line;
   begin
      if Grt.Options.Flag_No_Run then
         --  Some options like --has-feature set the exit status.
         Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
         return;
      end if;

      if Time_Resolution = 'a' then
         Time_Resolution := Vhdl.Std_Package.Get_Minimal_Time_Resolution;
         if Time_Resolution = '?' then
            Time_Resolution := 'f';
         end if;
      end if;
      Vhdl.Std_Package.Set_Time_Resolution (Time_Resolution);

      --  Overwrite time resolution in flag string.
      Flags.Flag_String (5) := Time_Resolution;
      Grtlink.Flag_String := Flags.Flag_String;

      Synth.Flags.Severity_Level := Grt.Options.Severity_Level;

      Elaborate_Proc := Simul.Vhdl_Simul.Runtime_Elaborate'Access;
      Simul.Vhdl_Simul.Simulation;

      --  Simul uses report_msg.
      if Errorout.Nbr_Errors > 0 then
         Grt.Errors.Exit_Status := 1;
      end if;

      Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
   begin
      if Option = "--debug" or Option = "-g" then
         Elab.Debugger.Flag_Debug_Enable := True;
      elsif Option = "-t" then
         Synth.Flags.Flag_Trace_Statements := True;
      elsif Option = "-i" then
         Simul.Vhdl_Simul.Flag_Interractive := True;
      else
         return False;
      end if;
      return True;
   end Decode_Option;

   procedure Disp_Help is
   begin
      Simple_IO.Put_Line (" --debug        Run with debugger");
   end Disp_Help;

   procedure Register_Commands
   is
   begin
      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Decode_Option'Access,
                         Disp_Help'Access);
      Ghdlcomp.Register_Commands;
   end Register_Commands;
end Ghdlsimul;
