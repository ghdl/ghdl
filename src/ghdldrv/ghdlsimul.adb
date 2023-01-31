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
with System;

with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Interfaces;
with Interfaces.C;

with Ghdllocal; use Ghdllocal;

with Flags;
with Errorout;
with Simple_IO;
with Options;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Std_Package;
with Vhdl.Sem;
with Vhdl.Canon;
with Vhdl.Configuration;
with Vhdl.Utils;
with Vhdl.Back_End;

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
with Synth.Errors;
with Synth.Vhdl_Foreign;

with Simul.Vhdl_Elab;
with Simul.Vhdl_Simul;
with Simul.Main;

package body Ghdlsimul is
   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      Common_Compile_Init (Analyze_Only);

      Vhdl.Back_End.Sem_Foreign := Vhdl.Back_End.Sem_Foreign_Wrapper'Access;

      if Analyze_Only then
         return;
      end if;

      --  The design is always analyzed in whole.
      Flags.Flag_Whole_Analyze := True;
      Vhdl.Canon.Canon_Flag_Add_Labels := True;
      Vhdl.Canon.Canon_Flag_Add_Suspend_State := True;

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
      Top : Node;
   begin
      Common_Compile_Elab (Cmd_Name, Args, True, Opt_Arg, Config);

      --  For compatibility, also handle '-gGEN=VAL' options after the
      --  top-level unit.
      --  Handle --expect-failure
      for I in Opt_Arg .. Args'Last loop
         declare
            Arg : String renames Args (I).all;
            Res : Options.Option_State;
            pragma Unreferenced (Res);
         begin
            if Arg'Length > 3
              and then Arg (Arg'First + 1) = 'g'
              and then Is_Generic_Override_Option (Arg)
            then
               Res := Decode_Generic_Override_Option (Arg);
            elsif Arg = "--expect-failure" then
               Flag_Expect_Failure := True;
            end if;
         end;
      end loop;

      --  If all design units are loaded, late semantic checks can be
      --  performed.
      declare
         use Vhdl.Configuration;
         Unit : Node;
      begin
         if Flag_Load_All_Design_Units then
            for I in Design_Units.First .. Design_Units.Last loop
               Unit := Design_Units.Table (I);
               Vhdl.Sem.Sem_Analysis_Checks_List (Unit, False);
               --  There cannot be remaining checks to do.
               pragma Assert
                 (Get_Analysis_Checks_List (Unit) = Null_Iir_List);
            end loop;
         end if;
      end;

      --  Handle automatic time resolution.
      --  Must be done before elaboration, as time value could be computed.
      if Time_Resolution = 'a' then
         Time_Resolution := Vhdl.Std_Package.Get_Minimal_Time_Resolution;
         if Time_Resolution = '?' then
            Time_Resolution := 'f';
         end if;
      end if;
      Vhdl.Std_Package.Set_Time_Resolution (Time_Resolution);

      --  Set flags.
      Synth.Flags.Flag_Simulation := True;
      Synth.Errors.Debug_Handler := Elab.Debugger.Debug_Error'Access;
      Synth.Vhdl_Foreign.Initialize;

      --  Generic overriding.
      Top := Vhdl.Utils.Get_Entity_From_Configuration (Config);
      Vhdl.Configuration.Apply_Generic_Override (Top);
      Vhdl.Configuration.Check_Entity_Declaration_Top (Top, False);

      if Errorout.Nbr_Errors > 0 then
         raise Errorout.Compilation_Error;
      end if;

      --  *THE* elaboration.
      --  Compute values, instantiate..
      Lib_Unit := Get_Library_Unit (Config);
      pragma Assert (Get_Kind (Lib_Unit) /= Iir_Kind_Foreign_Module);
      Inst := Elab.Vhdl_Insts.Elab_Top_Unit (Lib_Unit);

      if Errorout.Nbr_Errors > 0 then
         raise Errorout.Compilation_Error;
      end if;

      --  Finish elaboration: gather processes, signals.
      --  Compute drivers, sources...
      Simul.Vhdl_Elab.Gather_Processes (Inst);
      Simul.Vhdl_Elab.Elab_Processes;
      Simul.Vhdl_Elab.Compute_Sources;

      if Errorout.Nbr_Errors > 0 then
         raise Errorout.Compilation_Error;
      end if;

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

   procedure Run
   is
      use Ada.Command_Line;
   begin
      if Grt.Options.Flag_No_Run then
         --  Some options like --has-feature set the exit status.
         Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
         return;
      end if;

      --  Overwrite time resolution in flag string.
      Flags.Flag_String (5) := Time_Resolution;
      Grtlink.Flag_String := Flags.Flag_String;

      Synth.Flags.Severity_Level := Grt.Options.Severity_Level;

      Simul.Vhdl_Simul.Simulation;

      Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
      Res : Options.Option_State;
      pragma Unreferenced (Res);
   begin
      if Option = "--debug" or Option = "-g" then
         Elab.Debugger.Flag_Debug_Enable := True;
      elsif Option = "-t" then
         Synth.Flags.Flag_Trace_Statements := True;
      elsif Option = "-i" then
         Simul.Main.Flag_Interractive := True;
      elsif Option = "-ge" then
         Simul.Main.Flag_Debug_Elab := True;
      elsif Option'Last > 3
        and then Option (Option'First + 1) = 'g'
        and then Is_Generic_Override_Option (Option)
      then
         Res := Decode_Generic_Override_Option (Option);
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
