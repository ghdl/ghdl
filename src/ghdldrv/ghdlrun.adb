--  GHDL driver - JIT commands.
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

with Interfaces;
with Interfaces.C;

with Types; use Types;
with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with Simple_IO; use Simple_IO;

with Flags;
with Options;
with Errorout; use Errorout;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Std_Package;
with Vhdl.Sem;
with Vhdl.Canon;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Back_End;
with Vhdl.Nodes_GC;
with Vhdl.Utils;
with Vhdl.Configuration;

with Elab.Debugger;
with Elab.Vhdl_Context;
with Elab.Vhdl_Insts;

with Ortho_Jit;
with Ortho_Nodes; use Ortho_Nodes;
with Trans_Decls;
with Translation;
with Trans_Link;
with Trans_Foreign;

with Simul.Main;
with Simul.Vhdl_Elab;
with Simul.Vhdl_Simul;
with Simul.Vhdl_Compile;
with Simul.Fst;

with Synth.Flags;
with Elab.Vhdl_Errors;
with Synth.Vhdl_Foreign;

with Grt.Main;
with Grt.Options;
with Grt.Types;
with Grt.Errors;
with Grt.Backtraces.Jit;
with Grt.Heap;

with Grt.Vcd;
with Grt.Vcdz;
with Grt.Vpi;
with Grt.Vhpi;
with Grt.Waves;
with Grt.Vital_Annotate;
with Grt.Disp_Tree;
with Grt.Disp_Rti;
with Grt.Psl;
with Grt.Backtraces;

with Ghdlcomp; use Ghdlcomp;
with Grtlink;

package body Ghdlrun is
   type Run_Mode_Kind is
     (
      --  Fully interpreted.
      Run_Interp,

      --  Elaboration is interpreted, simulation is compiled.
      Run_Jit,

      --  Most of the elaboration and the whole simulation is compiled.
      Run_Elab_Jit
     );

   Run_Mode : Run_Mode_Kind := Run_Jit;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Vhdl.Back_End.Foreign_Info_Type;
                           Ortho : O_Dnode);

   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      Common_Compile_Init (Analyze_Only);
      if Analyze_Only then
         return;
      end if;

      Translation.Foreign_Hook := Foreign_Hook'Access;
      Trans_Foreign.Init;

      --  FIXME: add a flag to force unnesting.
      --  Translation.Flag_Unnest_Subprograms := True;

      --  The design is always analyzed in whole.
      Flags.Flag_Whole_Analyze := True;
      Vhdl.Canon.Canon_Flag_Add_Labels := True;

      case Run_Mode is
         when Run_Interp =>
            Vhdl.Canon.Canon_Flag_Add_Suspend_State := True;
            Vhdl.Canon.Canon_Flag_Concurrent_Stmts := False;
         when others =>
            null;
      end case;
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : String_Acc_Array; Opt_Arg : out Natural)
   is
      Config : Iir;
   begin
      Common_Compile_Elab (Cmd_Name, Args, True, Opt_Arg, Config);

      if Run_Mode /= Run_Elab_Jit then
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
      end if;

      if Time_Resolution = 'a' then
         Time_Resolution := Vhdl.Std_Package.Get_Minimal_Time_Resolution;
         if Time_Resolution = '?' then
            Time_Resolution := 'f';
         end if;
         if Flag_Verbose then
            Put ("Time resolution is 1 ");
            case Time_Resolution is
               when 'f' => Put ("fs");
               when 'p' => Put ("ps");
               when 'n' => Put ("ns");
               when 'u' => Put ("us");
               when 'm' => Put ("ms");
               when 's' => Put ("sec");
               when others => Put ("??");
            end case;
            New_Line;
         end if;
      end if;
      Vhdl.Std_Package.Set_Time_Resolution (Time_Resolution);

      --  Overwrite time resolution in flag string.
      Flags.Flag_String (5) := Time_Resolution;
      Grtlink.Flag_String := Flags.Flag_String;

      case Run_Mode is
         when Run_Elab_Jit =>
            Ortho_Jit.Init;

            Translation.Initialize;

            Translation.Elaborate (Config, True);
         when Run_Interp
           | Run_Jit =>
            --  Set flags.
            Synth.Flags.Flag_Simulation := True;
            Elab.Vhdl_Errors.Debug_Handler := Elab.Debugger.Debug_Error'Access;
            Synth.Vhdl_Foreign.Initialize;

            --  As all design units are loaded, late semantic checks can be
            --  performed.
            declare
               use Vhdl.Configuration;
               Unit : Node;
            begin
               for I in Design_Units.First .. Design_Units.Last loop
                  Unit := Design_Units.Table (I);
                  Vhdl.Sem.Sem_Analysis_Checks_List (Unit, False);
                  --  There cannot be remaining checks to do.
                  pragma Assert
                    (Get_Analysis_Checks_List (Unit) = Null_Iir_List);
               end loop;
            end;

            declare
               use Elab.Vhdl_Context;
               Lib_Unit : Node;
               Inst : Synth_Instance_Acc;
               Top : Node;
            begin
               --  Generic overriding.
               Top := Vhdl.Utils.Get_Entity_From_Configuration (Config);
               Vhdl.Configuration.Apply_Generic_Override (Top);
               Vhdl.Configuration.Check_Entity_Declaration_Top (Top, False);

               if Errorout.Nbr_Errors > 0 then
                  raise Errorout.Compilation_Error;
               end if;

               if Run_Mode = Run_Jit then
                  Elab.Vhdl_Insts.Flag_Macro_Expand_Instance := True;
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
            end;
      end case;

      if Errorout.Nbr_Errors > 0 then
         --  This may happen (bad entity for example).
         raise Compilation_Error;
      end if;

      if Flags.Check_Ast_Level > 0 then
         Vhdl.Nodes_GC.Report_Unreferenced;
      end if;
   end Compile_Elab;

   --  Set options.
   --  This is a little bit over-kill: from C to Ada and then again to C...
   procedure Set_Run_Options (Args : String_Acc_Array)
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
   Elaborate_Proc : Elaborate_Acc := null;

   procedure Ghdl_Elaborate is
   begin
      Elaborate_Proc.all;
   end Ghdl_Elaborate;

   procedure Def (Decl : O_Dnode; Addr : Address)
     renames Ortho_Jit.Set_Address;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Vhdl.Back_End.Foreign_Info_Type;
                           Ortho : O_Dnode)
   is
      Res : Address;
   begin
      Res := Trans_Foreign.Get_Foreign_Address (Decl, Info);
      if Res /= Null_Address then
         Def (Ortho, Res);
      end if;
   end Foreign_Hook;

   procedure Register_Modules is
   begin
      --  List of modules to be registered.
      Grt.Disp_Tree.Register;
      Grt.Vcd.Register;
      Grt.Vcdz.Register;
      Grt.Waves.Register;
      Simul.Fst.Register;
      Grt.Vpi.Register;
      Grt.Vhpi.Register;
      Grt.Vital_Annotate.Register;
      Grt.Disp_Rti.Register;
      Grt.Psl.Register;
      Grt.Backtraces.Register;
   end Register_Modules;

   procedure Run
   is
      use Ada.Command_Line;
      function Conv is new Ada.Unchecked_Conversion
        (Source => Address, Target => Elaborate_Acc);
      Err : Boolean;
      Decl : O_Dnode;
   begin
      case Run_Mode is
         when Run_Elab_Jit =>
            if Flag_Verbose then
               Put_Line ("Linking in memory");
            end if;

            Trans_Link.Link;

            Def (Trans_Decls.Ghdl_Allocate,
                 Grt.Heap.Ghdl_Allocate'Address);
            Def (Trans_Decls.Ghdl_Deallocate,
                 Grt.Heap.Ghdl_Deallocate'Address);

            Ortho_Jit.Link (Err);
            if Err then
               raise Compile_Error;
            end if;

            Grtlink.Std_Standard_Boolean_RTI_Ptr :=
              Ortho_Jit.Get_Address (Trans_Decls.Std_Standard_Boolean_Rti);
            Grtlink.Std_Standard_Bit_RTI_Ptr :=
              Ortho_Jit.Get_Address (Trans_Decls.Std_Standard_Bit_Rti);
            if Vhdl.Ieee.Std_Logic_1164.Resolved /= Null_Iir then
               Decl := Translation.Get_Resolv_Ortho_Decl
                 (Vhdl.Ieee.Std_Logic_1164.Resolved);
               if Decl /= O_Dnode_Null then
                  Grtlink.Ieee_Std_Logic_1164_Resolved_Resolv_Ptr :=
                    Ortho_Jit.Get_Address (Decl);
               end if;
            end if;

            Grt.Backtraces.Jit.Symbolizer_Proc := Ortho_Jit.Symbolize'Access;

            Elaborate_Proc :=
              Conv (Ortho_Jit.Get_Address (Trans_Decls.Ghdl_Elaborate));

            Ortho_Jit.Finish;

            Translation.Finalize;
            Options.Finalize;

            if Flag_Verbose then
               Put_Line ("Starting simulation");
            end if;

            Register_Modules;

            Grt.Main.Run;

         when Run_Interp
           | Run_Jit =>
            if Grt.Options.Flag_No_Run then
               --  Some options like --has-feature set the exit status.
               Set_Exit_Status (Exit_Status (Grt.Errors.Exit_Status));
               return;
            end if;

            Synth.Flags.Severity_Level := Grt.Options.Severity_Level;

            if Run_Mode = Run_Jit then
               Elaborate_Proc := Simul.Vhdl_Compile.Elaborate'Access;

               Register_Modules;

               Simul.Vhdl_Compile.Simulation;
            else
               Elaborate_Proc := Simul.Vhdl_Simul.Runtime_Elaborate'Access;
               Simul.Vhdl_Simul.Simulation;
            end if;
      end case;

      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Grt.Errors.Exit_Status));
   end Run;

   --  Command run help.
   type Command_Run_Help is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Run_Help; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Run_Help) return String;
   procedure Perform_Action (Cmd : in out Command_Run_Help;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Run_Help; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "run-help"
        or else Name = "--run-help";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Run_Help) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "run-help"
        & ASCII.LF & "  Display help for RUNOPTS options"
        & ASCII.LF & "  alias: --run-help";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Run_Help;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error ("warning: command 'run-help' does not accept any argument");
         Success := False;
      end if;
      Put_Line ("These options can only be placed at [RUNOPTS]");
      --  Register modules, since they add commands.
      Register_Modules;
      --  Bypass usual help header.
      Grt.Options.Argc := 0;
      Grt.Options.Help;
      Success := True;
   end Perform_Action;

   function Decode_Option (Option : String) return Boolean
   is
      pragma Assert (Option'First = 1);
      Res : Options.Option_State;
      pragma Unreferenced (Res);
   begin
      if Option = "--debug" or Option = "-g" then
         Elab.Debugger.Flag_Debug_Enable := True;
      elsif Option = "-t" then
         Synth.Flags.Flag_Trace_Statements := True;
      elsif Option = "-ta" then
         Simul.Vhdl_Simul.Trace_Solver := True;
      elsif Option = "-tr" then
         Simul.Vhdl_Simul.Trace_Residues := True;
      elsif Option = "-i" then
         Simul.Main.Flag_Interractive := True;
      elsif Option = "-ge" then
         Simul.Main.Flag_Debug_Elab := True;
      elsif Option = "--jit" then
         Run_Mode := Run_Jit;
      elsif Option = "--elab-jit" then
         Run_Mode := Run_Elab_Jit;
      elsif Option = "--interp" then
         Run_Mode := Run_Interp;
      elsif Option'Last > 3
        and then Option (Option'First + 1) = 'g'
        and then Is_Generic_Override_Option (Option)
      then
         Res := Decode_Generic_Override_Option (Option);
      elsif Run_Mode /= Run_Interp
        and then Ortho_Jit.Decode_Option (Option)
      then
         null;
      else
         return False;
      end if;
      return True;
   end Decode_Option;

   procedure Disp_Help is
   begin
      Ortho_Jit.Disp_Help;
      Simple_IO.Put_Line (" --debug        Run with debugger");
   end Disp_Help;

   procedure Register_Commands is
   begin
      case Run_Mode is
         when Run_Interp =>
            Ghdlmain.Version_String :=
              new String'("static elaboration, interpretation");
         when Run_Jit =>
            Ghdlmain.Version_String :=
              new String'("static elaboration, "
                            & Ortho_Jit.Get_Jit_Name & " code generator");
         when Run_Elab_Jit =>
            Ghdlmain.Version_String :=
              new String'(Ortho_Jit.Get_Jit_Name & " code generator");
      end case;

      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Decode_Option'Access,
                         Disp_Help'Access);
      Ghdlcomp.Register_Commands;
      Translation.Register_Translation_Back_End;
      Register_Command (new Command_Run_Help);
   end Register_Commands;
end Ghdlrun;
