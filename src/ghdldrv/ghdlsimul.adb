--  GHDL driver - simulator commands.
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

with Ada.Text_IO;
with Ada.Command_Line;

with Ghdllocal; use Ghdllocal;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Types;
with Flags;
with Name_Table;
with Errorout; use Errorout;
with Std_Package;
with Libraries;
with Canon;
with Configuration;
with Iirs_Utils;
with Simul.Annotations;
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
      if Analyze_Only then
         Setup_Libraries (True);
      else
         Setup_Libraries (False);
         Libraries.Load_Std_Library;
         --  WORK library is not loaded.  FIXME: why ?
      end if;

      if Time_Resolution /= 'a' then
         Std_Package.Set_Time_Resolution (Time_Resolution);
      end if;

      if Analyze_Only then
         return;
      end if;

      Simul.Annotations.Annotate (Std_Package.Std_Standard_Unit);

      Canon.Canon_Flag_Add_Labels := True;
      Canon.Canon_Flag_Sequentials_Stmts := True;
      Canon.Canon_Flag_Expressions := True;
      Canon.Canon_Flag_All_Sensitivity := True;
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
      use Name_Table;
      use Types;
      use Configuration;

      First_Id : Name_Id;
      Sec_Id : Name_Id;
   begin
      Extract_Elab_Unit (Cmd_Name, Args, Opt_Arg);

      Flags.Flag_Elaborate := True;
      -- Translation.Chap12.Elaborate (Prim_Name.all, Sec_Name.all, "", True);

      if Errorout.Nbr_Errors > 0 then
         --  This may happen (bad entity for example).
         raise Compilation_Error;
      end if;

      First_Id := Get_Identifier (Prim_Name.all);
      if Sec_Name = null then
         Sec_Id := Null_Identifier;
      else
         Sec_Id := Get_Identifier (Sec_Name.all);
      end if;
      Top_Conf := Configuration.Configure (First_Id, Sec_Id);
      if Top_Conf = Null_Iir then
         raise Compilation_Error;
      end if;

      --  Check (and possibly abandon) if entity can be at the top of the
      --  hierarchy.
      declare
         Conf_Unit : constant Iir := Get_Library_Unit (Top_Conf);
         Arch : constant Iir := Get_Named_Entity
           (Get_Block_Specification (Get_Block_Configuration (Conf_Unit)));
         Entity : constant Iir := Iirs_Utils.Get_Entity (Arch);
      begin
         Configuration.Check_Entity_Declaration_Top (Entity);
         if Nbr_Errors > 0 then
            raise Compilation_Error;
         end if;
      end;

      --  Annotate all units.
      for I in Design_Units.First .. Design_Units.Last loop
         Simul.Annotations.Annotate (Design_Units.Table (I));
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

      Grtlink.Flag_String := Flags.Flag_String;

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

   procedure Disp_Long_Help
   is
      use Ada.Text_IO;
   begin
      Put_Line (" --debug        Run with debugger");
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
