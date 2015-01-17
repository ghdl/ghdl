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
with Iirs; use Iirs;
with Flags;
with Back_End;
with Name_Table;
with Errorout; use Errorout;
with Std_Package;
with Libraries;
with Canon;
with Configuration;
with Iirs_Utils;
with Annotations;
with Elaboration;
with Sim_Be;
with Simulation;
with Execution;

with Ghdlcomp;

with Grt.Vpi;
pragma Unreferenced (Grt.Vpi);
with Grt.Types;
with Grt.Options;
with Grtlink;

package body Ghdlsimul is

   --  FIXME: reuse simulation.top_config
   Top_Conf : Iir;

   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      if Analyze_Only then
         return;
      end if;

      -- Initialize.
      Back_End.Finish_Compilation := Sim_Be.Finish_Compilation'Access;
      Back_End.Sem_Foreign := null;

      Setup_Libraries (False);
      Libraries.Load_Std_Library;

      -- Here, time_base can be set.
      Annotations.Annotate (Std_Package.Std_Standard_Unit);

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

      for I in Args'Range loop
         Arg := Args (I);
         if Arg.all = "--disp-tree" then
            Simulation.Disp_Tree := True;
         elsif Arg.all = "--expect-failure" then
            Decode_Option (Arg.all, Status);
            pragma Assert (Status = Decode_Option_Ok);
         elsif Arg.all = "--trace-elab" then
            Elaboration.Trace_Elaboration := True;
         elsif Arg.all = "--trace-drivers" then
            Elaboration.Trace_Drivers := True;
         elsif Arg.all = "--trace-annotation" then
            Annotations.Trace_Annotation := True;
         elsif Arg.all = "--trace-simu" then
            Simulation.Trace_Simulation := True;
         elsif Arg.all = "--trace-stmt" then
            Execution.Trace_Statements := True;
         elsif Arg.all = "--stats" then
            Simulation.Disp_Stats := True;
         elsif Arg.all = "-i" then
            Simulation.Flag_Interractive := True;
         else
            Decode_Option (Arg.all, Status);
            case Status is
               when Decode_Option_Last =>
                  exit;
               when Decode_Option_Help =>
                  --  FIXME: is that correct ?
                  exit;
               when Decode_Option_Ok =>
                  null;
            end case;
            --  Ghdlmain.Error ("unknown run options '" & Arg.all & "'");
            --  raise Option_Error;
         end if;
      end loop;
   end Set_Run_Options;

   procedure Run is
   begin
      Grtlink.Flag_String := Flags.Flag_String;

      Simulation.Simulation_Entity (Top_Conf);
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
   begin
      if Option = "--debug" then
         Simulation.Flag_Debugger := True;
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

   procedure Register_Commands
   is
   begin
      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Decode_Option'Access,
                         Disp_Long_Help'Access);
      Ghdlcomp.Register_Commands;
   end Register_Commands;
end Ghdlsimul;
