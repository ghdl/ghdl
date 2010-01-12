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
with Annotations;
with Elaboration;
with Sim_Be;
with Simulation;

with Ghdlcomp;

package body Ghdlsimul is

   Flag_Expect_Failure : Boolean := False;
   pragma Unreferenced (Flag_Expect_Failure);

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
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
   begin
      Extract_Elab_Unit (Cmd_Name, Args, Opt_Arg);

      Flags.Flag_Elaborate := True;
      -- Translation.Chap12.Elaborate (Prim_Name.all, Sec_Name.all, "", True);

      if Errorout.Nbr_Errors > 0 then
         --  This may happen (bad entity for example).
         raise Compilation_Error;
      end if;
   end Compile_Elab;

   --  Set options.
   procedure Set_Run_Options (Args : Argument_List)
   is
      Arg : String_Access;
   begin
      for I in Args'Range loop
         Arg := Args (I);
         if Arg.all = "--disp-tree" then
            Simulation.Disp_Tree := True;
         elsif Arg.all = "--expect-failure" then
            Flag_Expect_Failure := True;
         elsif Arg.all = "--trace-elab" then
            Elaboration.Trace_Elaboration := True;
         elsif Arg.all = "--trace-simu" then
            Simulation.Trace_Simulation := True;
         else
            null;
         end if;
      end loop;
   end Set_Run_Options;

   procedure Run
   is
      use Name_Table;
      use Types;

      First_Id : Name_Id;
      Sec_Id : Name_Id;
      Top_Conf : Iir;
   begin
      First_Id := Get_Identifier (Prim_Name.all);
      if Sec_Name = null then
         Sec_Id := Null_Identifier;
      else
         Sec_Id := Get_Identifier (Sec_Name.all);
      end if;
      Top_Conf := Configuration.Configure (First_Id, Sec_Id);

      Simulation.Simulation_Entity (Top_Conf);
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
      pragma Unreferenced (Option);
   begin
      return False;
   end Decode_Option;

   procedure Disp_Long_Help
   is
   begin
      null;
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
