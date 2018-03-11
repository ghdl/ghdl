--  GHDL driver for synthesis
--  Copyright (C) 2016 Tristan Gingold
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
with Ghdlcomp;
with Ghdlmain;
with Ghdlsimul;

with Libraries;
with Flags;
with Canon;

with Simul.Elaboration;

with Synthesis;
with Netlists.Dump;

package body Ghdlsynth is
   --  Command --synth
   type Command_Synth is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Synth; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Synth) return String;

   procedure Perform_Action (Cmd : in out Command_Synth;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Synth; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--synth";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Synth) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--synth [FILES... -e] UNIT [ARCH]   Synthesis from UNIT";
   end Get_Short_Help;

   function Ghdl_Synth (Args : Argument_List) return Netlists.Module
   is
      E_Opt : Integer;
      Opt_Arg : Natural;
   begin
      --  If the '-e' switch is present, there is a list of files.
      E_Opt := Args'First - 1;
      for I in Args'Range loop
         if Args (I).all = "-e" then
            E_Opt := I;
            exit;
         end if;
      end loop;

      Ghdlcomp.Hooks.Compile_Init.all (False);
      Flags.Flag_Elaborate_With_Outdated := False;
      Flags.Flag_Only_Elab_Warnings := True;

      Libraries.Load_Work_Library (E_Opt >= Args'First);

      --  Do not canon concurrent statements.
      Canon.Canon_Flag_Concurrent_Stmts := False;

      Canon.Canon_Flag_Add_Labels := True;

      --  Analyze files (if any)
      for I in Args'First .. E_Opt - 1 loop
         Ghdlcomp.Compile_Analyze_File (Args (I).all);
      end loop;

      --  Elaborate
      Ghdlcomp.Hooks.Compile_Elab.all
        ("--synth", Args (E_Opt + 1 .. Args'Last), Opt_Arg);

      if Opt_Arg <= Args'Last then
         Ghdlmain.Error ("extra options ignored");
      end if;

      --  Hooks.Set_Run_Options (Args (Opt_Arg .. Args'Last));

      Simul.Elaboration.Elaborate_Design (Ghdlsimul.Get_Top_Config);

      return Synthesis.Synth_Design (Ghdlsimul.Get_Top_Config);
      --  Hooks.Run.all;
   end Ghdl_Synth;

   procedure Perform_Action (Cmd : in out Command_Synth;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Res : Netlists.Module;
   begin
      Res := Ghdl_Synth (Args);
      Netlists.Dump.Disp_Module (Res);
   end Perform_Action;

   procedure Register_Commands is
   begin
      Ghdlmain.Register_Command (new Command_Synth);
   end Register_Commands;
end Ghdlsynth;
