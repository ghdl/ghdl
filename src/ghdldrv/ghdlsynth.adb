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

with Types; use Types;
with Ghdllocal; use Ghdllocal;
with Ghdlcomp; use Ghdlcomp;
with Ghdlmain; use Ghdlmain;
with Options; use Options;
with Errorout;

with Libraries;
with Flags;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Errors;
with Vhdl.Std_Package;
with Vhdl.Canon;
with Vhdl.Configuration;
with Vhdl.Annotations;
with Vhdl.Utils;

with Synthesis;
with Netlists; use Netlists;
with Netlists.Dump;
with Netlists.Disp_Vhdl;

package body Ghdlsynth is
   type Out_Format is (Format_Raw, Format_Vhdl);

   --  Command --synth
   type Command_Synth is new Command_Lib with record
      Disp_Inline : Boolean := True;
      Oformat : Out_Format := Format_Vhdl;
   end record;
   function Decode_Command (Cmd : Command_Synth; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Synth) return String;
   procedure Decode_Option (Cmd : in out Command_Synth;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Perform_Action (Cmd : Command_Synth;
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

   procedure Decode_Option (Cmd : in out Command_Synth;
                            Option : String;
                            Arg : String;
                            Res : out Option_State) is
   begin
      if Option = "--disp-noinline" then
         Cmd.Disp_Inline := False;
         Res := Option_Ok;
      elsif Option = "--out=raw" then
         Cmd.Oformat := Format_Raw;
         Res := Option_Ok;
      elsif Option = "--out=vhdl" then
         Cmd.Oformat := Format_Vhdl;
         Res := Option_Ok;
      else
         Decode_Option (Command_Lib (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   --  Init, analyze and configure.
   --  Return the top configuration.
   function Ghdl_Synth_Configure (Args : Argument_List) return Node
   is
      use Vhdl.Errors;
      use Vhdl.Configuration;
      use Errorout;
      E_Opt : Integer;
      Opt_Arg : Natural;
      Config : Iir;
      Top : Iir;
      Prim_Id : Name_Id;
      Sec_Id : Name_Id;
   begin
      --  If the '-e' switch is present, there is a list of files.
      E_Opt := Args'First - 1;
      for I in Args'Range loop
         if Args (I).all = "-e" then
            E_Opt := I;
            exit;
         end if;
      end loop;

      Vhdl.Annotations.Flag_Synthesis := True;

      Common_Compile_Init (False);
      --  Will elaborate.
      Flags.Flag_Elaborate := True;
      Flags.Flag_Elaborate_With_Outdated := E_Opt >= Args'First;
      Flags.Flag_Only_Elab_Warnings := True;

      Libraries.Load_Work_Library (E_Opt >= Args'First);

      --  Do not canon concurrent statements.
      Vhdl.Canon.Canon_Flag_Concurrent_Stmts := False;

      --  Analyze files (if any)
      for I in Args'First .. E_Opt - 1 loop
         Ghdlcomp.Compile_Analyze_File (Args (I).all);
      end loop;

      --  Elaborate
      if E_Opt = Args'Last then
         --  No unit.
         Top := Vhdl.Configuration.Find_Top_Entity (Libraries.Work_Library);
         if Top = Null_Node then
            Ghdlmain.Error ("no top unit found");
            return Null_Iir;
         end if;
         Errorout.Report_Msg (Msgid_Note, Option, No_Source_Coord,
                              "top entity is %i", (1 => +Top));
         if Nbr_Errors > 0 then
            --  No need to configure if there are missing units.
            return Null_Iir;
         end if;
         Prim_Id := Get_Identifier (Top);
         Sec_Id := Null_Identifier;
      else
         Extract_Elab_Unit ("--synth", Args (E_Opt + 1 .. Args'Last), Opt_Arg,
                            Prim_Id, Sec_Id);
         if Opt_Arg <= Args'Last then
            Ghdlmain.Error ("extra options ignored");
            return Null_Iir;
         end if;
      end if;

      Config := Vhdl.Configuration.Configure (Prim_Id, Sec_Id);

      if Nbr_Errors > 0 then
         --  No need to configure if there are missing units.
         return Null_Iir;
      end if;

      --  Check (and possibly abandon) if entity can be at the top of the
      --  hierarchy.
      declare
         Entity : constant Iir :=
           Vhdl.Utils.Get_Entity_From_Configuration (Config);
      begin
         Vhdl.Configuration.Check_Entity_Declaration_Top (Entity, False);
         if Nbr_Errors > 0 then
            return Null_Iir;
         end if;
      end;

      --  Annotate all units.
      Vhdl.Annotations.Annotate (Vhdl.Std_Package.Std_Standard_Unit);
      for I in Design_Units.First .. Design_Units.Last loop
         Vhdl.Annotations.Annotate (Design_Units.Table (I));
      end loop;

      return Config;
   end Ghdl_Synth_Configure;

   function Ghdl_Synth (Args : Argument_List) return Module
   is
      Config : Node;
   begin
      Config := Ghdl_Synth_Configure (Args);

      if Config = Null_Iir then
         return No_Module;
      end if;

      return Synthesis.Synth_Design (Config);
   end Ghdl_Synth;

   procedure Perform_Action (Cmd : Command_Synth;
                             Args : Argument_List)
   is
      Res : Module;
      Config : Iir;
   begin
      Config := Ghdl_Synth_Configure (Args);

      if Config = Null_Iir then
         raise Errorout.Compilation_Error;
      end if;

      Res := Synthesis.Synth_Design (Config);
      if Res = No_Module then
         raise Errorout.Compilation_Error;
      end if;

      case Cmd.Oformat is
         when Format_Raw =>
            Netlists.Dump.Flag_Disp_Inline := Cmd.Disp_Inline;
            Netlists.Dump.Disp_Module (Res);
         when Format_Vhdl =>
            Netlists.Disp_Vhdl.Disp_Vhdl (Res);
      end case;
   end Perform_Action;

   procedure Register_Commands is
   begin
      Ghdlmain.Register_Command (new Command_Synth);
   end Register_Commands;
end Ghdlsynth;
