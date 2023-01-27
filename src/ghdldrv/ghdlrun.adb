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
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Interfaces;
with Interfaces.C;

with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with Simple_IO; use Simple_IO;

with Flags;
with Options;
with Errorout; use Errorout;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Std_Package;
with Vhdl.Canon;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Back_End;
with Ortho_Jit;
with Ortho_Nodes; use Ortho_Nodes;
with Trans_Decls;
with Translation;
with Trans_Link;
with Trans_Foreign;

with Grt.Main;
with Grt.Modules;
with Grt.Options;
with Grt.Types;
with Grt.Errors;
with Grt.Backtraces.Jit;
with Grt.Analog_Solver;

with Ghdlcomp; use Ghdlcomp;
with Grtlink;

package body Ghdlrun is
   procedure Foreign_Hook (Decl : Iir;
                           Info : Vhdl.Back_End.Foreign_Info_Type;
                           Ortho : O_Dnode);

   subtype F64_C_Arr_Ptr is Grt.Analog_Solver.F64_C_Arr_Ptr;

   procedure Residues (T : Grt.Types.Ghdl_F64;
                       Y : F64_C_Arr_Ptr;
                       Yp : F64_C_Arr_Ptr;
                       Res : F64_C_Arr_Ptr);
   pragma Export (C, Residues, "grt__analog_solver__residues");

   procedure Set_Quantities_Values (Y : F64_C_Arr_Ptr; Yp: F64_C_Arr_Ptr);
   pragma Export (C, Set_Quantities_Values, "grt__analog_solver__set_values");

   procedure Residues (T : Grt.Types.Ghdl_F64;
                       Y : F64_C_Arr_Ptr;
                       Yp : F64_C_Arr_Ptr;
                       Res : F64_C_Arr_Ptr) is
   begin
      raise Program_Error;
   end Residues;

   procedure Set_Quantities_Values (Y : F64_C_Arr_Ptr; Yp: F64_C_Arr_Ptr) is
   begin
      raise Program_Error;
   end Set_Quantities_Values;

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
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
      Config : Iir;
   begin
      Common_Compile_Elab (Cmd_Name, Args, True, Opt_Arg, Config);

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

      Ortho_Jit.Init;

      Translation.Initialize;

      Translation.Elaborate (Config, True);

      if Errorout.Nbr_Errors > 0 then
         --  This may happen (bad entity for example).
         raise Compilation_Error;
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

   procedure Run
   is
      function Conv is new Ada.Unchecked_Conversion
        (Source => Address, Target => Elaborate_Acc);
      Err : Boolean;
      Decl : O_Dnode;
   begin
      if Flag_Verbose then
         Put_Line ("Linking in memory");
      end if;

      Trans_Link.Link;

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

      Grtlink.Flag_String := Flags.Flag_String;

      Grt.Backtraces.Jit.Symbolizer_Proc := Ortho_Jit.Symbolize'Access;

      Elaborate_Proc :=
        Conv (Ortho_Jit.Get_Address (Trans_Decls.Ghdl_Elaborate));

      Ortho_Jit.Finish;

      Translation.Finalize;
      Options.Finalize;

      if Flag_Verbose then
         Put_Line ("Starting simulation");
      end if;

      Grt.Modules.Register_Modules;

      Grt.Main.Run;

      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Grt.Errors.Exit_Status));
   end Run;


   --  Command run help.
   type Command_Run_Help is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Run_Help; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Run_Help) return String;
   procedure Perform_Action (Cmd : in out Command_Run_Help;
                             Args : Argument_List);

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
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error ("warning: command 'run-help' does not accept any argument");
      end if;
      Put_Line ("These options can only be placed at [RUNOPTS]");
      --  Register modules, since they add commands.
      Grt.Modules.Register_Modules;
      --  Bypass usual help header.
      Grt.Options.Argc := 0;
      Grt.Options.Help;
   end Perform_Action;

   procedure Register_Commands
   is
   begin
      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Ortho_Jit.Decode_Option'Access,
                         Ortho_Jit.Disp_Help'Access);
      Ghdlcomp.Register_Commands;
      Register_Command (new Command_Run_Help);
      Translation.Register_Translation_Back_End;
   end Register_Commands;
end Ghdlrun;
