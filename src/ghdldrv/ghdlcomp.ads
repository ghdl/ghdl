--  GHDL driver - compile commands.
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Vhdl.Nodes; use Vhdl.Nodes;

package Ghdlcomp is
   --  This procedure is called at start of commands which call
   --  finish_compilation to generate code.
   type Compile_Init_Acc is access procedure (Analyze_Only : Boolean);

   --  This procedure is called for elaboration.
   --  CMD_NAME is the name of the command, used to report errors.
   --  ARGS is the argument list, starting from the unit name to be elaborated.
   --   The procedure should extract the unit.
   --  OPT_ARG is the index of the first argument from ARGS to be used as
   --   a run option.
   type Compile_Elab_Acc is access procedure
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural);

   --  Use ARGS as run options.
   --  Should do all the work.
   type Set_Run_Options_Acc is access
     procedure (Args : Argument_List);

   --  Run the simulation.
   --  All the parameters were set through calling Compile_Elab and
   --  Set_Run_Options.
   type Run_Acc is access procedure;

   --  Called when an analysis/elaboration option is decoded.
   --  Return True if OPTION is known (and do the side effects).
   --  No parameters are allowed.
   type Decode_Option_Acc is access function (Option : String) return Boolean;

   --  Disp help for options decoded by Decode_Option.
   type Disp_Long_Help_Acc is access procedure;

   --  All the hooks gathered.
   --  A record is used to be sure all hooks are set.
   type Hooks_Type is record
      Compile_Init : Compile_Init_Acc := null;
      Compile_Elab : Compile_Elab_Acc := null;
      Set_Run_Options : Set_Run_Options_Acc := null;
      Run : Run_Acc := null;
      Decode_Option : Decode_Option_Acc := null;
      Disp_Long_Help : Disp_Long_Help_Acc := null;
   end record;

   Hooks : Hooks_Type;

   --  Register commands.
   procedure Register_Commands;

   --  Output of --disp-config.
   procedure Disp_Config;

   -- --time-resolution=X
   -- Where X corresponds to:
   -- fs => 'f'
   -- ps => 'p'
   -- ns => 'n'
   -- us => 'u'
   -- ms => 'm'
   -- sec => 's'
   -- min => 'M'
   -- hr => 'h'
   Time_Resolution: Character := 'f';

   --  Common action to perform before analysis: library setup.
   procedure Common_Compile_Init (Analyze_Only : Boolean);

   --  Common action to perform before elaboration:
   --  * extract PRIM_NAME and SEC_NAME from ARGS.
   --  * configure
   --  * Check top entity
   --  If ALLOW_UNDEF_GENERIC is False, the generics must have a default value.
   procedure Common_Compile_Elab (Cmd_Name : String;
                                  Args : Argument_List;
                                  Allow_Undef_Generic : Boolean;
                                  Opt_Arg : out Natural;
                                  Config : out Iir);

   --  Functionnal interface.
   --  Must be first initialized by Compile_Init
   procedure Compile_Analyze_Init (Load_Work : Boolean := True);

   --  Load and parse FILE, put library units in the work library (without
   --  analyzing them).
   procedure Compile_Load_Vhdl_File (File : String);

   --  Load, parse and analyze FILE.
   function Compile_Analyze_File (File : String) return Iir;

   procedure Compile_Elaborate (Unit_Name : String_Access);
   procedure Compile_Run;

   --  Load and parse file (without analysis).  Put units in the work library.
   type Load_File_Acc is access procedure (File : String);

   --  Hook for verilog.
   Init_Verilog_Options : Compile_Init_Acc;
   Load_Verilog_File : Load_File_Acc;
end Ghdlcomp;
