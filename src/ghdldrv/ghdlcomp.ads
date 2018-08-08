--  GHDL driver - compile commands.
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Iirs; use Iirs;

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

   --  Functionnal interface.
   --  Must be first initialized by Compile_Init
   procedure Compile_Analyze_Init (Load_Work : Boolean := True);
   procedure Compile_Analyze_File (File : String);
   function Compile_Analyze_File2 (File : String) return Iir;
   procedure Compile_Elaborate (Unit_Name : String_Access);
   procedure Compile_Run;
end Ghdlcomp;
