--  Command line options.
--  Copyright (C) 2008 Tristan Gingold
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

package Options is
   --  How an option was handled by Parse_Option.
   type Option_State is
     (
      --  Option correctly parsed.
      Option_Ok,

      --  Option is unknown.
      Option_Unknown,

      --  Option has an error (message was displayed).
      Option_Err,

      --  Option_Arg_Req: OPTION requires an argument.  Must be set only when
      --     ARG = "", the manager will recall Decode_Option.
      Option_Arg_Req,

      --  Option_Arg: OPTION used the argument.
      Option_Arg,

      Option_End
     );

   -- Return true if opt is recognize by flags.
   --  Note: std_names.std_names_initialize and files_map.init_paths must have
   --  been called before this subprogram.
   function Parse_Option (Opt : String) return Option_State;

   -- Disp help about these options.
   procedure Disp_Options_Help;

   --  Initialization
   --  There are several stages:
   --  1) C, C++ initialization, Ada elaboration
   --     That should be automatic
   --  2) ghdl initialization
   --     Call the Initialize procedure below.
   --  3) Set options
   --  4) VHDL initialization (build of standard package)
   --     Done by setup_libraries.  Must be done after options because
   --     the standard package changes according to the options
   --  5) Analyze...
   --  6) Finalize to free all the memory.
   --     Then you can restart from 2).

   --  Front-end intialization.
   procedure Initialize;

   --  Free all the memory.
   procedure Finalize;

   Option_Error: exception;
end Options;
