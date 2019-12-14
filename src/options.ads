--  Command line options.
--  Copyright (C) 2008 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

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

   --  Front-end intialization.
   procedure Initialize;

   Option_Error: exception;
end Options;
