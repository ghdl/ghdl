--  Ortho front-end specifications.
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

package Ortho_Front is
   type String_Acc is access String;

   --  Called before decode_option.
   --  This procedure can only do internal initializations.  It cannot call
   --  ortho subprograms.
   procedure Init;

   --  An ortho back-end decodes the command line.  Unknown options may
   --  be decoded by the user, with this function.
   --  When an ortho back-end encounter an unknown option, it sets OPT with
   --  this option and ARG with the next one, if any.
   --
   --  DECODE_OPTION must return the number of argument used, ie:
   --   0 if OPT is unknown.
   --   1 if OPT is known but ARG is unused.
   --   2 if OPT is known and ARG used.
   function Decode_Option (Opt : String_Acc; Arg : String_Acc) return Natural;

   --  Start to parse file FILENAME.
   --  Return False in case of error.
   function Parse (Filename : String_Acc) return Boolean;
end Ortho_Front;
