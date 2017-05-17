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
   -- Return true if opt is recognize by flags.
   --  Note: std_names.std_names_initialize and files_map.init_paths must have
   --  been called before this subprogram.
   function Parse_Option (Option : String) return Boolean;

   -- Disp help about these options.
   procedure Disp_Options_Help;

   --  Front-end intialization.
   procedure Initialize;
end Options;
