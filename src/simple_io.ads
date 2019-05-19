--  Very simple IO package
--  Copyright (C) 2019 Tristan Gingold
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

package Simple_IO is
   procedure Put (S : String);
   procedure Put (C : Character);
   procedure Put_Line (S : String);
   procedure New_Line;

   procedure Put_Err (S : String);
   procedure Put_Err (C : Character);
   procedure Put_Line_Err (S : String);
   procedure New_Line_Err;
end Simple_IO;
