--  EDIF parser.
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

with Edif.Nodes; use Edif.Nodes;

package Edif.Parse is
   --  Simple parser: return generic constructs (lists).
   --  Do not try to interpret EDIF.
   function Parse_File_Simple return Node;

   --  Parse as EDIF 2.0.0
   --  There is almost no error recovery: the parser stops at the first error,
   --  and it return Null_Node.
   function Parse_Edif200 return Node;
end Edif.Parse;
