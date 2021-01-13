--  Nodes recognizer for ieee packages - utilities.
--  Copyright (C) 2016 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Ieee is
   --  Skip constant string declaration for a copyright, if present.
   function Skip_Copyright_Notice (Decl : Iir) return Iir;

   --  Return the next node after implicit subprogram declarations.
   function Skip_Implicit (Decl : Iir) return Iir;
end Vhdl.Ieee;
