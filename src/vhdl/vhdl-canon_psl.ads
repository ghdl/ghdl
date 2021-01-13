--  Canonicalization pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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
with PSL.Types; use PSL.Types;

package Vhdl.Canon_PSL is
   --  Version of Canon.Canon_Extract_Sensitivity for PSL nodes.
   procedure Canon_Extract_Sensitivity
     (Expr: PSL_Node; Sensitivity_List: Iir_List);
end Vhdl.Canon_PSL;
