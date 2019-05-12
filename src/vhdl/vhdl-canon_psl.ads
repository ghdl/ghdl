--  Canonicalization pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;
with PSL.Types; use PSL.Types;

package Vhdl.Canon_PSL is
   --  Version of Canon.Canon_Extract_Sensitivity for PSL nodes.
   procedure Canon_Extract_Sensitivity
     (Expr: PSL_Node; Sensitivity_List: Iir_List);
end Vhdl.Canon_PSL;
