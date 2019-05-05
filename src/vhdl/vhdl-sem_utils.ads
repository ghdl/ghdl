--  Semantic utilities.
--  Copyright (C) 2018 Tristan Gingold
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

package Vhdl.Sem_Utils is
   --  Compute and set the hash profile of a subprogram or enumeration clause.
   procedure Compute_Subprogram_Hash (Subprg : Iir);

   function Create_Anonymous_Interface
     (Atype : Iir) return Iir_Interface_Constant_Declaration;

   --  Create predefined operations for DECL.
   procedure Create_Implicit_Operations
     (Decl : Iir; Is_Std_Standard : Boolean := False);
end Vhdl.Sem_Utils;
