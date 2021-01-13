--  Back-end specialization
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
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Back_End is
   --  Back-end options.
   type Parse_Option_Acc is access function (Opt : String) return Boolean;
   Parse_Option : Parse_Option_Acc := null;

   --  Disp back-end option help.
   type Disp_Option_Acc is access procedure;
   Disp_Option : Disp_Option_Acc := null;

   --  DECL is an architecture (library unit) or a subprogram (specification)
   --  decorated with a FOREIGN attribute.  Do back-end checks.
   --  May be NULL for no additionnal checks.
   type Sem_Foreign_Acc is access procedure (Decl : Iir);
   Sem_Foreign : Sem_Foreign_Acc := null;
end Vhdl.Back_End;
