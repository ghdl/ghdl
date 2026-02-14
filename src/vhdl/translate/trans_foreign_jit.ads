--  Link of foreign subprograms for JIT backends.
--  Copyright (C) 2026 Tristan Gingold
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
with Vhdl.Back_End;
with Ortho_Nodes; use Ortho_Nodes;

package Trans_Foreign_Jit is
   procedure Init;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Vhdl.Back_End.Foreign_Info_Type;
                           Ortho : O_Dnode);
end Trans_Foreign_Jit;
