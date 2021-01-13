--  VHDL PSL parser.
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

with PSL.Types; use PSL.Types;
with Vhdl.Tokens; use Vhdl.Tokens;

package Vhdl.Parse_Psl is
   function Parse_Psl_Sequence return PSL_Node;
   function Parse_Psl_Property return PSL_Node;
   function Parse_Psl_Boolean return PSL_Node;
   function Parse_Psl_Declaration (Tok : Token_Type) return PSL_Node;

   --  True if endpoint declaration N is instantiated (ie has no parameters).
   function Is_Instantiated_Declaration (N : PSL_Node) return Boolean;
end Vhdl.Parse_Psl;
