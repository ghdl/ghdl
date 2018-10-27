--  VHDL PSL parser.
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

with Types; use Types;
with Tokens; use Tokens;

package Parse_Psl is
   function Parse_Psl_Sequence (Full_Hdl_Expr : Boolean) return PSL_Node;
   function Parse_Psl_Property return PSL_Node;
   function Parse_Psl_Boolean return PSL_Node;
   function Parse_Psl_Declaration (Tok : Token_Type) return PSL_Node;

   --  True if endpoint declaration N is instantiated (ie has no parameters).
   function Is_Instantiated_Declaration (N : PSL_Node) return Boolean;
end Parse_Psl;
