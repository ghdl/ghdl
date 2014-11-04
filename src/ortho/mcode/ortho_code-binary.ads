--  Interface with binary writer for mcode.
--  Copyright (C) 2006 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Binary_File; use Binary_File;

package Ortho_Code.Binary is
   function To_Symbol is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Symbol);

   function To_Int32 is new Ada.Unchecked_Conversion
     (Source => Symbol, Target => Int32);

   function Get_Decl_Symbol (Decl : O_Dnode) return Symbol;
   function Get_Label_Symbol (Label : O_Enode) return Symbol;
   procedure Set_Label_Symbol (Label : O_Enode; Sym : Symbol);
end Ortho_Code.Binary;

