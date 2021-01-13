--  Interface with binary writer for mcode.
--  Copyright (C) 2006 Tristan Gingold
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

