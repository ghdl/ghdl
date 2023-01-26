--  JIT features for ortho mcode.
--  Copyright (C) 2006-2023 Tristan Gingold
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

with System.Storage_Elements; use System.Storage_Elements;
with System; use System;

package Ortho_Mcode.Jit is
   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address);

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address;

   function Get_Type_Size (Typ : O_Tnode) return Storage_Count;
   function Get_Field_Offset (Field : O_Fnode) return Storage_Count;
end Ortho_Mcode.Jit;
