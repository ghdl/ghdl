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

with Ada.Unchecked_Conversion;

with Ortho_Code.Binary;
with Binary_File; use Binary_File;
with Binary_File.Memory;

package body Ortho_Mcode.Jit is
   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address)
   is
      use Ortho_Code.Binary;
   begin
      Binary_File.Memory.Set_Symbol_Address
        (Get_Decl_Symbol (Ortho_Code.O_Dnode (Decl)), Addr);
   end Set_Address;

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address
   is
      use Ortho_Code.Binary;

      function Conv is new Ada.Unchecked_Conversion
        (Source => Pc_Type, Target => Address);
   begin
      return Conv (Get_Symbol_Vaddr
                     (Get_Decl_Symbol (Ortho_Code.O_Dnode (Decl))));
   end Get_Address;

   function Get_Type_Size (Typ : O_Tnode) return Storage_Count is
   begin
      return Storage_Count
        (Ortho_Code.Types.Get_Type_Size (Ortho_Code.O_Tnode (Typ)));
   end Get_Type_Size;

   function Get_Field_Offset (Field : O_Fnode) return Storage_Count is
   begin
      return Storage_Count
        (Ortho_Code.Types.Get_Field_Offset (Ortho_Code.O_Fnode (Field)));
   end Get_Field_Offset;
end Ortho_Mcode.Jit;
