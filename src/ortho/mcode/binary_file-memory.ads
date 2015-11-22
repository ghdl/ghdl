--  Binary file execute in memory handler.
--  Copyright (C) 2006-2015 Tristan Gingold
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
with Ada.Unchecked_Conversion;

package Binary_File.Memory is

   --  Must be called before set_symbol_address.
   procedure Write_Memory_Init;

   --  Give a value to an undefined or external symbol.
   procedure Set_Symbol_Address (Sym : Symbol; Addr : System.Address);

   --  Do the real work: resolve relocations.
   procedure Write_Memory_Relocate (Error : out Boolean);

   --  Read the result: get address and size of a section.
   function Get_Section_Addr (Sect : Section_Acc) return System.Address;
   function Get_Section_Size (Sect : Section_Acc) return Pc_Type;

   --  Helpers.
   function To_Pc_Type is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Pc_Type);
   function To_Address is new Ada.Unchecked_Conversion
     (Source => Pc_Type, Target => System.Address);

end Binary_File.Memory;
