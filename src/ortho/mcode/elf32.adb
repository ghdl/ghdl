--  ELF32 definitions.
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
package body Elf32 is
   function Elf32_R_Sym (I : Elf32_Word) return Elf32_Word is
   begin
      return Shift_Right (I, 8);
   end Elf32_R_Sym;

   function Elf32_R_Type (I : Elf32_Word) return Elf32_Word is
   begin
      return I and 16#Ff#;
   end Elf32_R_Type;

   function Elf32_R_Info (S, T : Elf32_Word) return Elf32_Word is
   begin
      return Shift_Left (S, 8) or T;
   end Elf32_R_Info;
end Elf32;
