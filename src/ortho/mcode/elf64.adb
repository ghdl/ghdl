--  ELF64 definitions.
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

package body Elf64 is
   function Elf64_R_Sym (I : Elf64_Xword) return Elf64_Word is
   begin
      return Elf64_Word (Shift_Right (I, 32));
   end Elf64_R_Sym;

   function Elf64_R_Type (I : Elf64_Xword) return Elf64_Word is
   begin
      return Elf64_Word (I and 16#Ffff_ffff#);
   end Elf64_R_Type;

   function Elf64_R_Info (S, T : Elf64_Word) return Elf64_Xword is
   begin
      return Shift_Left (Elf64_Xword (S), 32) or Elf64_Xword (T);
   end Elf64_R_Info;
end Elf64;
