--  ELF definitions.
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
package body Elf_Common is
   function Elf_St_Bind (Info : Elf_Uchar) return Elf_Uchar is
   begin
      return Shift_Right (Info, 4);
   end Elf_St_Bind;

   function Elf_St_Type (Info : Elf_Uchar) return Elf_Uchar is
   begin
      return Info and 16#0F#;
   end Elf_St_Type;

   function Elf_St_Info (B, T : Elf_Uchar) return Elf_Uchar is
   begin
      return Shift_Left (B, 4) or T;
   end Elf_St_Info;

--     function Elf32_R_Sym (I : Elf32_Word) return Elf32_Word is
--     begin
--        return Shift_Right (I, 8);
--     end Elf32_R_Sym;

--     function Elf32_R_Type (I : Elf32_Word) return Elf32_Word is
--     begin
--        return I and 16#Ff#;
--     end Elf32_R_Type;

--     function Elf32_R_Info (S, T : Elf32_Word) return Elf32_Word is
--     begin
--        return Shift_Left (S, 8) or T;
--     end Elf32_R_Info;
end Elf_Common;
