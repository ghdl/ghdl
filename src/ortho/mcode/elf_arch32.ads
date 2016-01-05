--  ELF32 view of ELF.
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
with Elf_Common; use Elf_Common;
with Elf32; use Elf32;

package Elf_Arch32 is
   subtype Elf_Ehdr is Elf32_Ehdr;
   subtype Elf_Shdr is Elf32_Shdr;
   subtype Elf_Sym is Elf32_Sym;
   subtype Elf_Rel is Elf32_Rel;
   subtype Elf_Rela is Elf32_Rela;
   subtype Elf_Phdr is Elf32_Phdr;

   subtype Elf_Off is Elf32_Off;
   subtype Elf_Size is Elf32_Word;
   subtype Elf_Addr is Elf32_Addr;
   Elf_Ehdr_Size : constant Natural := Elf32_Ehdr_Size;
   Elf_Shdr_Size : constant Natural := Elf32_Shdr_Size;
   Elf_Phdr_Size : constant Natural := Elf32_Phdr_Size;
   Elf_Sym_Size : constant Natural := Elf32_Sym_Size;
   Elf_Rel_Size : constant Natural := Elf32_Rel_Size;
   Elf_Rela_Size : constant Natural := Elf32_Rela_Size;

   Elf_Arch_Class : constant Elf_Uchar := ELFCLASS32;

   function Elf_R_Sym (I : Elf32_Word) return Elf32_Word
     renames Elf32_R_Sym;
   function Elf_R_Type (I : Elf32_Word) return Elf32_Word
     renames Elf32_R_Type;
   function Elf_R_Info (S, T : Elf32_Word) return Elf32_Word
     renames Elf32_R_Info;

end Elf_Arch32;
