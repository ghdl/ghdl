--  ELF64 view of ELF.
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
with Elf_Common; use Elf_Common;
with Elf64; use Elf64;

package Elf_Arch64 is
   subtype Elf_Ehdr is Elf64_Ehdr;
   subtype Elf_Shdr is Elf64_Shdr;
   subtype Elf_Sym is Elf64_Sym;
   subtype Elf_Rel is Elf64_Rel;
   subtype Elf_Rela is Elf64_Rela;
   subtype Elf_Phdr is Elf64_Phdr;

   subtype Elf_Off is Elf64_Off;
   subtype Elf_Size is Elf64_Xword;
   subtype Elf_Addr is Elf64_Addr;
   Elf_Ehdr_Size : constant Natural := Elf64_Ehdr_Size;
   Elf_Shdr_Size : constant Natural := Elf64_Shdr_Size;
   Elf_Phdr_Size : constant Natural := Elf64_Phdr_Size;
   Elf_Sym_Size : constant Natural := Elf64_Sym_Size;
   Elf_Rel_Size : constant Natural := Elf64_Rel_Size;
   Elf_Rela_Size : constant Natural := Elf64_Rela_Size;

   Elf_Arch_Class : constant Elf_Uchar := ELFCLASS64;

   function Elf_R_Sym (I : Elf64_Xword) return Elf_Word
     renames Elf64_R_Sym;
   function Elf_R_Type (I : Elf64_Xword) return Elf_Word
     renames Elf64_R_Type;
   function Elf_R_Info (S, T : Elf_Word) return Elf64_Xword
     renames Elf64_R_Info;
end Elf_Arch64;
