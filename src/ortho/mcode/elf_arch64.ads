--  ELF64 view of ELF.
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
   Elf_Ehdr_Size : constant Natural := Elf64_Ehdr_Size;
   Elf_Shdr_Size : constant Natural := Elf64_Shdr_Size;
   Elf_Phdr_Size : constant Natural := Elf64_Phdr_Size;
   Elf_Sym_Size : constant Natural := Elf64_Sym_Size;

   Elf_Arch_Class : constant Elf_Uchar := ELFCLASS64;
end Elf_Arch64;
