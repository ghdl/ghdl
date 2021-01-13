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
with Interfaces; use Interfaces;
with System;
with Elf_Common; use Elf_Common;

package Elf64 is
   subtype Elf64_Addr  is Unsigned_64;
   subtype Elf64_Off   is Unsigned_64;
   subtype Elf64_Uchar is Unsigned_8;
   subtype Elf64_Half  is Unsigned_16;
   subtype Elf64_Sword is Integer_32;
   subtype Elf64_Word  is Unsigned_32;
   subtype Elf64_Xword is Unsigned_64;
   subtype Elf64_Sxword is Integer_64;

   type Elf64_Ehdr is record
      E_Ident     : E_Ident_Type;
      E_Type      : Elf64_Half;
      E_Machine   : Elf64_Half;
      E_Version   : Elf64_Word;
      E_Entry     : Elf64_Addr;
      E_Phoff     : Elf64_Off;
      E_Shoff     : Elf64_Off;
      E_Flags     : Elf64_Word;
      E_Ehsize    : Elf64_Half;
      E_Phentsize : Elf64_Half;
      E_Phnum     : Elf64_Half;
      E_Shentsize : Elf64_Half;
      E_Shnum     : Elf64_Half;
      E_Shstrndx  : Elf64_Half;
   end record;

   Elf64_Ehdr_Size : constant Natural := Elf64_Ehdr'Size / System.Storage_Unit;

   type Elf64_Shdr is record
      Sh_Name      : Elf64_Word;
      Sh_Type      : Elf64_Word;
      Sh_Flags     : Elf64_Xword;
      Sh_Addr      : Elf64_Addr;
      Sh_Offset    : Elf64_Off;
      Sh_Size      : Elf64_Xword;
      Sh_Link      : Elf64_Word;
      Sh_Info      : Elf64_Word;
      Sh_Addralign : Elf64_Xword;
      Sh_Entsize   : Elf64_Xword;
   end record;
   Elf64_Shdr_Size : constant Natural := Elf64_Shdr'Size / System.Storage_Unit;

   --  Symbol table.
   type Elf64_Sym is record
      St_Name  : Elf64_Word;
      St_Info  : Elf64_Uchar;
      St_Other : Elf64_Uchar;
      St_Shndx : Elf64_Half;
      St_Value : Elf64_Addr;
      St_Size  : Elf64_Xword;
   end record;
   Elf64_Sym_Size : constant Natural := Elf64_Sym'Size / System.Storage_Unit;

   --  Relocation.
   type Elf64_Rel is record
      R_Offset : Elf64_Addr;
      R_Info : Elf64_Xword;
   end record;
   Elf64_Rel_Size : constant Natural := Elf64_Rel'Size / System.Storage_Unit;

   type Elf64_Rela is record
      R_Offset : Elf64_Addr;
      R_Info : Elf64_Xword;
      R_Addend : Elf64_Sxword;
   end record;
   Elf64_Rela_Size : constant Natural := Elf64_Rela'Size / System.Storage_Unit;

   function Elf64_R_Sym (I : Elf64_Xword) return Elf64_Word;
   function Elf64_R_Type (I : Elf64_Xword) return Elf64_Word;
   function Elf64_R_Info (S, T : Elf64_Word) return Elf64_Xword;

   --  For x86-64
   R_X86_64_NONE : constant Elf64_Word := 0;
   R_X86_64_64   : constant Elf64_Word := 1;
   R_X86_64_PC32 : constant Elf64_Word := 2;

   type Elf64_Phdr is record
      P_Type   : Elf64_Word;
      P_Flags  : Elf64_Word;
      P_Offset : Elf64_Off;
      P_Vaddr  : Elf64_Addr;
      P_Paddr  : Elf64_Addr;
      P_Filesz : Elf64_Xword;
      P_Memsz  : Elf64_Xword;
      P_Align  : Elf64_Xword;
   end record;
   Elf64_Phdr_Size : constant Natural := Elf64_Phdr'Size / System.Storage_Unit;
end Elf64;
