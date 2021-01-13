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
with Interfaces; use Interfaces;
with System;
with Elf_Common; use Elf_Common;

package Elf32 is
   subtype Elf32_Addr  is Unsigned_32;
   subtype Elf32_Half  is Unsigned_16;
   subtype Elf32_Off   is Unsigned_32;
   subtype Elf32_Sword is Integer_32;
   subtype Elf32_Word  is Unsigned_32;
   subtype Elf32_Uchar is Unsigned_8;

   type Elf32_Ehdr is record
      E_Ident     : E_Ident_Type;
      E_Type      : Elf32_Half;
      E_Machine   : Elf32_Half;
      E_Version   : Elf32_Word;
      E_Entry     : Elf32_Addr;
      E_Phoff     : Elf32_Off;
      E_Shoff     : Elf32_Off;
      E_Flags     : Elf32_Word;
      E_Ehsize    : Elf32_Half;
      E_Phentsize : Elf32_Half;
      E_Phnum     : Elf32_Half;
      E_Shentsize : Elf32_Half;
      E_Shnum     : Elf32_Half;
      E_Shstrndx  : Elf32_Half;
   end record;

   Elf32_Ehdr_Size : constant Natural := Elf32_Ehdr'Size / System.Storage_Unit;

   type Elf32_Shdr is record
      Sh_Name      : Elf32_Word;
      Sh_Type      : Elf32_Word;
      Sh_Flags     : Elf32_Word;
      Sh_Addr      : Elf32_Addr;
      Sh_Offset    : Elf32_Off;
      Sh_Size      : Elf32_Word;
      Sh_Link      : Elf32_Word;
      Sh_Info      : Elf32_Word;
      Sh_Addralign : Elf32_Word;
      Sh_Entsize   : Elf32_Word;
   end record;
   Elf32_Shdr_Size : constant Natural := Elf32_Shdr'Size / System.Storage_Unit;

   --  Symbol table.
   type Elf32_Sym is record
      St_Name  : Elf32_Word;
      St_Value : Elf32_Addr;
      St_Size  : Elf32_Word;
      St_Info  : Elf32_Uchar;
      St_Other : Elf32_Uchar;
      St_Shndx : Elf32_Half;
   end record;
   Elf32_Sym_Size : constant Natural := Elf32_Sym'Size / System.Storage_Unit;

   --  Relocation.
   type Elf32_Rel is record
      R_Offset : Elf32_Addr;
      R_Info : Elf32_Word;
   end record;
   Elf32_Rel_Size : constant Natural := Elf32_Rel'Size / System.Storage_Unit;

   type Elf32_Rela is record
      R_Offset : Elf32_Addr;
      R_Info : Elf32_Word;
      R_Addend : Elf32_Sword;
   end record;
   Elf32_Rela_Size : constant Natural := Elf32_Rela'Size / System.Storage_Unit;

   function Elf32_R_Sym (I : Elf32_Word) return Elf32_Word;
   function Elf32_R_Type (I : Elf32_Word) return Elf32_Word;
   function Elf32_R_Info (S, T : Elf32_Word) return Elf32_Word;

   --  For i386
   R_386_NONE : constant Elf32_Word := 0; -- none none
   R_386_32   : constant Elf32_Word := 1; -- word32 S+A
   R_386_PC32 : constant Elf32_Word := 2; -- word32 S+A-P

   --  For sparc
   R_SPARC_NONE    : constant Elf32_Word := 0; -- none
   R_SPARC_32 :      constant Elf32_Word := 3; -- (S + A)
   R_SPARC_WDISP30 : constant Elf32_Word := 7; -- (S + A - P) >> 2
   R_SPARC_WDISP22 : constant Elf32_Word := 8; -- (S + A - P) >> 2
   R_SPARC_HI22 :    constant Elf32_Word := 9; -- (S + A) >> 10
   R_SPARC_LO10 :    constant Elf32_Word := 12; -- (S + A) & 0x3ff
   R_SPARC_UA32 :    constant Elf32_Word := 23; -- (S + A)

   type Elf32_Phdr is record
      P_Type   : Elf32_Word;
      P_Offset : Elf32_Off;
      P_Vaddr  : Elf32_Addr;
      P_Paddr  : Elf32_Addr;
      P_Filesz : Elf32_Word;
      P_Memsz  : Elf32_Word;
      P_Flags  : Elf32_Word;
      P_Align  : Elf32_Word;
   end record;
   Elf32_Phdr_Size : constant Natural := Elf32_Phdr'Size / System.Storage_Unit;
end Elf32;
