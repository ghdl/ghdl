--  ELF definitions.
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

package Elf_Common is
   subtype Elf_Half  is Unsigned_16;
   subtype Elf_Sword is Integer_32;
   subtype Elf_Word  is Unsigned_32;
   subtype Elf_Uchar is Unsigned_8;

   EI_NIDENT : constant Natural := 16;
   type E_Ident_Type is array (Natural range 0 .. EI_NIDENT - 1)
     of Elf_Uchar;

   --  e_type values.
   ET_NONE   : constant Elf_Half := 0;        --  No file type
   ET_REL    : constant Elf_Half := 1;        --  Relocatable file
   ET_EXEC   : constant Elf_Half := 2;        --  Executable file
   ET_DYN    : constant Elf_Half := 3;        --  Shared object file
   ET_CORE   : constant Elf_Half := 4;        --  Core file
   ET_LOPROC : constant Elf_Half := 16#Ff00#; --  Processor-specific
   ET_HIPROC : constant Elf_Half := 16#Ffff#; --  Processor-specific

   --  e_machine values.
   EM_NONE        : constant Elf_Half := 0;  --  No machine
   EM_M32         : constant Elf_Half := 1;  --  AT&T WE 32100
   EM_SPARC       : constant Elf_Half := 2;  --  SPARC
   EM_386         : constant Elf_Half := 3;  --  Intel Architecture
   EM_68K         : constant Elf_Half := 4;  --  Motorola 68000
   EM_88K         : constant Elf_Half := 5;  --  Motorola 88000
   EM_860         : constant Elf_Half := 7;  --  Intel 80860
   EM_MIPS        : constant Elf_Half := 8;  --  MIPS RS3000 Big-Endian
   EM_MIPS_RS4_BE : constant Elf_Half := 10; --  MIPS RS4000 Big-Endian
   EM_X86_64      : constant Elf_Half := 62;
   -- RESERVED : constant Elf_Half := 11; -- -16 Reserved for future use

   --  e_version
   EV_NONE    : constant Elf_Uchar := 0; --  Invalid versionn
   EV_CURRENT : constant Elf_Uchar := 1; --  Current version

   --  e_ident identification indexes.
   EI_MAG0    : constant Natural := 0;  --  File identification
   EI_MAG1    : constant Natural := 1;  --  File identification
   EI_MAG2    : constant Natural := 2;  --  File identification
   EI_MAG3    : constant Natural := 3;  --  File identification
   EI_CLASS   : constant Natural := 4;  --  File class
   EI_DATA    : constant Natural := 5;  --  Data encoding
   EI_VERSION : constant Natural := 6;  --  File version
   EI_PAD     : constant Natural := 7;  --  Start of padding bytes
   --EI_NIDENT  : constant Natural := 16; --  Size of e_ident[]

   --  Magic values.
   ELFMAG0 : constant Elf_Uchar := 16#7f#; --  e_ident[EI_MAG0]
   ELFMAG1 : constant Elf_Uchar := Character'Pos ('E'); --  e_ident[EI_MAG1]
   ELFMAG2 : constant Elf_Uchar := Character'Pos ('L'); --  e_ident[EI_MAG2]
   ELFMAG3 : constant Elf_Uchar := Character'Pos ('F'); --  e_ident[EI_MAG3]

   ELFCLASSNONE : constant Elf_Uchar := 0; --  Invalid class
   ELFCLASS32   : constant Elf_Uchar := 1; --  32-bit objects
   ELFCLASS64   : constant Elf_Uchar := 2; --  64-bit objects

   ELFDATANONE : constant Elf_Uchar := 0; --  Invalid data encoding
   ELFDATA2LSB : constant Elf_Uchar := 1; --  See below
   ELFDATA2MSB : constant Elf_Uchar := 2; --  See below

   SHN_UNDEF     : constant Elf_Half := 0; --
   SHN_LORESERVE : constant Elf_Half := 16#Ff00#; --
   SHN_LOPROC    : constant Elf_Half := 16#ff00#; --
   SHN_HIPROC    : constant Elf_Half := 16#ff1f#; --
   SHN_ABS       : constant Elf_Half := 16#fff1#; --
   SHN_COMMON    : constant Elf_Half := 16#fff2#; --
   SHN_HIRESERVE : constant Elf_Half := 16#ffff#; --

   -- Sh_type.
   SHT_NULL          : constant Elf_Word := 0;
   SHT_PROGBITS      : constant Elf_Word := 1;
   SHT_SYMTAB        : constant Elf_Word := 2;
   SHT_STRTAB        : constant Elf_Word := 3;
   SHT_RELA          : constant Elf_Word := 4;
   SHT_HASH          : constant Elf_Word := 5;
   SHT_DYNAMIC       : constant Elf_Word := 6;
   SHT_NOTE          : constant Elf_Word := 7;
   SHT_NOBITS        : constant Elf_Word := 8;
   SHT_REL           : constant Elf_Word := 9;
   SHT_SHLIB         : constant Elf_Word := 10;
   SHT_DYNSYM        : constant Elf_Word := 11;
   SHT_INIT_ARRAY    : constant Elf_Word := 14;
   SHT_FINI_ARRAY    : constant Elf_Word := 15;
   SHT_PREINIT_ARRAY : constant Elf_Word := 16;
   SHT_GROUP         : constant Elf_Word := 17;
   SHT_SYMTAB_SHNDX  : constant Elf_Word := 18;
   SHT_NUM           : constant Elf_Word := 19;
   SHT_LOOS          : constant Elf_Word := 16#60000000#;
   SHT_GNU_LIBLIST   : constant Elf_Word := 16#6ffffff7#;
   SHT_CHECKSUM      : constant Elf_Word := 16#6ffffff8#;
   SHT_LOSUNW        : constant Elf_Word := 16#6ffffffa#;
   SHT_SUNW_Move     : constant Elf_Word := 16#6ffffffa#;
   SHT_SUNW_COMDAT   : constant Elf_Word := 16#6ffffffb#;
   SHT_SUNW_Syminfo  : constant Elf_Word := 16#6ffffffc#;
   SHT_GNU_Verdef    : constant Elf_Word := 16#6ffffffd#;
   SHT_GNU_Verneed   : constant Elf_Word := 16#6ffffffe#;
   SHT_GNU_Versym    : constant Elf_Word := 16#6fffffff#;
   SHT_HISUNW        : constant Elf_Word := 16#6fffffff#;
   SHT_HIOS          : constant Elf_Word := 16#6fffffff#;
   SHT_LOPROC        : constant Elf_Word := 16#70000000#;
   SHT_HIPROC        : constant Elf_Word := 16#7fffffff#;
   SHT_LOUSER        : constant Elf_Word := 16#80000000#;
   SHT_HIUSER        : constant Elf_Word := 16#ffffffff#;

   SHF_WRITE     : constant := 16#1#;
   SHF_ALLOC     : constant := 16#2#;
   SHF_EXECINSTR : constant := 16#4#;
   SHF_MASKPROC  : constant := 16#F0000000#;

   function Elf_St_Bind (Info : Elf_Uchar) return Elf_Uchar;
   function Elf_St_Type (Info : Elf_Uchar) return Elf_Uchar;
   function Elf_St_Info (B, T : Elf_Uchar) return Elf_Uchar;
   pragma Inline (Elf_St_Bind);
   pragma Inline (Elf_St_Type);
   pragma Inline (Elf_St_Info);

   --  Symbol binding.
   STB_LOCAL  : constant Elf_Uchar := 0;
   STB_GLOBAL : constant Elf_Uchar := 1;
   STB_WEAK   : constant Elf_Uchar := 2;
   STB_LOPROC : constant Elf_Uchar := 13;
   STB_HIPROC : constant Elf_Uchar := 15;

   --  Symbol types.
   STT_NOTYPE  : constant Elf_Uchar := 0;
   STT_OBJECT  : constant Elf_Uchar := 1;
   STT_FUNC    : constant Elf_Uchar := 2;
   STT_SECTION : constant Elf_Uchar := 3;
   STT_FILE    : constant Elf_Uchar := 4;
   STT_LOPROC  : constant Elf_Uchar := 13;
   STT_HIPROC  : constant Elf_Uchar := 15;


   PT_NULL         : constant Elf_Word := 0;
   PT_LOAD         : constant Elf_Word := 1;
   PT_DYNAMIC      : constant Elf_Word := 2;
   PT_INTERP       : constant Elf_Word := 3;
   PT_NOTE         : constant Elf_Word := 4;
   PT_SHLIB        : constant Elf_Word := 5;
   PT_PHDR         : constant Elf_Word := 6;
   PT_TLS          : constant Elf_Word := 7;
   PT_NUM          : constant Elf_Word := 8;
   PT_LOOS         : constant Elf_Word := 16#60000000#;
   PT_GNU_EH_FRAME : constant Elf_Word := 16#6474e550#;
   PT_LOSUNW       : constant Elf_Word := 16#6ffffffa#;
   PT_SUNWBSS      : constant Elf_Word := 16#6ffffffa#;
   PT_SUNWSTACK    : constant Elf_Word := 16#6ffffffb#;
   PT_HISUNW       : constant Elf_Word := 16#6fffffff#;
   PT_HIOS         : constant Elf_Word := 16#6fffffff#;
   PT_LOPROC       : constant Elf_Word := 16#70000000#;
   PT_HIPROC       : constant Elf_Word := 16#7fffffff#;

   PF_X : constant Elf_Word := 1;
   PF_W : constant Elf_Word := 2;
   PF_R : constant Elf_Word := 4;

   DT_NULL            : constant Elf_Word := 0;
   DT_NEEDED          : constant Elf_Word := 1;
   DT_PLTRELSZ        : constant Elf_Word := 2;
   DT_PLTGOT          : constant Elf_Word := 3;
   DT_HASH            : constant Elf_Word := 4;
   DT_STRTAB          : constant Elf_Word := 5;
   DT_SYMTAB          : constant Elf_Word := 6;
   DT_RELA            : constant Elf_Word := 7;
   DT_RELASZ          : constant Elf_Word := 8;
   DT_RELAENT         : constant Elf_Word := 9;
   DT_STRSZ           : constant Elf_Word := 10;
   DT_SYMENT          : constant Elf_Word := 11;
   DT_INIT            : constant Elf_Word := 12;
   DT_FINI            : constant Elf_Word := 13;
   DT_SONAME          : constant Elf_Word := 14;
   DT_RPATH           : constant Elf_Word := 15;
   DT_SYMBOLIC        : constant Elf_Word := 16;
   DT_REL             : constant Elf_Word := 17;
   DT_RELSZ           : constant Elf_Word := 18;
   DT_RELENT          : constant Elf_Word := 19;
   DT_PLTREL          : constant Elf_Word := 20;
   DT_DEBUG           : constant Elf_Word := 21;
   DT_TEXTREL         : constant Elf_Word := 22;
   DT_JMPREL          : constant Elf_Word := 23;
   DT_BIND_NOW        : constant Elf_Word := 24;
   DT_INIT_ARRAY      : constant Elf_Word := 25;
   DT_FINI_ARRAY      : constant Elf_Word := 26;
   DT_INIT_ARRAYSZ    : constant Elf_Word := 27;
   DT_FINI_ARRAYSZ    : constant Elf_Word := 28;
   DT_RUNPATH         : constant Elf_Word := 29;
   DT_FLAGS           : constant Elf_Word := 30;
   DT_ENCODING        : constant Elf_Word := 32;
   DT_PREINIT_ARRAY   : constant Elf_Word := 32;
   DT_PREINIT_ARRAYSZ : constant Elf_Word := 33;
   DT_NUM             : constant Elf_Word := 34;
   DT_LOOS            : constant Elf_Word := 16#60000000#;
   DT_HIOS            : constant Elf_Word := 16#6fffffff#;
   DT_LOPROC          : constant Elf_Word := 16#70000000#;
   DT_HIPROC          : constant Elf_Word := 16#7fffffff#;
   DT_VALRNGLO        : constant Elf_Word := 16#6ffffd00#;
   DT_GNU_PRELINKED   : constant Elf_Word := 16#6ffffdf5#;
   DT_GNU_CONFLICTSZ  : constant Elf_Word := 16#6ffffdf6#;
   DT_GNU_LIBLISTSZ   : constant Elf_Word := 16#6ffffdf7#;
   DT_CHECKSUM        : constant Elf_Word := 16#6ffffdf8#;
   DT_PLTPADSZ        : constant Elf_Word := 16#6ffffdf9#;
   DT_MOVEENT         : constant Elf_Word := 16#6ffffdfa#;
   DT_MOVESZ          : constant Elf_Word := 16#6ffffdfb#;
   DT_FEATURE_1       : constant Elf_Word := 16#6ffffdfc#;
   DT_POSFLAG_1       : constant Elf_Word := 16#6ffffdfd#;
   DT_SYMINSZ         : constant Elf_Word := 16#6ffffdfe#;
   DT_SYMINENT        : constant Elf_Word := 16#6ffffdff#;
   DT_VALRNGHI        : constant Elf_Word := 16#6ffffdff#;
   DT_ADDRRNGLO       : constant Elf_Word := 16#6ffffe00#;
   DT_GNU_CONFLICT    : constant Elf_Word := 16#6ffffef8#;
   DT_GNU_LIBLIST     : constant Elf_Word := 16#6ffffef9#;
   DT_CONFIG          : constant Elf_Word := 16#6ffffefa#;
   DT_DEPAUDIT        : constant Elf_Word := 16#6ffffefb#;
   DT_AUDIT           : constant Elf_Word := 16#6ffffefc#;
   DT_PLTPAD          : constant Elf_Word := 16#6ffffefd#;
   DT_MOVETAB         : constant Elf_Word := 16#6ffffefe#;
   DT_SYMINFO         : constant Elf_Word := 16#6ffffeff#;
   DT_ADDRRNGHI       : constant Elf_Word := 16#6ffffeff#;
   DT_VERSYM          : constant Elf_Word := 16#6ffffff0#;
   DT_RELACOUNT       : constant Elf_Word := 16#6ffffff9#;
   DT_RELCOUNT        : constant Elf_Word := 16#6ffffffa#;
   DT_FLAGS_1         : constant Elf_Word := 16#6ffffffb#;
   DT_VERDEF          : constant Elf_Word := 16#6ffffffc#;
   DT_VERDEFNUM       : constant Elf_Word := 16#6ffffffd#;
   DT_VERNEED         : constant Elf_Word := 16#6ffffffe#;
   DT_VERNEEDNUM      : constant Elf_Word := 16#6fffffff#;
   DT_AUXILIARY       : constant Elf_Word := 16#7ffffffd#;
   DT_FILTER          : constant Elf_Word := 16#7fffffff#;

end Elf_Common;
