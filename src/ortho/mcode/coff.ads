--  COFF definitions.
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
with System; use System;

package Coff is
   type Filehdr is record
      F_Magic  : Unsigned_16;    --  Magic number.
      F_Nscns  : Unsigned_16;    --  Number of sections.
      F_Timdat : Unsigned_32;    --  Time and date stamp.
      F_Symptr : Unsigned_32;    --  File pointer to symtab.
      F_Nsyms  : Unsigned_32;    --  Number of symtab entries.
      F_Opthdr : Unsigned_16;    --  Size of optionnal header.
      F_Flags  : Unsigned_16;    --  Flags;
   end record;

   --  Size of Filehdr.
   Filehdr_Size : constant Natural := Filehdr'Size / Storage_Unit;

   --  Magic numbers.
   I386magic  : constant Unsigned_16 := 16#014c#;
   X8664magic : constant Unsigned_16 := 16#8664#;

   --  Flags of file header.
   --  Relocation info stripped from file.
   F_Relflg : constant Unsigned_16 := 16#0001#;

   --  File is executable (no unresolved symbols).
   F_Exec : constant Unsigned_16 := 16#0002#;

   --  Line numbers stripped from file.
   F_Lnno : constant Unsigned_16 := 16#0004#;

   --  Local symbols stripped from file.
   F_Lsyms : constant Unsigned_16 := 16#0008#;

   type Scnhdr is record
      S_Name    : String (1 .. 8); --  Section name.
      S_Paddr   : Unsigned_32;    --  Physical address.
      S_Vaddr   : Unsigned_32;    --  Virtual address.
      S_Size    : Unsigned_32;     --  Section size.
      S_Scnptr  : Unsigned_32;   --  File pointer to raw section data.
      S_Relptr  : Unsigned_32;   --  File pointer to relocation data.
      S_Lnnoptr : Unsigned_32;  --  File pointer to line number data.
      S_Nreloc  : Unsigned_16;   --  Number of relocation entries.
      S_Nlnno   : Unsigned_16;    --  Number of line number entries.
      S_Flags   : Unsigned_32;    --  Flags.
   end record;
   Scnhdr_Size : constant Natural := Scnhdr'Size / Storage_Unit;

   -- section contains text only.
   STYP_TEXT : constant Unsigned_32 := 16#0020#;
   -- section contains data only.
   STYP_DATA : constant Unsigned_32 := 16#0040#;
   -- section contains bss only.
   STYP_BSS  : constant Unsigned_32 := 16#0080#;

   type Strent_Type is record
      E_Zeroes : Unsigned_32;
      E_Offset : Unsigned_32;
   end record;

   type Sym_Name (Inline : Boolean := True) is record
      case Inline is
         when True =>
            E_Name : String (1 .. 8);
         when False =>
            E : Strent_Type;
      end case;
   end record;
   pragma Unchecked_Union (Sym_Name);
   for Sym_Name'Size use 64;

   type Syment is record
      E        : Sym_Name;       --  Name of the symbol
      E_Value  : Unsigned_32;    --  Value
      E_Scnum  : Unsigned_16;    --  Section
      E_Type   : Unsigned_16;
      E_Sclass : Unsigned_8;
      E_Numaux : Unsigned_8;
   end record;
   Symesz : constant Natural := 18;
   for Syment'Size use Symesz * Storage_Unit;

   --  An undefined (extern) symbol.
   N_UNDEF : constant Unsigned_16 := 16#00_00#;
   --  An absolute symbol (e_value is a constant, not an address).
   N_ABS   : constant Unsigned_16 := 16#Ff_Ff#;
   --  A debugging symbol.
   N_DEBUG : constant Unsigned_16 := 16#Ff_Fe#;

   C_NULL    : constant Unsigned_8 := 0;
   C_AUTO    : constant Unsigned_8 := 1;
   C_EXT     : constant Unsigned_8 := 2;
   C_STAT    : constant Unsigned_8 := 3;
   C_REG     : constant Unsigned_8 := 4;
   C_EXTDEF  : constant Unsigned_8 := 5;
   C_LABEL   : constant Unsigned_8 := 6;
   C_ULABEL  : constant Unsigned_8 := 7;
   C_MOS     : constant Unsigned_8 := 8;
   C_ARG     : constant Unsigned_8 := 9;
   C_STRTAG  : constant Unsigned_8 := 10;
   C_MOU     : constant Unsigned_8 := 11;
   C_UNTAG   : constant Unsigned_8 := 12;
   C_TPDEF   : constant Unsigned_8 := 13;
   C_USTATIC : constant Unsigned_8 := 14;
   C_ENTAG   : constant Unsigned_8 := 15;
   C_MOE     : constant Unsigned_8 := 16;
   C_REGPARM : constant Unsigned_8 := 17;
   C_FIELD   : constant Unsigned_8 := 18;
   C_AUTOARG : constant Unsigned_8 := 19;
   C_LASTENT : constant Unsigned_8 := 20;
   C_BLOCK   : constant Unsigned_8 := 100;
   C_FCN     : constant Unsigned_8 := 101;
   C_EOS     : constant Unsigned_8 := 102;
   C_FILE    : constant Unsigned_8 := 103;
   C_LINE    : constant Unsigned_8 := 104;
   C_ALIAS   : constant Unsigned_8 := 105;
   C_HIDDEN  : constant Unsigned_8 := 106;
   C_EFCN    : constant Unsigned_8 := 255;

   --  Textual description of sclass.
   type Const_String_Acc is access constant String;
   type Sclass_Desc_Type is record
      Name : Const_String_Acc;
      Meaning : Const_String_Acc;
   end record;
   type Sclass_Desc_Array_Type is array (Unsigned_8) of Sclass_Desc_Type;
   Sclass_Desc : constant Sclass_Desc_Array_Type;

   type Auxent_File (Inline : Boolean := True) is record
      case Inline is
         when True =>
            X_Fname : String (1 .. 14);
         when False =>
            X_N : Strent_Type;
      end case;
   end record;
   pragma Unchecked_Union (Auxent_File);

   type Auxent_Scn is record
      X_Scnlen : Unsigned_32;
      X_Nreloc : Unsigned_16;
      X_Nlinno : Unsigned_16;
   end record;

   --  Relocation.
   type Reloc is record
      R_Vaddr : Unsigned_32;
      R_Symndx : Unsigned_32;
      R_Type : Unsigned_16;
   end record;
   Relsz : constant Natural := Reloc'Size / Storage_Unit;

   Reloc_Rel32  : constant Unsigned_16 := 20;
   Reloc_Addr32 : constant Unsigned_16 := 6;

private
   subtype S is String;
   Sclass_Desc : constant Sclass_Desc_Array_Type :=
     (C_NULL => (new S'("C_NULL"), new S'("No entry")),
      C_AUTO => (new S'("C_AUTO"), new S'("Automatic variable")),
      C_EXT => (new S'("C_EXT"), new S'("External/public symbol")),
      C_STAT => (new S'("C_STAT"), new S'("static (private) symbol")),
      C_REG => (new S'("C_REG"), new S'("register variable")),
      C_EXTDEF => (new S'("C_EXTDEF"), new S'("External definition")),
      C_LABEL => (new S'("C_LABEL"), new S'("label")),
      C_ULABEL => (new S'("C_ULABEL"), new S'("undefined label")),
      C_MOS => (new S'("C_MOS"), new S'("member of structure")),
      C_ARG => (new S'("C_ARG"), new S'("function argument")),
      C_STRTAG => (new S'("C_STRTAG"), new S'("structure tag")),
      C_MOU => (new S'("C_MOU"), new S'("member of union")),
      C_UNTAG => (new S'("C_UNTAG"), new S'("union tag")),
      C_TPDEF => (new S'("C_TPDEF"), new S'("type definition")),
      C_USTATIC => (new S'("C_USTATIC"), new S'("undefined static")),
      C_ENTAG => (new S'("C_ENTAG"), new S'("enumaration tag")),
      C_MOE => (new S'("C_MOE"), new S'("member of enumeration")),
      C_REGPARM => (new S'("C_REGPARM"), new S'("register parameter")),
      C_FIELD => (new S'("C_FIELD"), new S'("bit field")),
      C_AUTOARG => (new S'("C_AUTOARG"), new S'("auto argument")),
      C_LASTENT => (new S'("C_LASTENT"), new S'("dummy entry (end of block)")),
      C_BLOCK => (new S'("C_BLOCK"), new S'("beginning or end of block")),
      C_FCN => (new S'("C_FCN"), new S'("beginning or end of function")),
      C_EOS => (new S'("C_EOS"), new S'("end of structure")),
      C_FILE => (new S'("C_FILE"), new S'("file name")),
      C_LINE => (new S'("C_LINE"),
                 new S'("line number, reformatted as symbol")),
      C_ALIAS => (new S'("C_ALIAS"), new S'("duplicate tag")),
      C_HIDDEN => (new S'("C_HIDDEN"),
                   new S'("ext symbol in dmert public lib")),
      C_EFCN => (new S'("C_EFCN"), new S'("physical end of function")),
      others => (null, null));

end Coff;
