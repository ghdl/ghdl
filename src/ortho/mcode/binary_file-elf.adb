--  Binary file ELF writer.
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
with Ada.Text_IO; use Ada.Text_IO;
with Elf_Common;
with Elf32;
with Elf64;
with Elf_Arch;

package body Binary_File.Elf is
   NUL : Character renames ASCII.NUL;

   type Arch_Bool is array (Arch_Kind) of Boolean;
   Is_Rela : constant Arch_Bool := (Arch_Unknown => False,
                                    Arch_X86 => False,
                                    Arch_X86_64 => True,
                                    Arch_Sparc => True,
                                    Arch_Ppc => True);

   procedure Write (Fd : GNAT.OS_Lib.File_Descriptor)
   is
      use Elf_Common;
      use Elf_Arch;
      use GNAT.OS_Lib;

      procedure Xwrite (Data : System.Address; Len : Natural) is
      begin
         if Write (Fd, Data, Len) /= Len then
            raise Write_Error;
         end if;
      end Xwrite;

      procedure Check_File_Pos (Off : Elf_Off)
      is
         L : Long_Integer;
      begin
         L := File_Length (Fd);
         if L /= Long_Integer (Off) then
            Put_Line (Standard_Error, "check_file_pos error: expect "
                      & Elf_Off'Image (Off) & ", found "
                      & Long_Integer'Image (L));
            raise Write_Error;
         end if;
      end Check_File_Pos;

      function Sect_Align (V : Elf_Off) return Elf_Off
      is
         Tmp : Elf_Off;
      begin
         Tmp := V + 2 ** 2 - 1;
         return Tmp - (Tmp mod 2 ** 2);
      end Sect_Align;

      type Section_Info_Type is record
         Sect : Section_Acc;
         --  Index of the section symbol (in symtab).
         Sym : Elf_Word;
         --  Number of relocs to write.
         --Nbr_Relocs : Natural;
      end record;
      type Section_Info_Array is array (Natural range <>) of Section_Info_Type;
      Sections : Section_Info_Array (0 .. 3 + 2 * Nbr_Sections);
      type Elf_Shdr_Array is array (Natural range <>) of Elf_Shdr;
      Shdr : Elf_Shdr_Array (0 .. 3 + 2 * Nbr_Sections);
      Nbr_Sect : Natural;
      Sect : Section_Acc;

      --  The first 4 sections are always present.
      Sect_Null : constant Natural := 0;
      Sect_Shstrtab : constant Natural := 1;
      Sect_Symtab : constant Natural := 2;
      Sect_Strtab : constant Natural := 3;
      Sect_First : constant Natural := 4;

      Offset : Elf_Off;

      --  Size of a relocation entry.
      Rel_Size : Natural;

      --  If true, do local relocs.
      Flag_Reloc : constant Boolean := True;
      --  If true, discard local symbols;
      Flag_Discard_Local : Boolean := True;

      --  Number of symbols.
      Nbr_Symbols : Natural := 0;
   begin
      --  If relocations are not performs, then local symbols cannot be
      --  discarded.
      if not Flag_Reloc then
         Flag_Discard_Local := False;
      end if;

      --  Set size of a relocation entry.  This avoids severals conditionnal.
      if Is_Rela (Arch) then
         Rel_Size := Elf_Rela_Size;
      else
         Rel_Size := Elf_Rel_Size;
      end if;

      --  Set section header.

      --  SHT_NULL.
      Shdr (Sect_Null) :=
        Elf_Shdr'(Sh_Name => 0,
                  Sh_Type => SHT_NULL,
                  Sh_Flags => 0,
                  Sh_Addr => 0,
                  Sh_Offset => 0,
                  Sh_Size => 0,
                  Sh_Link => 0,
                  Sh_Info => 0,
                  Sh_Addralign => 0,
                  Sh_Entsize => 0);

      --  shstrtab.
      Shdr (Sect_Shstrtab) :=
        Elf_Shdr'(Sh_Name => 1,
                  Sh_Type => SHT_STRTAB,
                  Sh_Flags => 0,
                  Sh_Addr => 0,
                  Sh_Offset => 0,     --  Filled latter.
                  --  NUL: 1, .symtab: 8, .strtab: 8 and .shstrtab: 10.
                  Sh_Size => 1 + 10 + 8 + 8,
                  Sh_Link => 0,
                  Sh_Info => 0,
                  Sh_Addralign => 1,
                  Sh_Entsize => 0);

      --  Symtab
      Shdr (Sect_Symtab) :=
        Elf_Shdr'(Sh_Name => 11,
                  Sh_Type => SHT_SYMTAB,
                  Sh_Flags => 0,
                  Sh_Addr => 0,
                  Sh_Offset => 0,
                  Sh_Size => 0,
                  Sh_Link => Elf_Word (Sect_Strtab),
                  Sh_Info => 0, --  FIXME
                  Sh_Addralign => 4,
                  Sh_Entsize => Elf_Size (Elf_Sym_Size));

      --  strtab.
      Shdr (Sect_Strtab) :=
        Elf_Shdr'(Sh_Name => 19,
                  Sh_Type => SHT_STRTAB,
                  Sh_Flags => 0,
                  Sh_Addr => 0,
                  Sh_Offset => 0,
                  Sh_Size => 0,
                  Sh_Link => 0,
                  Sh_Info => 0,
                  Sh_Addralign => 1,
                  Sh_Entsize => 0);

      --  Fill sections.
      Sect := Section_Chain;
      Nbr_Sect := Sect_First;
      Nbr_Symbols := 1;
      while Sect /= null loop
         --  For Size to word conversion.
         pragma Warnings (Off);

         Sections (Nbr_Sect) := (Sect => Sect,
                                 Sym => Elf_Word (Nbr_Symbols));
         Nbr_Symbols := Nbr_Symbols + 1;
         Sect.Number := Nbr_Sect;

         Shdr (Nbr_Sect) :=
           Elf_Shdr'(Sh_Name => Elf_Word (Shdr (Sect_Shstrtab).Sh_Size),
                     Sh_Type => SHT_PROGBITS,
                     Sh_Flags => 0,
                     Sh_Addr => Elf_Addr (Sect.Vaddr),
                     Sh_Offset => 0,
                     Sh_Size => 0,
                     Sh_Link => 0,
                     Sh_Info => 0,
                     Sh_Addralign => 2 ** Sect.Align,
                     Sh_Entsize => Elf_Size (Sect.Esize));
         if Sect.Data = null then
            Shdr (Nbr_Sect).Sh_Type := SHT_NOBITS;
         end if;
         if (Sect.Flags and Section_Read) /= 0 then
            Shdr (Nbr_Sect).Sh_Flags :=
              Shdr (Nbr_Sect).Sh_Flags or SHF_ALLOC;
         end if;
         if (Sect.Flags and Section_Exec) /= 0 then
            Shdr (Nbr_Sect).Sh_Flags :=
              Shdr (Nbr_Sect).Sh_Flags or SHF_EXECINSTR;
         end if;
         if (Sect.Flags and Section_Write) /= 0 then
            Shdr (Nbr_Sect).Sh_Flags :=
              Shdr (Nbr_Sect).Sh_Flags or SHF_WRITE;
         end if;
         if Sect.Flags = Section_Strtab then
            Shdr (Nbr_Sect).Sh_Type := SHT_STRTAB;
            Shdr (Nbr_Sect).Sh_Addralign := 1;
            Shdr (Nbr_Sect).Sh_Entsize := 0;
         end if;

         Shdr (Sect_Shstrtab).Sh_Size := Shdr (Sect_Shstrtab).Sh_Size
           + Sect.Name'Length + 1;      -- 1 for Nul.

         Nbr_Sect := Nbr_Sect + 1;
         if Flag_Reloc then
            if Sect.First_Reloc /= null then
               Do_Intra_Section_Reloc (Sect);
            end if;
         end if;
         if Sect.First_Reloc /= null then
            --  Add a section for the relocs.
            Shdr (Nbr_Sect) := Elf_Shdr'
              (Sh_Name => Elf_Word (Shdr (Sect_Shstrtab).Sh_Size),
               Sh_Type => SHT_NULL,
               Sh_Flags => 0,
               Sh_Addr => 0,
               Sh_Offset => 0,
               Sh_Size => 0,
               Sh_Link => Elf_Word (Sect_Symtab),
               Sh_Info => Elf_Word (Nbr_Sect - 1),
               Sh_Addralign => 4,
               Sh_Entsize => Elf_Size (Rel_Size));

            if Is_Rela (Arch) then
               Shdr (Nbr_Sect).Sh_Type := SHT_RELA;
            else
               Shdr (Nbr_Sect).Sh_Type := SHT_REL;
            end if;
            Shdr (Sect_Shstrtab).Sh_Size := Shdr (Sect_Shstrtab).Sh_Size
              + Sect.Name'Length + 4        --  4 for ".rel"
              + Boolean'Pos (Is_Rela (Arch)) + 1; -- 1 for 'a', 1 for Nul.

            Nbr_Sect := Nbr_Sect + 1;
         end if;
         Sect := Sect.Next;

         pragma Warnings (On);
      end loop;

      --  Lay-out sections.
      Offset := Elf_Off (Elf_Ehdr_Size);

      --  Section table
      Offset := Offset + Elf_Off (Nbr_Sect * Elf_Shdr_Size);

      --  shstrtab.
      Shdr (Sect_Shstrtab).Sh_Offset := Offset;

      Offset := Sect_Align (Offset + Shdr (Sect_Shstrtab).Sh_Size);

      --  user-sections and relocation.
      for I in Sect_First .. Nbr_Sect - 1 loop
         Sect := Sections (I).Sect;
         if Sect /= null then
            Sect.Pc := Pow_Align (Sect.Pc, Sect.Align);
            Shdr (Sect.Number).Sh_Size := Elf_Size (Sect.Pc);
            if Sect.Data /= null then
               --  Set data offset.
               Shdr (Sect.Number).Sh_Offset := Offset;
               Offset := Offset + Shdr (Sect.Number).Sh_Size;

               --  Set relocs offset.
               if Sect.First_Reloc /= null then
                  Shdr (Sect.Number + 1).Sh_Offset := Offset;
                  Shdr (Sect.Number + 1).Sh_Size :=
                    Elf_Size (Sect.Nbr_Relocs * Rel_Size);
                  Offset := Offset + Shdr (Sect.Number + 1).Sh_Size;
               end if;
            end if;
            --  Set link.
            if Sect.Link /= null then
               Shdr (Sect.Number).Sh_Link := Elf_Word (Sect.Link.Number);
            end if;
         end if;
      end loop;

      --  Number symbols, put local before globals.
      Nbr_Symbols := 1 + Nbr_Sections;

      --  First local symbols.
      for I in Symbols.First .. Symbols.Last loop
         case Get_Scope (I) is
            when Sym_Private =>
               Set_Number (I, Nbr_Symbols);
               Nbr_Symbols := Nbr_Symbols + 1;
            when Sym_Local =>
               if not Flag_Discard_Local then
                  Set_Number (I, Nbr_Symbols);
                  Nbr_Symbols := Nbr_Symbols + 1;
               end if;
            when Sym_Undef
              | Sym_Global =>
               null;
         end case;
      end loop;

      Shdr (Sect_Symtab).Sh_Info := Elf_Word (Nbr_Symbols);

      --  Then globals.
      for I in Symbols.First .. Symbols.Last loop
         case Get_Scope (I) is
            when Sym_Private
              | Sym_Local =>
               null;
            when Sym_Undef =>
               if Get_Used (I) then
                  Set_Number (I, Nbr_Symbols);
                  Nbr_Symbols := Nbr_Symbols + 1;
               end if;
            when Sym_Global =>
               Set_Number (I, Nbr_Symbols);
               Nbr_Symbols := Nbr_Symbols + 1;
         end case;
      end loop;

      --  Symtab.
      Shdr (Sect_Symtab).Sh_Offset := Offset;
      --  1 for nul.
      Shdr (Sect_Symtab).Sh_Size := Elf_Size (Nbr_Symbols * Elf_Sym_Size);

      Offset := Offset + Shdr (Sect_Symtab).Sh_Size;

      --  Strtab offset.
      Shdr (Sect_Strtab).Sh_Offset := Offset;
      Shdr (Sect_Strtab).Sh_Size := 1;

      --  Compute length of strtab.
      --    First, sections names.
      Sect := Section_Chain;
--       while Sect /= null loop
--          Shdr (Sect_Strtab).Sh_Size :=
--            Shdr (Sect_Strtab).Sh_Size + Sect.Name'Length + 1;
--          Sect := Sect.Prev;
--       end loop;
      --   Then symbols.
      declare
         Len : Natural;
         L : Natural;
      begin
         Len := 0;
         for I in Symbols.First .. Symbols.Last loop
            L := Get_Symbol_Name_Length (I) + 1;
            case Get_Scope (I) is
               when Sym_Local =>
                  if Flag_Discard_Local then
                     L := 0;
                  end if;
               when Sym_Private =>
                  null;
               when Sym_Global =>
                  null;
               when Sym_Undef =>
                  if not Get_Used (I) then
                     L := 0;
                  end if;
            end case;
            Len := Len + L;
         end loop;

         Shdr (Sect_Strtab).Sh_Size :=
           Shdr (Sect_Strtab).Sh_Size + Elf_Size (Len);
      end;

      --  Write file header.
      declare
         Ehdr : Elf_Ehdr;
      begin
         Ehdr := (E_Ident => (EI_MAG0 => ELFMAG0,
                              EI_MAG1 => ELFMAG1,
                              EI_MAG2 => ELFMAG2,
                              EI_MAG3 => ELFMAG3,
                              EI_CLASS => ELFCLASSNONE,
                              EI_DATA => ELFDATANONE,
                              EI_VERSION => EV_CURRENT,
                              EI_PAD .. 15 => 0),
                  E_Type => ET_REL,
                  E_Machine => EM_NONE,
                  E_Version => Elf_Word (EV_CURRENT),
                  E_Entry => 0,
                  E_Phoff => 0,
                  E_Shoff => Elf_Off (Elf_Ehdr_Size),
                  E_Flags => 0,
                  E_Ehsize => Elf_Half (Elf_Ehdr_Size),
                  E_Phentsize => 0,
                  E_Phnum => 0,
                  E_Shentsize => Elf_Half (Elf_Shdr_Size),
                  E_Shnum => Elf_Half (Nbr_Sect),
                  E_Shstrndx => 1);
         case Arch is
            when Arch_X86 =>
               Ehdr.E_Ident (EI_DATA) := ELFDATA2LSB;
               Ehdr.E_Ident (EI_CLASS) := ELFCLASS32;
               Ehdr.E_Machine := EM_386;
            when Arch_X86_64 =>
               Ehdr.E_Ident (EI_DATA) := ELFDATA2LSB;
               Ehdr.E_Ident (EI_CLASS) := ELFCLASS64;
               Ehdr.E_Machine := EM_X86_64;
            when Arch_Sparc =>
               Ehdr.E_Ident (EI_DATA) := ELFDATA2MSB;
               Ehdr.E_Ident (EI_CLASS) := ELFCLASS32;
               Ehdr.E_Machine := EM_SPARC;
            when others =>
               raise Program_Error;
         end case;
         Xwrite (Ehdr'Address, Elf_Ehdr_Size);
      end;

      -- Write shdr.
      Xwrite (Shdr'Address, Nbr_Sect * Elf_Shdr_Size);

      -- Write shstrtab
      Check_File_Pos (Shdr (Sect_Shstrtab).Sh_Offset);
      declare
         Str : String :=
           NUL & ".shstrtab" & NUL & ".symtab" & NUL & ".strtab" & NUL;
         Rela : String := NUL & ".rela";
      begin
         Xwrite (Str'Address, Str'Length);
         Sect := Section_Chain;
         while Sect /= null loop
            Xwrite (Sect.Name.all'Address, Sect.Name'Length);
            if Sect.First_Reloc /= null then
               if Is_Rela (Arch) then
                  Xwrite (Rela'Address, Rela'Length);
               else
                  Xwrite (Rela'Address, Rela'Length - 1);
               end if;
               Xwrite (Sect.Name.all'Address, Sect.Name'Length);
            end if;
            Xwrite (NUL'Address, 1);
            Sect := Sect.Next;
         end loop;
      end;
      --  Pad.
      declare
         Delt : Elf_Size;
         Nul_Str : String (1 .. 4) := (others => NUL);
      begin
         Delt := Shdr (Sect_Shstrtab).Sh_Size and 3;
         if Delt /= 0 then
            Xwrite (Nul_Str'Address, Natural (4 - Delt));
         end if;
      end;

      --  Write sections content and reloc.
      for I in 1 .. Nbr_Sect loop
         Sect := Sections (I).Sect;
         if Sect /= null then
            if Sect.Data /= null then
               Check_File_Pos (Shdr (Sect.Number).Sh_Offset);
               Xwrite (Sect.Data (0)'Address, Natural (Sect.Pc));
            end if;
            declare
               R : Reloc_Acc;
               Rel : Elf_Rel;
               Rela : Elf_Rela;
               S : Elf_Word;
               T : Elf_Word;
               Nbr_Reloc : Natural;
            begin
               R := Sect.First_Reloc;
               Nbr_Reloc := 0;
               while R /= null loop
                  if R.Done then
                     S := Sections (Get_Section (R.Sym).Number).Sym;
                  else
                     S := Elf_Word (Get_Number (R.Sym));
                  end if;

                  if Is_Rela (Arch) then
                     case Arch is
                        when Arch_X86_64 =>
                           case R.Kind is
                              when Reloc_Pc32 =>
                                 T := Elf64.R_X86_64_PC32;
                              when others =>
                                 raise Program_Error;
                           end case;
                        when Arch_Sparc =>
                           case R.Kind is
                              when Reloc_Disp22 =>
                                 T := Elf32.R_SPARC_WDISP22;
                              when Reloc_Disp30 =>
                                 T := Elf32.R_SPARC_WDISP30;
                              when Reloc_Hi22 =>
                                 T := Elf32.R_SPARC_HI22;
                              when Reloc_Lo10 =>
                                 T := Elf32.R_SPARC_LO10;
                              when Reloc_32 =>
                                 T := Elf32.R_SPARC_32;
                              when Reloc_Ua_32 =>
                                 T := Elf32.R_SPARC_UA32;
                              when others =>
                                 raise Program_Error;
                           end case;
                        when others =>
                           raise Program_Error;
                     end case;
                     Rela.R_Addend := 0;
                     Rela.R_Offset := Elf_Addr (R.Addr);
                     Rela.R_Info := Elf_R_Info (S, T);
                     Xwrite (Rela'Address, Elf_Rela_Size);
                  else
                     case R.Kind is
                        when Reloc_32 =>
                           T := Elf32.R_386_32;
                        when Reloc_Pc32 =>
                           T := Elf32.R_386_PC32;
                        when others =>
                           raise Program_Error;
                     end case;
                     Rel.R_Offset := Elf_Addr (R.Addr);
                     Rela.R_Info := Elf_R_Info (S, T);
                     Xwrite (Rel'Address, Elf_Rel_Size);
                  end if;
                  Nbr_Reloc := Nbr_Reloc + 1;
                  R := R.Sect_Next;
               end loop;
               if Nbr_Reloc /= Sect.Nbr_Relocs then
                  raise Program_Error;
               end if;
            end;
         end if;
      end loop;

      --  Write symbol table.
      Check_File_Pos (Shdr (Sect_Symtab).Sh_Offset);
      declare
         Str_Off : Elf_Off;

         procedure Gen_Sym (S : Symbol)
         is
            Sym : Elf_Sym;
            Bind : Elf_Uchar;
            Typ : Elf_Uchar;
         begin
            Sym := Elf_Sym'(St_Name => Elf_Word (Str_Off),
                            St_Value => Elf_Addr (Get_Symbol_Value (S)),
                            St_Size => 0,
                            St_Info => 0,
                            St_Other => 0,
                            St_Shndx => SHN_UNDEF);
            if Get_Section (S) /= null then
               Sym.St_Shndx := Elf_Half (Get_Section (S).Number);
            end if;
            case Get_Scope (S) is
               when Sym_Private
                 | Sym_Local =>
                  Bind := STB_LOCAL;
                  Typ := STT_NOTYPE;
               when Sym_Global =>
                  Bind := STB_GLOBAL;
                  if Get_Section (S) /= null
                    and then (Get_Section (S).Flags and Section_Exec) /= 0
                  then
                     Typ := STT_FUNC;
                  else
                     Typ := STT_OBJECT;
                  end if;
               when Sym_Undef =>
                  Bind := STB_GLOBAL;
                  Typ := STT_NOTYPE;
            end case;
            Sym.St_Info := Elf_St_Info (Bind, Typ);

            Xwrite (Sym'Address, Elf_Sym_Size);

            Str_Off := Str_Off + Elf_Off (Get_Symbol_Name_Length (S) + 1);
         end Gen_Sym;

         Sym : Elf_Sym;
      begin

         Str_Off := 1;

         --   write null entry
         Sym := Elf_Sym'(St_Name => 0,
                         St_Value => 0,
                         St_Size => 0,
                         St_Info => 0,
                         St_Other => 0,
                         St_Shndx => SHN_UNDEF);
         Xwrite (Sym'Address, Elf_Sym_Size);

         --   write section entries
         Sect := Section_Chain;
         while Sect /= null loop
            Sym := Elf_Sym'(St_Name => 0,
                            St_Value => 0,
                            St_Size => 0,
                            St_Info => Elf_St_Info (STB_LOCAL, STT_SECTION),
                            St_Other => 0,
                            St_Shndx => Elf_Half (Sect.Number));
            Xwrite (Sym'Address, Elf_Sym_Size);
            Sect := Sect.Next;
         end loop;

         --  First local symbols.
         for I in Symbols.First .. Symbols.Last loop
            case Get_Scope (I) is
               when Sym_Private =>
                  Gen_Sym (I);
               when Sym_Local =>
                  if not Flag_Discard_Local then
                     Gen_Sym (I);
                  end if;
               when Sym_Global
                 | Sym_Undef =>
                  null;
            end case;
         end loop;

         --  Then global symbols.
         for I in Symbols.First .. Symbols.Last loop
            case Get_Scope (I) is
               when Sym_Global =>
                  Gen_Sym (I);
               when Sym_Undef =>
                  if Get_Used (I) then
                     Gen_Sym (I);
                  end if;
               when Sym_Private
                 | Sym_Local =>
                  null;
            end case;
         end loop;
      end;

      -- Write strtab.
      Check_File_Pos (Shdr (Sect_Strtab).Sh_Offset);
      --  First is NUL.
      Xwrite (NUL'Address, 1);

      --  Then the symbols name.
      declare
         procedure Write_Sym_Name (S : Symbol)
         is
            Str : String := Get_Symbol_Name (S) & NUL;
         begin
            Xwrite (Str'Address, Str'Length);
         end Write_Sym_Name;
      begin
         --  First locals.
         for I in Symbols.First .. Symbols.Last loop
            case Get_Scope (I) is
               when Sym_Private =>
                  Write_Sym_Name (I);
               when Sym_Local =>
                  if not Flag_Discard_Local then
                     Write_Sym_Name (I);
                  end if;
               when Sym_Global
                 | Sym_Undef =>
                  null;
            end case;
         end loop;

         --  Then global symbols.
         for I in Symbols.First .. Symbols.Last loop
            case Get_Scope (I) is
               when Sym_Global =>
                  Write_Sym_Name (I);
               when Sym_Undef =>
                  if Get_Used (I) then
                     Write_Sym_Name (I);
                  end if;
               when Sym_Private
                 | Sym_Local =>
                  null;
            end case;
         end loop;
      end;
   end Write;

end Binary_File.Elf;
