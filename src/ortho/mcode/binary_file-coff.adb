--  Binary file COFF writer.
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
with Coff; use Coff;

package body Binary_File.Coff is
   NUL : Character renames ASCII.NUL;

   procedure Write (Fd : GNAT.OS_Lib.File_Descriptor)
   is
      use GNAT.OS_Lib;

      procedure Xwrite (Data : System.Address; Len : Natural) is
      begin
         if Write (Fd, Data, Len) /= Len then
            raise Write_Error;
         end if;
      end Xwrite;

      type Section_Info_Type is record
         Sect : Section_Acc;
         --  File offset for the data.
         Data_Offset : Natural;
         --  File offset for the relocs.
         Reloc_Offset : Natural;
         --  Number of relocs to write.
         Nbr_Relocs : Natural;
      end record;
      type Section_Info_Array is array (Natural range <>) of Section_Info_Type;
      Sections : Section_Info_Array (1 .. Nbr_Sections + 3);
      Nbr_Sect : Natural;
      Sect_Text : constant Natural := 1;
      Sect_Data : constant Natural := 2;
      Sect_Bss : constant Natural := 3;
      Sect : Section_Acc;

      --Section_Align : constant Natural := 2;

      Offset : Natural;
      Symtab_Offset : Natural;
      --  Number of symtab entries.
      Nbr_Symbols : Natural;
      Strtab_Offset : Natural;

      function Gen_String (Str : String) return Sym_Name
      is
         Res : Sym_Name;
      begin
         if Str'Length <= 8 then
            Res.E_Name := (others => NUL);
            Res.E_Name (1 .. Str'Length) := Str;
         else
            Res.E := (E_Zeroes => 0, E_Offset => Unsigned_32 (Offset));
            Offset := Offset + Str'Length + 1;
         end if;
         return Res;
      end Gen_String;

      --  Well known sections name.
      type String_Array is array (Sect_Text .. Sect_Bss) of String (1 .. 8);
      Sect_Name : constant String_Array :=
        (Sect_Text => ".text" & NUL & NUL & NUL,
         Sect_Data => ".data" & NUL & NUL & NUL,
         Sect_Bss => ".bss" & NUL & NUL & NUL & NUL);
      type Unsigned32_Array is array (Sect_Text .. Sect_Bss) of Unsigned_32;
      Sect_Flags : constant Unsigned32_Array :=
        (Sect_Text => STYP_TEXT,
         Sect_Data => STYP_DATA,
         Sect_Bss => STYP_BSS);

      --  If true, do local relocs.
      Flag_Reloc : constant Boolean := True;
      --  If true, discard local symbols;
      Flag_Discard_Local : Boolean := True;

      --  Handle every symbol in the right order: first external, then local.
      generic
         with procedure Handle_Symbol (S : Symbol);
      procedure For_Each_Symbol;

      procedure For_Each_Symbol is
      begin
         for I in Symbols.First .. Symbols.Last loop
            if Get_Scope (I) in Symbol_Scope_External then
               Handle_Symbol (I);
            end if;
         end loop;
         --  Then the local symbols (2).
         if not Flag_Discard_Local then
            for I in Symbols.First .. Symbols.Last loop
               if Get_Scope (I) not in Symbol_Scope_External then
                  Handle_Symbol (I);
               end if;
            end loop;
         end if;
      end For_Each_Symbol;
   begin
      --  If relocations are not performs, then local symbols cannot be
      --  discarded.
      if not Flag_Reloc then
         Flag_Discard_Local := False;
      end if;

      --  Fill sections.
      Sect := Section_Chain;
      Nbr_Sect := 3;
      declare
         N : Natural;
      begin
         while Sect /= null loop
            if Sect.Name.all = ".text" then
               N := Sect_Text;
            elsif Sect.Name.all = ".data" then
               N := Sect_Data;
            elsif Sect.Name.all = ".bss" then
               N := Sect_Bss;
            else
               Nbr_Sect := Nbr_Sect + 1;
               N := Nbr_Sect;
            end if;
            Sections (N).Sect := Sect;
            Sect.Number := N;
            Sect := Sect.Next;
         end loop;
      end;

      --  Set data offset.
      Offset := Filehdr_Size + Nbr_Sect * Scnhdr_Size;
      for I in 1 .. Nbr_Sect loop
         if Sections (I).Sect /= null
           and then Sections (I).Sect.Data /= null
         then
            Sections (I).Data_Offset := Offset;
            Offset := Offset + Natural (Sections (I).Sect.Pc);
         else
            Sections (I).Data_Offset := 0;
         end if;
      end loop;

      --  Set relocs offset.
      declare
         Rel : Reloc_Acc;
      begin
         for I in 1 .. Nbr_Sect loop
            Sections (I).Nbr_Relocs := 0;
            if Sections (I).Sect /= null then
               Sections (I).Reloc_Offset := Offset;
               if not Flag_Reloc then
                  --  Do local relocations.
                  Rel := Sections (I).Sect.First_Reloc;
                  while Rel /= null loop
                     if S_Local (Rel.Sym) then
                        if Get_Section (Rel.Sym) = Sections (I).Sect
                        then
                           --  Intra section local reloc.
                           Apply_Reloc (Sections (I).Sect, Rel);
                        else
                           --  Inter section local reloc.
                           --  A relocation is still required.
                           Sections (I).Nbr_Relocs :=
                             Sections (I).Nbr_Relocs + 1;
                           --  FIXME: todo.
                           raise Program_Error;
                        end if;
                     else
                        Sections (I).Nbr_Relocs := Sections (I).Nbr_Relocs + 1;
                     end if;
                     Rel := Rel.Sect_Next;
                  end loop;
               else
                  Sections (I).Nbr_Relocs := Sections (I).Sect.Nbr_Relocs;
               end if;
               Offset := Offset + Sections (I).Nbr_Relocs * Relsz;
            else
               Sections (I).Reloc_Offset := 0;
            end if;
         end loop;
      end;

      Symtab_Offset := Offset;

      --  Add symbol table length.
      declare
         procedure Number_Symbol (S : Symbol) is
         begin
            Set_Number (S, Nbr_Symbols);
            Nbr_Symbols := Nbr_Symbols + 1;
         end Number_Symbol;

         procedure Number_Each_Symbol is
            new For_Each_Symbol (Number_Symbol);
      begin
         Nbr_Symbols := 2 + Nbr_Sect * 2; --  2 for file.
         Number_Each_Symbol;
         Offset := Offset + Nbr_Symbols * Symesz;
         Strtab_Offset := Offset;

         --  4 for strtab length.
         Offset := Offset + 4;
      end;

      --  Write file header.
      declare
         Hdr : Filehdr;
      begin
         if Arch = Arch_X86_64 then
            Hdr.F_Magic := X8664magic;
         else
            Hdr.F_Magic := I386magic;
         end if;
         Hdr.F_Nscns := Unsigned_16 (Nbr_Sect);
         Hdr.F_Timdat := 0;
         Hdr.F_Symptr := Unsigned_32 (Symtab_Offset);
         Hdr.F_Nsyms := Unsigned_32 (Nbr_Symbols);
         Hdr.F_Opthdr := 0;
         Hdr.F_Flags := F_Lnno;
         Xwrite (Hdr'Address, Filehdr_Size);
      end;

      --  Write sections header.
      for I in 1 .. Nbr_Sect loop
         declare
            Hdr : Scnhdr;
            L : Natural;
         begin
            case I is
               when Sect_Text
                 | Sect_Data
                 | Sect_Bss =>
                  Hdr.S_Name := Sect_Name (I);
                  Hdr.S_Flags := Sect_Flags (I);
               when others =>
                  Hdr.S_Flags := 0;
                  L := Sections (I).Sect.Name'Length;
                  if L > Hdr.S_Name'Length then
                     --  Truncate long sectio names
                     Hdr.S_Name := Sections (I).Sect.Name
                       (Sections (I).Sect.Name'First ..
                        Sections (I).Sect.Name'First + Hdr.S_Name'Length - 1);
                  else
                     Hdr.S_Name (1 .. L) := Sections (I).Sect.Name.all;
                     Hdr.S_Name (L + 1 .. Hdr.S_Name'Last) := (others => NUL);
                  end if;
            end case;
            Hdr.S_Paddr := 0;
            Hdr.S_Vaddr := 0;
            Hdr.S_Scnptr := Unsigned_32 (Sections (I).Data_Offset);
            Hdr.S_Relptr := Unsigned_32 (Sections (I).Reloc_Offset);
            Hdr.S_Lnnoptr := 0;
            Hdr.S_Nreloc := Unsigned_16 (Sections (I).Nbr_Relocs);
            if Sections (I).Sect /= null then
               Hdr.S_Size := Unsigned_32 (Sections (I).Sect.Pc);
            else
               Hdr.S_Size := 0;
            end if;
            Hdr.S_Nlnno := 0;
            Xwrite (Hdr'Address, Scnhdr_Size);
         end;
      end loop;

      --  Write sections content.
      for I in 1 .. Nbr_Sect loop
         if Sections (I).Sect /= null
           and then Sections (I).Sect.Data /= null
         then
            Xwrite (Sections (I).Sect.Data (0)'Address,
                    Natural (Sections (I).Sect.Pc));
         end if;
      end loop;

      --  Write sections reloc.
      for I in 1 .. Nbr_Sect loop
         if Sections (I).Sect /= null then
            declare
               R : Reloc_Acc;
               Rel : Reloc;
            begin
               R := Sections (I).Sect.First_Reloc;
               while R /= null loop
                  case R.Kind is
                     when Reloc_32 =>
                        Rel.R_Type := Reloc_Addr32;
                     when Reloc_Pc32 =>
                        Rel.R_Type := Reloc_Rel32;
                     when others =>
                        raise Program_Error;
                  end case;
                  Rel.R_Vaddr := Unsigned_32 (R.Addr);
                  Rel.R_Symndx := Unsigned_32 (Get_Number (R.Sym));
                  Xwrite (Rel'Address, Relsz);
                  R := R.Sect_Next;
               end loop;
            end;
         end if;
      end loop;

      --  Write symtab.
      --   Write file symbol + aux
      declare
         Sym : Syment;
         A_File : Auxent_File;
      begin
         Sym := (E => (Inline => True,
                       E_Name => ".file" & NUL & NUL & NUL),
                 E_Value => 0,
                 E_Scnum => N_DEBUG,
                 E_Type => 0,
                 E_Sclass => C_FILE,
                 E_Numaux => 1);
         Xwrite (Sym'Address, Symesz);
         A_File := (Inline => True,
                    X_Fname => "testfile.xxxxx");
         Xwrite (A_File'Address, Symesz);
      end;
      --   Write sections symbol + aux
      for I in 1 .. Nbr_Sect loop
         declare
            A_Scn : Auxent_Scn;
            Sym : Syment;
         begin
            Sym := (E => (Inline => True, E_Name => (others => NUL)),
                    E_Value => 0,
                    E_Scnum => Unsigned_16 (I),
                    E_Type => 0,
                    E_Sclass => C_STAT,
                    E_Numaux => 1);
            if I <= Sect_Bss then
               Sym.E.E_Name := Sect_Name (I);
            else
               Sym.E := Gen_String (Sections (I).Sect.Name.all);
            end if;
            Xwrite (Sym'Address, Symesz);
            if Sections (I).Sect /= null
              and then Sections (I).Sect.Data /= null
            then
               A_Scn :=
                 (X_Scnlen => Unsigned_32 (Sections (I).Sect.Pc),
                  X_Nreloc => Unsigned_16 (Sections (I).Nbr_Relocs),
                  X_Nlinno => 0);
            else
               A_Scn := (X_Scnlen => 0, X_Nreloc => 0, X_Nlinno => 0);
            end if;
            Xwrite (A_Scn'Address, Symesz);
         end;
      end loop;

      --   Write symbols.
      declare
         procedure Write_Symbol (S : Symbol)
         is
            Sym : Syment;
         begin
            Sym := (E => Gen_String (Get_Symbol_Name (S)),
                    E_Value => Unsigned_32
                       (Get_Symbol_Value (S) and 16#ffff_ffff#),
                    E_Scnum => 0,
                    E_Type => 0,
                    E_Sclass => C_EXT,
                    E_Numaux => 0);
            case Get_Scope (S) is
               when Sym_Local
                 | Sym_Private =>
                  Sym.E_Sclass := C_STAT;
               when Sym_Undef
                 | Sym_Global =>
                  Sym.E_Sclass := C_EXT;
            end case;
            if Get_Section (S) /= null then
               Sym.E_Scnum := Unsigned_16 (Get_Section (S).Number);
            end if;
            Xwrite (Sym'Address, Symesz);
         end Write_Symbol;

         procedure Write_Each_Symbol is
            new For_Each_Symbol (Write_Symbol);
      begin
         Write_Each_Symbol;
      end;

      --  Write strtab.
      --    Write strtab length.
      declare
         L : Unsigned_32;

         procedure Write_String (Str : String) is
         begin
            if Str (Str'Last) /= NUL then
               raise Program_Error;
            end if;
            if Str'Length <= 9 then
               return;
            end if;
            Xwrite (Str'Address, Str'Length);
            Strtab_Offset := Strtab_Offset + Str'Length;
         end Write_String;

         procedure Write_Symbol_String (S : Symbol)
         is
            Str : constant String := Get_Symbol_Name (S);
         begin
            Write_String (Str & NUL);
         end Write_Symbol_String;

         procedure Write_Each_Symbol_String is
            new For_Each_Symbol (Write_Symbol_String);
      begin
         L := Unsigned_32 (Offset - Strtab_Offset);
         Xwrite (L'Address, 4);

         --  Write section name string.
         for I in Sect_Bss + 1 .. Nbr_Sect loop
            if Sections (I).Sect /= null
              and then Sections (I).Sect.Name'Length > 8
            then
               Write_String (Sections (I).Sect.Name.all & NUL);
            end if;
         end loop;

         Write_Each_Symbol_String;

         if Strtab_Offset + 4 /= Offset then
            raise Program_Error;
         end if;
      end;
   end Write;

end Binary_File.Coff;
