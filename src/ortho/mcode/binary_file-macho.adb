--  Binary file Mach-O writer.
--  Copyright (C) 2015 Tristan Gingold
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
with Macho; use Macho;
with Macho_Arch64; use Macho_Arch64;

package body Binary_File.Macho is
   procedure Write (Fd : GNAT.OS_Lib.File_Descriptor)
   is
      use GNAT.OS_Lib;

      --  If true, discard local symbols;
      Flag_Discard_Local : Boolean := True;

      procedure Xwrite (Data : System.Address; Len : Natural) is
      begin
         if Write (Fd, Data, Len) /= Len then
            raise Write_Error;
         end if;
      end Xwrite;

      function Symbol_Discarded (S : Symbol) return Boolean is
      begin
         case Get_Scope (S) is
            when Sym_Local =>
               if Flag_Discard_Local then
                  return True;
               end if;
            when Sym_Private =>
               null;
            when Sym_Global =>
               null;
            when Sym_Undef =>
               if not Get_Used (S) then
                  return True;
               end if;
         end case;
         return False;
      end Symbol_Discarded;

      procedure Fill_Name (Dest : out String; Src : String)
      is
         subtype D_Type is String (1 .. Dest'Length);
         D : D_Type renames Dest;
         subtype S_Type is String (1 .. Src'Length);
         S : S_Type renames Src;
      begin
         if S'Length < D'Length then
            D (1 .. S'Length) := S;
            D (S'Length + 1 .. D'Last) := (others => ASCII.NUL);
         else
            D := S (1 .. D'Last);
         end if;
      end Fill_Name;

      type Section_Info_Type is record
         Sect : Section_Acc;
         --  Index of the section symbol (in symtab).
      end record;
      type Section_Info_Array is array (Natural range <>) of Section_Info_Type;
      Sects_Info : Section_Info_Array (1 .. Nbr_Sections);
      type Section_Array is array (Natural range <>) of Section;
      Sects_Hdr : Section_Array (1 .. Nbr_Sections);
      Nbr_Sect : Natural;
      Sect : Section_Acc;

      --  Various offsets.
      File_Offset : Natural;
      Seg_Offset : Natural;
      Symtab_Offset : Natural;
      Strtab_Offset : Natural;
      Sizeof_Cmds : Natural;

      --  Number of symtab entries.
      Nbr_Symbols : Natural;

      Str_Size : Natural;

      --  If true, do local relocs.
      Flag_Reloc : constant Boolean := True;
   begin
      --  If relocations are not performs, then local symbols cannot be
      --  discarded.
      if not Flag_Reloc then
         Flag_Discard_Local := False;
      end if;

      --  Count sections.
      Sect := Section_Chain;
      Nbr_Sect := 0;
      while Sect /= null loop
         Nbr_Sect := Nbr_Sect + 1;
         Sects_Info (Nbr_Sect).Sect := Sect;
         Sect.Number := Nbr_Sect;
         Sect := Sect.Next;
      end loop;

      --  Set sections offset.
      Sizeof_Cmds := Lc_Size + Segment_Command_Size
        + Nbr_Sect * Section_Size
        + Lc_Size + Symtab_Command_Size;
      File_Offset := Header_Size + Sizeof_Cmds;
      Seg_Offset := File_Offset;
      for I in 1 .. Nbr_Sect loop
         Sect := Sects_Info (I).Sect;
         if Sect.Data /= null then
            --  FIXME: alignment ?
            Sects_Hdr (I).Offset := Unsigned_32 (File_Offset);
            File_Offset := File_Offset + Natural (Sect.Pc);
         else
            Sects_Hdr (I).Offset := 0;
         end if;
      end loop;

      --  Relocs
      --  FIXME: todo.

      Symtab_Offset := File_Offset;
      Str_Size := 0;
      Nbr_Symbols := 0;
      for I in Symbols.First .. Symbols.Last loop
         if not Symbol_Discarded (I) then
            Nbr_Symbols := Nbr_Symbols + 1;
            Set_Number (I, Nbr_Symbols);
            Str_Size := Str_Size + Get_Symbol_Name_Length (I) + 1;
         else
            Set_Number (I, 0);
         end if;
      end loop;

      File_Offset := File_Offset + Nbr_Symbols * Nlist_Size;
      Strtab_Offset := File_Offset;

      --  Write file header.
      declare
         Hdr : Header;
         Cputype : Unsigned_32;
      begin
         case Arch is
            when Arch_X86 =>
               Cputype := Cputype_I386;
            when Arch_X86_64 =>
               Cputype := Cputype_I386 + Cpu_Arch_64;
            when others =>
               raise Program_Error;
         end case;
         Hdr := (Magic => Magic,
                 Cputype => Cputype,
                 Cpusubtype => Cpusubtype_I386_All,
                 Filetype => Mh_Object,
                 Ncmds => 2,
                 Sizeofcmds => Unsigned_32 (Sizeof_Cmds),
                 others => 0);
         Xwrite (Hdr'Address, Header_Size);
      end;

      --  Write segment and section commands.
      declare
         Lc : Load_Command;
         Seg : Segment_Command;
      begin
         Lc := (Cmd => Lc_Segment,
                Cmdsize => Unsigned_32 (Lc_Size + Segment_Command_Size
                                          + Nbr_Sect * Section_Size));
         Xwrite (Lc'Address, Lc_Size);
         Seg := (Segname => (others => ASCII.NUL),
                 Vmaddr => 0,
                 Vmsize => 0, --  FIXME
                 Fileoff => Addr_T (Seg_Offset),
                 Filesize => Addr_T (Symtab_Offset - Seg_Offset),
                 Maxprot => 7, --  rwx
                 Initprot => 7,
                 Nsects => Unsigned_32 (Nbr_Sect),
                 Flags => 0);
         Xwrite (Seg'Address, Segment_Command_Size);
      end;

      --  Write section headers.
      for I in 1 .. Nbr_Sect loop
         Sect := Sects_Info (I).Sect;
         declare
            Hdr : Section renames Sects_Hdr (I);
            Secname_Raw : constant String := Sect.Name.all;
            subtype S_Type is String (1 .. Secname_Raw'Length);
            Secname : S_Type renames Secname_Raw;
         begin
            if Secname = ".text" then
               Fill_Name (Hdr.Sectname, "__text");
               Fill_Name (Hdr.Segname, "__TEXT");
            elsif Secname = ".rodata" then
               Fill_Name (Hdr.Sectname, "__const");
               Fill_Name (Hdr.Segname, "__TEXT");
            elsif (Sect.Flags and Section_Debug) /= 0 then
               if Secname'Length > 7
                 and then Secname (1 .. 7) = ".debug_"
               then
                  Fill_Name (Hdr.Sectname,
                             "__debug_" & Secname (8 .. Secname'Last));
               else
                  Fill_Name (Hdr.Sectname, Sect.Name.all);
               end if;
               Fill_Name (Hdr.Segname, "__DWARF");
            else
               Fill_Name (Hdr.Sectname, Secname);
               Fill_Name (Hdr.Segname, "");
            end if;
            Hdr.Addr := Addr_T (Sect.Vaddr);
            Hdr.Size := Addr_T (Sect.Pc);
            Hdr.Align := Unsigned_32 (Sect.Align);
            Hdr.Reloff := 0;
            Hdr.Nreloc := 0;
            Hdr.Flags := 0;
            Hdr.Reserved1 := 0;
            Hdr.Reserved2 := 0;
            Xwrite (Hdr'Address, Section_Size);
         end;
      end loop;

      --  Write symtab command
      declare
         Lc : Load_Command;
         Symtab : Symtab_Command;
      begin
         Lc := (Cmd => Lc_Symtab,
                Cmdsize => Unsigned_32 (Lc_Size + Symtab_Command_Size));
         Xwrite (Lc'Address, Lc_Size);
         Symtab := (Symoff => Unsigned_32 (Symtab_Offset),
                    Nsyms => Unsigned_32 (Nbr_Symbols),
                    Stroff => Unsigned_32 (Strtab_Offset),
                    Strsize => Unsigned_32 (Str_Size));
         Xwrite (Symtab'Address, Symtab_Command_Size);
      end;

      --  Write sections content.
      for I in 1 .. Nbr_Sect loop
         Sect := Sects_Info (I).Sect;
         if Sect.Data /= null then
            Xwrite (Sect.Data (0)'Address, Natural (Sect.Pc));
         end if;
      end loop;

      --  FIXME: write relocs.

      --   Write symbols.
      declare
         Str_Offset : Natural;

         generic
            with procedure Handle (S : Symbol);
         procedure Foreach_Symbol;

         procedure Foreach_Symbol is
         begin
            --  First, the local and private symbols.
            for I in Symbols.First .. Symbols.Last loop
               case Get_Scope (I) is
                  when Sym_Local =>
                     if not Flag_Discard_Local then
                        Handle (I);
                     end if;
                  when Sym_Private =>
                     Handle (I);
                  when Sym_Global
                    | Sym_Undef =>
                     null;
               end case;
            end loop;

            --  Then global symbols
            for I in Symbols.First .. Symbols.Last loop
               case Get_Scope (I) is
                  when Sym_Local
                    | Sym_Private =>
                     null;
                  when Sym_Global =>
                     Handle (I);
                  when Sym_Undef =>
                     null;
               end case;
            end loop;
            --  Then undef symbols.
            for I in Symbols.First .. Symbols.Last loop
               case Get_Scope (I) is
                  when Sym_Local
                    | Sym_Private =>
                     null;
                  when Sym_Global =>
                     null;
                  when Sym_Undef =>
                     if Get_Used (I) then
                        Handle (I);
                     end if;
               end case;
            end loop;
         end Foreach_Symbol;

         procedure Write_Symbol (S : Symbol)
         is
            Sym : Nlist;
         begin
            Sym := (N_Strx => Unsigned_32 (Str_Offset),
                    N_Type => 0,
                    N_Sect => 0,
                    N_Desc => 0,
                    N_Value => Addr_T (Get_Symbol_Value (S)));
            Str_Offset := Str_Offset + Get_Symbol_Name_Length (S) + 1;
            if Get_Scope (S) = Sym_Undef then
               Sym.N_Type := N_Undf;
            else
               if Get_Scope (S) = Sym_Global then
                  Sym.N_Type := N_Sect + N_Ext;
               else
                  Sym.N_Type := N_Sect;
               end if;
               Sym.N_Sect := Unsigned_8 (Get_Section (S).Number);
               Sym.N_Value := Sym.N_Value + Addr_T (Get_Section (S).Vaddr);
            end if;
            Xwrite (Sym'Address, Nlist_Size);
         end Write_Symbol;

         procedure Write_String (Sym : Symbol)
         is
            Str : constant String := Get_Symbol_Name (Sym) & ASCII.NUL;
         begin
            Xwrite (Str'Address, Str'Length);
         end Write_String;

         procedure Write_All_Symbols is new
           Foreach_Symbol (Write_Symbol);
         procedure Write_All_Strings is new
           Foreach_Symbol (Write_String);
      begin
         Str_Offset := 0;

         Write_All_Symbols;
         Write_All_Strings;
      end;
   end Write;

end Binary_File.Macho;
