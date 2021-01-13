--  COFF dumper.
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
with Interfaces; use Interfaces;
with System;
with Ada.Unchecked_Conversion;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Hex_Images; use Hex_Images;

procedure Coffdump is
   type Cstring is array (Unsigned_32 range <>) of Character;
   type Cstring_Acc is access Cstring;
   type Section_Array is array (Unsigned_16 range <>) of Scnhdr;
   type Section_Array_Acc is access Section_Array;
   --  Array of sections.
   Sections : Section_Array_Acc;

   type External_Symbol is array (0 .. Symesz - 1) of Character;
   type External_Symbol_Array is array (Unsigned_32 range <>)
     of External_Symbol;
   type Symbol_Array_Acc is access External_Symbol_Array;
   --  Symbols table.
   External_Symbols : Symbol_Array_Acc;

   --  String table.
   Str : Cstring_Acc;
   Str_Size : Natural;

   Hdr : Filehdr;
   --Sym : Syment;
   Fd : File_Descriptor;
   Skip : Natural;
   Skip_Kind : Unsigned_8;
   Aux_File : Auxent_File;
   Aux_Scn : Auxent_Scn;
   Rel : Reloc;
   Len : Natural;

   Nul : constant Character := Character'Val (0);

   function Find_Nul (S : String) return String is
   begin
      for I in S'Range loop
         if S (I) = Nul then
            return S (S'First .. I - 1);
         end if;
      end loop;
      return S;
   end Find_Nul;

   function Get_String (N : Strent_Type; S : String) return String
   is
   begin
      if N.E_Zeroes /= 0 then
         return Find_Nul (S);
      else
         for I in N.E_Offset .. Str'Last loop
            if Str (I) = Nul then
               return String (Str (N.E_Offset .. I - 1));
            end if;
         end loop;
         raise Program_Error;
      end if;
   end Get_String;

   procedure Memcpy
     (Dst : System.Address; Src : System.Address; Size : Natural);
   pragma Import (C, Memcpy);

   function Get_Section_Name (N : Unsigned_16) return String is
   begin
      if N = N_UNDEF then
         return "UNDEF";
      elsif N = N_ABS then
         return "ABS";
      elsif N = N_DEBUG then
         return "DEBUG";
      elsif N > Hdr.F_Nscns then
         return "???";
      else
         return Find_Nul (Sections (N).S_Name);
      end if;
   end Get_Section_Name;

   function Get_Symbol (N : Unsigned_32) return Syment is
      function Unchecked_Conv is new Ada.Unchecked_Conversion
        (Source => External_Symbol, Target => Syment);
   begin
      if N > Hdr.F_Nsyms then
         raise Constraint_Error;
      end if;
      return Unchecked_Conv (External_Symbols (N));
   end Get_Symbol;

   function Get_Symbol_Name (N : Unsigned_32) return String
   is
      S : Syment := Get_Symbol (N);
   begin
      return Get_String (S.E.E, S.E.E_Name);
   end Get_Symbol_Name;
begin
   for I in 1 .. Argument_Count loop
      Fd := Open_Read (Argument (I), Binary);
      if Fd = Invalid_FD then
         Put_Line ("cannot open " & Argument (I));
         return;
      end if;
      --  Read file header.
      if Read (Fd, Hdr'Address, Filehdr_Size) /= Filehdr_Size then
         Put_Line ("cannot read header");
         return;
      end if;
      Put_Line ("File: " & Argument (I));
      Put_Line ("magic:               " & Hex_Image (Hdr.F_Magic));
      Put_Line ("number of sections:  " & Hex_Image (Hdr.F_Nscns));
      Put_Line ("time and date stamp: " & Hex_Image (Hdr.F_Timdat));
      Put_Line ("symtab file pointer: " & Hex_Image (Hdr.F_Symptr));
      Put_Line ("nbr symtab entries:  " & Hex_Image (Hdr.F_Nsyms));
      Put_Line ("opt header size:     " & Hex_Image (Hdr.F_Opthdr));
      Put_Line ("flags:               " & Hex_Image (Hdr.F_Flags));

      --  Read sections header.
      Lseek (Fd, Long_Integer (Hdr.F_Opthdr), Seek_Cur);
      Sections := new Section_Array (1 .. Hdr.F_Nscns);
      Len := Scnhdr_Size * Natural (Hdr.F_Nscns);
      if Read (Fd, Sections (1)'Address, Len) /= Len then
         Put_Line ("cannot read section header");
         return;
      end if;
      for I in 1 .. Hdr.F_Nscns loop
         declare
            S: Scnhdr renames Sections (I);
         begin
            Put_Line ("Section " & Find_Nul (S.S_Name));
            Put_Line ("Physical address :     " & Hex_Image (S.S_Paddr));
            Put_Line ("Virtual address :      " & Hex_Image (S.S_Vaddr));
            Put_Line ("section size :         " & Hex_Image (S.S_Size));
            Put_Line ("section pointer :      " & Hex_Image (S.S_Scnptr));
            Put_Line ("relocation pointer :   " & Hex_Image (S.S_Relptr));
            Put_Line ("line num pointer :     " & Hex_Image (S.S_Lnnoptr));
            Put_Line ("Nbr reloc entries :    " & Hex_Image (S.S_Nreloc));
            Put_Line ("Nbr line num entries : " & Hex_Image (S.S_Nlnno));
            Put_Line ("Flags :                " & Hex_Image (S.S_Flags));
         end;
      end loop;

      --  Read string table.
      Lseek (Fd,
             Long_Integer (Hdr.F_Symptr + Hdr.F_Nsyms * Unsigned_32 (Symesz)),
             Seek_Set);
      if Read (Fd, Str_Size'Address, 4) /= 4 then
         Put_Line ("cannot read string table size");
         return;
      end if;
      Str := new Cstring (0 .. Unsigned_32 (Str_Size));
      if Read (Fd, Str (4)'Address, Str_Size - 4) /= Str_Size - 4 then
         Put_Line ("cannot read string table");
         return;
      end if;

      --  Read symbol table.
      Lseek (Fd, Long_Integer (Hdr.F_Symptr), Seek_Set);
      External_Symbols := new External_Symbol_Array (0 .. Hdr.F_Nsyms - 1);
      Len := Natural (Hdr.F_Nsyms) * Symesz;
      if Read (Fd, External_Symbols (0)'Address, Len) /= Len then
            Put_Line ("cannot read symbol");
            return;
         end if;

      Skip := 0;
      Skip_Kind := C_NULL;
      for I in External_Symbols'range loop
         if Skip > 0 then
            case Skip_Kind is
               when C_FILE =>
                  Memcpy (Aux_File'Address, External_Symbols (I)'Address,
                          Aux_File'Size / 8);
                  Put_Line ("aux file : " & Get_String (Aux_File.X_N,
                                                        Aux_File.X_Fname));
                  Skip_Kind := C_NULL;
               when C_STAT =>
                  Memcpy (Aux_Scn'Address, External_Symbols (I)'Address,
                          Aux_Scn'Size / 8);
                  Put_Line ("section len:   " & Hex_Image (Aux_Scn.X_Scnlen));
                  Put_Line ("nbr reloc ent: " & Hex_Image (Aux_Scn.X_Nreloc));
                  Put_Line ("nbr line num:  " & Hex_Image (Aux_Scn.X_Nlinno));
               when others =>
                  Put_Line ("skip");
            end case;
            Skip := Skip - 1;
         else
            declare
               S : Syment := Get_Symbol (I);
            begin
               Put_Line ("Symbol #" & Hex_Image (I));
               Put_Line ("symbol name : " & Get_Symbol_Name (I));
               Put_Line ("symbol value: " & Hex_Image (S.E_Value));
               Put_Line ("section num : " & Hex_Image (S.E_Scnum)
                         & "  " & Get_Section_Name (S.E_Scnum));
               Put_Line ("type        : " & Hex_Image (S.E_Type));
               Put      ("sclass      : " & Hex_Image (S.E_Sclass));
               if Sclass_Desc (S.E_Sclass).Name /= null then
                  Put ("  (");
                  Put (Sclass_Desc (S.E_Sclass).Name.all);
                  Put (" - ");
                  Put (Sclass_Desc (S.E_Sclass).Meaning.all);
                  Put (")");
               end if;
               New_Line;
               Put_Line ("numaux      : " & Hex_Image (S.E_Numaux));
               if S.E_Numaux > 0 then
                  case S.E_Sclass is
                     when C_FILE =>
                        Skip_Kind := C_FILE;
                     when C_STAT =>
                        Skip_Kind := C_STAT;
                     when others =>
                        Skip_Kind := C_NULL;
                  end case;
               end if;
               Skip := Natural (S.E_Numaux);
            end;
         end if;
      end loop;

      --  Disp relocs.
      for I in 1 .. Hdr.F_Nscns loop
         if Sections (I).S_Nreloc > 0 then
            --  Read relocations.
            Put_Line ("Relocations for section " & Get_Section_Name (I));
            Lseek (Fd, Long_Integer (Sections (I).S_Relptr), Seek_Set);
            for J in 1 .. Sections (I).S_Nreloc loop
               if Read (Fd, Rel'Address, Relsz) /= Relsz then
                  Put_Line ("cannot read reloc");
                  return;
               end if;
               Put_Line ("reloc virtual addr: " & Hex_Image (Rel.R_Vaddr));
               Put_Line ("symbol index      : " & Hex_Image (Rel.R_Symndx)
                         & "  " & Get_Symbol_Name (Rel.R_Symndx));
               Put ("type of relocation: " & Hex_Image (Rel.R_Type));
               case Rel.R_Type is
                  when Reloc_Rel32 =>
                     Put (" RELOC_REL32");
                  when Reloc_Addr32 =>
                     Put (" RELOC_ADDR32");
                  when others =>
                     null;
               end case;
               New_Line;
            end loop;
         end if;
      end loop;

      Close (Fd);
   end loop;
end Coffdump;

