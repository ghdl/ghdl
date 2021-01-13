--  ELF dumper (library).
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
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with Interfaces; use Interfaces;
with Hex_Images; use Hex_Images;
with Elf_Common; use Elf_Common;
with Dwarf;

package body Elfdumper is
   function Get_String (Strtab : Strtab_Type; N : Elf_Size) return String
   is
      E : Elf_Size;
   begin
      E := N;
      while Strtab.Base (E) /= Nul loop
         E := E + 1;
      end loop;
      if E = N then
         return "";
      else
         return String (Strtab.Base (N .. E - 1));
      end if;
   end Get_String;

   procedure Disp_Ehdr (Ehdr : Elf_Ehdr) is
   begin
      Put ("File class: ");
      case Ehdr.E_Ident (EI_CLASS) is
         when ELFCLASSNONE =>
            Put ("none");
         when ELFCLASS32 =>
            Put ("class_32");
         when ELFCLASS64 =>
            Put ("class_64");
         when others =>
            Put ("others");
      end case;
      New_Line;

      Put ("encoding  : ");
      case Ehdr.E_Ident (EI_DATA) is
         when ELFDATANONE =>
            Put ("none");
         when ELFDATA2LSB =>
            Put ("LSB byte order");
         when ELFDATA2MSB =>
            Put ("MSB byte order");
         when others =>
            Put ("unknown");
      end case;
      New_Line;

      Put ("version   : ");
      case Ehdr.E_Ident (EI_VERSION) is
         when EV_NONE =>
            Put ("none");
         when EV_CURRENT =>
            Put ("current (1)");
         when others =>
            Put ("future");
      end case;
      New_Line;

      if Ehdr.E_Ident (EI_CLASS) /= Elf_Arch_Class
--        or Ehdr.E_Ident (EI_DATA) /= ELFDATA2LSB
        or Ehdr.E_Ident (EI_VERSION) /= EV_CURRENT
      then
         Put_Line ("bad class/data encoding/version");
         return;
      end if;

      Put ("File type : ");
      case Ehdr.E_Type is
         when ET_NONE =>
            Put ("no file type");
         when ET_REL =>
            Put ("relocatable file");
         when ET_EXEC =>
            Put ("executable file");
         when ET_CORE =>
            Put ("core file");
         when ET_LOPROC .. ET_HIPROC =>
            Put ("processor-specific");
         when others =>
            Put ("unknown");
      end case;
      New_Line;

      Put ("machine   : ");
      case Ehdr.E_Machine is
         when EM_NONE =>
            Put ("no machine");
         when EM_M32 =>
            Put ("AT&T WE 32100");
         when EM_SPARC =>
            Put ("SPARC");
         when EM_386 =>
            Put ("Intel architecture");
         when EM_68K =>
            Put ("Motorola 68000");
         when EM_88K =>
            Put ("Motorola 88000");
         when EM_860 =>
            Put ("Intel 80860");
         when EM_MIPS =>
            Put ("MIPS RS3000 Big-Endian");
         when EM_MIPS_RS4_BE =>
            Put ("MIPS RS4000 Big-Endian");
         when others =>
            Put ("unknown");
      end case;
      New_Line;

      Put_Line ("Version   : " & Hex_Image (Ehdr.E_Version));
      Put_Line ("Phoff     : " & Hex_Image (Ehdr.E_Phoff));
      Put_Line ("Shoff     : " & Hex_Image (Ehdr.E_Shoff));
      Put_Line ("flags     : " & Hex_Image (Ehdr.E_Flags));
      Put_Line ("phentsize : " & Hex_Image (Ehdr.E_Ehsize));
      Put_Line ("phnum     : " & Hex_Image (Ehdr.E_Phentsize));
      Put_Line ("shentsize : " & Hex_Image (Ehdr.E_Shentsize));
      Put_Line ("shnum     : " & Hex_Image (Ehdr.E_Shnum));
      Put_Line ("shstrndx  : " & Hex_Image (Ehdr.E_Shstrndx));
   end Disp_Ehdr;

   function Get_Shdr_Type_Name (Stype : Elf_Word) return String is
   begin
      case Stype is
         when SHT_NULL =>
            return "NULL";
         when SHT_PROGBITS =>
            return "PROGBITS";
         when SHT_SYMTAB =>
            return "SYMTAB";
         when SHT_STRTAB =>
            return "STRTAB";
         when SHT_RELA =>
            return "RELA";
         when SHT_HASH =>
            return "HASH";
         when SHT_DYNAMIC =>
            return "DYNAMIC";
         when SHT_NOTE =>
            return "NOTE";
         when SHT_NOBITS =>
            return "NOBITS";
         when SHT_REL =>
            return "REL";
         when SHT_SHLIB =>
            return "SHLIB";
         when SHT_DYNSYM =>
            return "DYNSYM";
         when SHT_INIT_ARRAY =>
            return "INIT_ARRAY";
         when SHT_FINI_ARRAY =>
            return "FINI_ARRAY";
         when SHT_PREINIT_ARRAY =>
            return "PREINIT_ARRAY";
         when SHT_GROUP =>
            return "GROUP";
         when SHT_SYMTAB_SHNDX =>
            return "SYMTAB_SHNDX";
         when SHT_NUM =>
            return "NUM";
         when SHT_LOOS =>
            return "LOOS";
         when SHT_GNU_LIBLIST =>
            return "GNU_LIBLIST";
         when SHT_CHECKSUM =>
            return "CHECKSUM";
         when SHT_SUNW_Move =>
            return "SUNW_move";
         when SHT_SUNW_COMDAT =>
            return "SUNW_COMDAT";
         when SHT_SUNW_Syminfo =>
            return "SUNW_syminfo";
         when SHT_GNU_Verdef =>
            return "GNU_verdef";
         when SHT_GNU_Verneed =>
            return "GNU_verneed";
         when SHT_GNU_Versym =>
            return "GNU_versym";
         when SHT_LOPROC .. SHT_HIPROC =>
            return "Processor dependant";
         when SHT_LOUSER .. SHT_HIUSER =>
            return "User dependant";
         when others =>
            return "unknown";
      end case;
   end Get_Shdr_Type_Name;

   procedure Disp_Shdr (Shdr : Elf_Shdr; Sh_Strtab : Strtab_Type)
   is
   begin
      Put_Line ("name  : " & Hex_Image (Shdr.Sh_Name) & " """
                & Get_String (Sh_Strtab, Elf_Size (Shdr.Sh_Name)) & """");
      Put ("type  : " & Hex_Image (Shdr.Sh_Type) & " ");
      Put (Get_Shdr_Type_Name (Shdr.Sh_Type));
      New_Line;
      Put ("flags : " & Hex_Image (Shdr.Sh_Flags));
      if (Shdr.Sh_Flags and SHF_WRITE) /= 0 then
         Put (" WRITE");
      end if;
      if (Shdr.Sh_Flags and SHF_ALLOC) /= 0 then
         Put (" ALLOC");
      end if;
      if (Shdr.Sh_Flags and SHF_EXECINSTR) /= 0 then
         Put (" EXEC");
      end if;
      New_Line;
      Put ("addr  : " & Hex_Image (Shdr.Sh_Addr));
      Put ("  offset : " & Hex_Image (Shdr.Sh_Offset));
      Put ("       size : " & Hex_Image (Shdr.Sh_Size));
      New_Line;
      Put ("link  : " & Hex_Image (Shdr.Sh_Link));
      Put ("    info : " & Hex_Image (Shdr.Sh_Info));
      Put ("  addralign : " & Hex_Image (Shdr.Sh_Addralign));
      Put ("  entsize : " & Hex_Image (Shdr.Sh_Entsize));
      New_Line;
   end Disp_Shdr;

   procedure Disp_Sym (File : Elf_File;
                       Sym : Elf_Sym;
                       Strtab : Strtab_Type)
   is
   begin
      Put (Hex_Image (Sym.St_Value));
      Put (" " & Hex_Image (Sym.St_Size));
      Put (' ');
      --Put ("  info:" & Hex_Image (Sym.St_Info) & " ");
      case Elf_St_Bind (Sym.St_Info) is
         when STB_LOCAL =>
            Put ("loc ");
         when STB_GLOBAL =>
            Put ("glob");
         when STB_WEAK =>
            Put ("weak");
         when others =>
            Put ("?   ");
      end case;
      Put (' ');
      case Elf_St_Type (Sym.St_Info) is
         when STT_NOTYPE =>
            Put ("none");
         when STT_OBJECT =>
            Put ("obj ");
         when STT_FUNC =>
            Put ("func");
         when STT_SECTION =>
            Put ("sect");
         when STT_FILE =>
            Put ("file");
         when others =>
            Put ("?   ");
      end case;
      --Put ("  other:" & Hex_Image (Sym.St_Other));
      Put (' ');
      case Sym.St_Shndx is
         when SHN_UNDEF =>
            Put ("UNDEF   ");
         when 1 .. SHN_LORESERVE - 1 =>
            declare
               S : String := Get_Section_Name (File, Sym.St_Shndx);
               Max : constant Natural := 8;
            begin
               if S'Length <= Max then
                  Put (S);
                  for I in S'Length + 1 .. Max loop
                     Put (' ');
                  end loop;
               else
                  Put (S (S'First .. S'First + Max - 1));
               end if;
            end;
         when SHN_LOPROC .. SHN_HIPROC =>
            Put ("*proc*  ");
         when SHN_ABS =>
            Put ("*ABS*   ");
         when SHN_COMMON =>
            Put ("*COMMON*");
         when others =>
            Put ("??      ");
      end case;
      --Put (" sect:" & Hex_Image (Sym.St_Shndx));
      Put (' ');
      Put_Line (Get_String (Strtab, Elf_Size (Sym.St_Name)));
   end Disp_Sym;

   function Get_Offset (File : Elf_File; Off : Elf_Off; Size : Elf_Size)
                       return Address
   is
   begin
      if Off > File.Length or Off + Size > File.Length then
         return Null_Address;
      end if;
      return File.Base + Storage_Offset (Off);
   end Get_Offset;

   function Get_Section_Base (File : Elf_File; Shdr : Elf_Shdr)
                             return Address
   is
   begin
      return Get_Offset (File, Shdr.Sh_Offset, Shdr.Sh_Size);
   end Get_Section_Base;

   function Get_Section_Base (File : Elf_File; Index : Elf_Half)
                             return Address
   is
      Shdr : Elf_Shdr_Acc;
   begin
      Shdr := Get_Shdr (File, Index);
      return Get_Section_Base (File, Shdr.all);
   end Get_Section_Base;

   function Get_Segment_Base (File : Elf_File; Phdr : Elf_Phdr)
                             return Address
   is
   begin
      return Get_Offset (File, Phdr.P_Offset, Phdr.P_Filesz);
   end Get_Segment_Base;

   function Get_Segment_Base (File : Elf_File; Index : Elf_Half)
                             return Address
   is
      Phdr : Elf_Phdr_Acc;
   begin
      Phdr := Get_Phdr (File, Index);
      return Get_Segment_Base (File, Phdr.all);
   end Get_Segment_Base;

   procedure Open_File (File : out Elf_File; Filename : String)
   is
      function Malloc (Size : Integer) return Address;
      pragma Import (C, Malloc);

      use GNAT.OS_Lib;
      Length : Long_Integer;
      Len : Integer;
      Fd : File_Descriptor;
   begin
      File := (Filename => new String'(Filename),
               Status => Status_Ok,
               Length => 0,
               Base => Null_Address,
               Ehdr => null,
               Shdr_Base => Null_Address,
               Sh_Strtab => (null, 0),
               Phdr_Base => Null_Address);

      --  Open the file.
      Fd := Open_Read (Filename, Binary);
      if Fd = Invalid_FD then
         File.Status := Status_Open_Failure;
         return;
      end if;

      --  Get length.
      Length := File_Length (Fd);
      Len := Integer (Length);
      if Len < Elf_Ehdr_Size then
         File.Status := Status_Bad_File;
         Close (Fd);
         return;
      end if;

      File.Length := Elf_Off (Len);

      --  Allocate memory for the file.
      File.Base := Malloc (Len);
      if File.Base = Null_Address then
         File.Status := Status_Memory;
         Close (Fd);
         return;
      end if;

      --  Read the whole file.
      if Read (Fd, File.Base, Integer (Length)) /= Integer (Length) then
         File.Status := Status_Read_Error;
         Close (Fd);
         return;
      end if;

      Close (Fd);

      File.Ehdr := To_Elf_Ehdr_Acc (File.Base);

      if File.Ehdr.E_Ident (EI_MAG0) /= ELFMAG0
        or File.Ehdr.E_Ident (EI_MAG1) /= ELFMAG1
        or File.Ehdr.E_Ident (EI_MAG2) /= ELFMAG2
        or File.Ehdr.E_Ident (EI_MAG3) /= ELFMAG3
      then
         File.Status := Status_Bad_Magic;
         return;
      end if;

      if File.Ehdr.E_Ident (EI_CLASS) /= Elf_Arch_Class
--        or Ehdr.E_Ident (EI_DATA) /= ELFDATA2LSB
        or File.Ehdr.E_Ident (EI_VERSION) /= EV_CURRENT
      then
         File.Status := Status_Bad_Class;
         return;
      end if;
   end Open_File;

   function Get_Status (File : Elf_File) return Elf_File_Status is
   begin
      return File.Status;
   end Get_Status;

   function Get_Ehdr (File : Elf_File) return Elf_Ehdr_Acc is
   begin
      return File.Ehdr;
   end Get_Ehdr;

   function Get_Shdr (File : Elf_File; Index : Elf_Half)
                     return Elf_Shdr_Acc
   is
   begin
      if Index >= File.Ehdr.E_Shnum then
         raise Constraint_Error;
      end if;
      return To_Elf_Shdr_Acc
        (File.Shdr_Base
         + Storage_Offset (Index * Elf_Half (Elf_Shdr_Size)));
   end Get_Shdr;

   procedure Load_Phdr (File : in out Elf_File)
   is
   begin
      if Get_Ehdr (File).E_Phentsize /= Elf_Half (Elf_Phdr_Size) then
         return;
      end if;

      File.Phdr_Base :=
        Get_Offset (File, Get_Ehdr (File).E_Phoff,
                    Elf_Size (Get_Ehdr (File).E_Phnum
                              * Elf_Half (Elf_Phdr_Size)));
   end Load_Phdr;

   function Get_Phdr (File : Elf_File; Index : Elf_Half)
                     return Elf_Phdr_Acc
   is
   begin
      if Index >= File.Ehdr.E_Phnum then
         raise Constraint_Error;
      end if;
      return To_Elf_Phdr_Acc
        (File.Phdr_Base
         + Storage_Offset (Index * Elf_Half (Elf_Phdr_Size)));
   end Get_Phdr;

   function Get_Strtab (File : Elf_File; Index : Elf_Half)
                       return Strtab_Type
   is
      Shdr : Elf_Shdr_Acc;
   begin
      Shdr := Get_Shdr (File, Index);
      if Shdr = null or Shdr.Sh_Type /= SHT_STRTAB then
         return Null_Strtab;
      end if;
      return (Base => To_Strtab_Fat_Acc (Get_Section_Base (File, Shdr.all)),
              Length => Shdr.Sh_Size);
   end Get_Strtab;

   procedure Load_Shdr (File : in out Elf_File)
   is
   begin
      if Get_Ehdr (File).E_Shentsize /= Elf_Half (Elf_Shdr_Size) then
         return;
      end if;

      File.Shdr_Base :=
        Get_Offset (File, Get_Ehdr (File).E_Shoff,
                    Elf_Size (Get_Ehdr (File).E_Shnum
                              * Elf_Half (Elf_Shdr_Size)));
      File.Sh_Strtab := Get_Strtab (File, Get_Ehdr (File).E_Shstrndx);
   end Load_Shdr;

   function Get_Sh_Strtab (File : Elf_File) return Strtab_Type is
   begin
      return File.Sh_Strtab;
   end Get_Sh_Strtab;

   function Get_Section_Name (File : Elf_File; Index : Elf_Half)
                             return String
   is
   begin
      return Get_String (Get_Sh_Strtab (File),
                         Elf_Size (Get_Shdr (File, Index).Sh_Name));
   end Get_Section_Name;

   function Get_Section_By_Name (File : Elf_File; Name : String)
                                return Elf_Half
   is
      Ehdr : Elf_Ehdr_Acc;
      Shdr : Elf_Shdr_Acc;
      Sh_Strtab : Strtab_Type;
   begin
      Ehdr := Get_Ehdr (File);
      Sh_Strtab := Get_Sh_Strtab (File);
      for I in 1 .. Ehdr.E_Shnum - 1 loop
         Shdr := Get_Shdr (File, I);
         if Get_String (Sh_Strtab, Elf_Size (Shdr.Sh_Name)) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Get_Section_By_Name;

   procedure Disp_Symtab (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      S_Strtab : Strtab_Type;
      Base : Address;
      Off : Storage_Offset;
   begin
      Shdr := Get_Shdr (File, Index);
      if Shdr.Sh_Entsize /= Elf_Size (Elf_Sym_Size) then
         return;
      end if;
      S_Strtab := Get_Strtab (File, Elf_Half (Shdr.Sh_Link));
      Base := Get_Section_Base (File, Shdr.all);
      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Disp_Sym (File, To_Elf_Sym_Acc (Base + Off).all, S_Strtab);
         Off := Off + Storage_Offset (Elf_Sym_Size);
      end loop;
   end Disp_Symtab;

   procedure Disp_Strtab (File : Elf_File; Index : Elf_Half)
   is
      Strtab : Strtab_Type;
      S, E : Elf_Size;
   begin
      Strtab := Get_Strtab (File, Index);
      S := 1;
      while S < Strtab.Length loop
         E := S;
         while Strtab.Base (E) /= Nul loop
            E := E + 1;
         end loop;
         Put_Line (Hex_Image (S) & ": "
                   & String (Strtab.Base (S .. E - 1)));
         S := E + 1;
      end loop;
   end Disp_Strtab;

   function Read_Byte (Addr : Address) return Unsigned_8
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      return To_Unsigned_8_Acc (Addr).all;
   end Read_Byte;

   procedure Read_ULEB128 (Base : Address;
                           Off : in out Storage_Offset;
                           Res : out Unsigned_32)
   is
      B : Unsigned_8;
      Shift : Integer;
   begin
      Res := 0;
      Shift := 0;
      loop
         B := Read_Byte (Base + Off);
         Off := Off + 1;
         Res := Res or Shift_Left (Unsigned_32 (B and 16#7f#), Shift);
         exit when (B and 16#80#) = 0;
         Shift := Shift + 7;
      end loop;
   end Read_ULEB128;

   procedure Read_SLEB128 (Base : Address;
                           Off : in out Storage_Offset;
                           Res : out Unsigned_32)
   is
      B : Unsigned_8;
      Shift : Integer;
   begin
      Res := 0;
      Shift := 0;
      loop
         B := Read_Byte (Base + Off);
         Off := Off + 1;
         Res := Res or Shift_Left (Unsigned_32 (B and 16#7f#), Shift);
         Shift := Shift + 7;
         exit when (B and 16#80#) = 0;
      end loop;
      if Shift < 32 and (Res and Shift_Left (1, Shift - 1)) /= 0 then
         Res := Res or Shift_Left (-1, Shift);
      end if;
   end Read_SLEB128;

   procedure Read_Word4 (Base : Address;
                         Off : in out Storage_Offset;
                         Res : out Unsigned_32)
   is
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Base + Off + 0);
      B1 := Read_Byte (Base + Off + 1);
      B2 := Read_Byte (Base + Off + 2);
      B3 := Read_Byte (Base + Off + 3);
      Res := Shift_Left (Unsigned_32 (B3), 24)
        or Shift_Left (Unsigned_32 (B2), 16)
        or Shift_Left (Unsigned_32 (B1), 8)
        or Shift_Left (Unsigned_32 (B0), 0);
      Off := Off + 4;
   end Read_Word4;

   procedure Read_Word2 (Base : Address;
                         Off : in out Storage_Offset;
                         Res : out Unsigned_16)
   is
      B0, B1 : Unsigned_8;
   begin
      B0 := Read_Byte (Base + Off + 0);
      B1 := Read_Byte (Base + Off + 1);
      Res := Shift_Left (Unsigned_16 (B1), 8)
        or Shift_Left (Unsigned_16 (B0), 0);
      Off := Off + 2;
   end Read_Word2;

   procedure Read_Byte (Base : Address;
                        Off : in out Storage_Offset;
                        Res : out Unsigned_8)
   is
   begin
      Res := Read_Byte (Base + Off);
      Off := Off + 1;
   end Read_Byte;

   procedure Disp_Note (Base : Address; Size : Storage_Offset)
   is
      Off : Storage_Offset;
      Namesz : Unsigned_32;
      Descsz : Unsigned_32;
      Ntype : Unsigned_32;
      B : Unsigned_8;
      Is_Full : Boolean;
   begin
      Off := 0;
      while Off < Size loop
         Read_Word4 (Base, Off, Namesz);
         Read_Word4 (Base, Off, Descsz);
         Read_Word4 (Base, Off, Ntype);
         Put ("type : ");
         Put (Hex_Image (Ntype));
         New_Line;
         Put ("name : ");
         Put (Hex_Image (Namesz));
         Put ("  ");
         for I in 1 .. Namesz loop
            Read_Byte (Base, Off, B);
            if B /= 0 then
               Put (Character'Val (B));
            end if;
         end loop;
         if Namesz mod 4 /= 0 then
            for I in (Namesz mod 4) .. 3 loop
               Read_Byte (Base, Off, B);
            end loop;
         end if;
         New_Line;
         Put ("desc : ");
         Put (Hex_Image (Descsz));
         Put (" ");
         Is_Full := Descsz >= 20;
         for I in 1 .. Descsz loop
            if Is_Full and (I mod 16) = 1 then
               New_Line;
            end if;
            Read_Byte (Base, Off, B);
            Put (' ');
            Put (Hex_Image (B));
         end loop;
         if Descsz mod 4 /= 0 then
            for I in (Descsz mod 4) .. 3 loop
               Read_Byte (Base, Off, B);
            end loop;
         end if;
         New_Line;
      end loop;
   end Disp_Note;

   procedure Disp_Section_Note (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);
      Disp_Note (Base, Storage_Offset (Shdr.Sh_Size));
   end Disp_Section_Note;

   procedure Disp_Segment_Note (File : Elf_File; Index : Elf_Half)
   is
      Phdr : Elf_Phdr_Acc;
      Base : Address;
   begin
      Phdr := Get_Phdr (File, Index);
      Base := Get_Segment_Base (File, Phdr.all);
      Disp_Note (Base, Storage_Offset (Phdr.P_Filesz));
   end Disp_Segment_Note;


   function Get_Dt_Name (Name : Elf_Word) return String is
   begin
      case Name is
         when DT_NULL =>
            return "NULL";
         when DT_NEEDED =>
            return "NEEDED";
         when DT_PLTRELSZ =>
            return "PLTRELSZ";
         when DT_PLTGOT =>
            return "PLTGOT";
         when DT_HASH =>
            return "HASH";
         when DT_STRTAB =>
            return "STRTAB";
         when DT_SYMTAB =>
            return "SYMTAB";
         when DT_RELA =>
            return "RELA";
         when DT_RELASZ =>
            return "RELASZ";
         when DT_RELAENT =>
            return "RELAENT";
         when DT_STRSZ =>
            return "STRSZ";
         when DT_SYMENT =>
            return "SYMENT";
         when DT_INIT =>
            return "INIT";
         when DT_FINI =>
            return "FINI";
         when DT_SONAME =>
            return "SONAME";
         when DT_RPATH =>
            return "RPATH";
         when DT_SYMBOLIC =>
            return "SYMBOLIC";
         when DT_REL =>
            return "REL";
         when DT_RELSZ =>
            return "RELSZ";
         when DT_RELENT =>
            return "RELENT";
         when DT_PLTREL =>
            return "PLTREL";
         when DT_DEBUG =>
            return "DEBUG";
         when DT_TEXTREL =>
            return "TEXTREL";
         when DT_JMPREL =>
            return "JMPREL";
         when DT_BIND_NOW =>
            return "BIND_NOW";
         when DT_INIT_ARRAY =>
            return "INIT_ARRAY";
         when DT_FINI_ARRAY =>
            return "FINI_ARRAY";
         when DT_INIT_ARRAYSZ =>
            return "INIT_ARRAYSZ";
         when DT_FINI_ARRAYSZ =>
            return "FINI_ARRAYSZ";
         when DT_RUNPATH =>
            return "RUNPATH";
         when DT_FLAGS =>
            return "FLAGS";
--           when DT_ENCODING =>
--              return "ENCODING";
         when DT_PREINIT_ARRAY =>
            return "PREINIT_ARRAY";
         when DT_PREINIT_ARRAYSZ =>
            return "PREINIT_ARRAYSZ";
         when DT_NUM =>
            return "NUM";
         when DT_LOOS =>
            return "LOOS";
--           when DT_HIOS =>
--              return "HIOS";
         when DT_LOPROC =>
            return "LOPROC";
--           when DT_HIPROC =>
--              return "HIPROC";
         when DT_VALRNGLO =>
            return "VALRNGLO";
         when DT_GNU_PRELINKED =>
            return "GNU_PRELINKED";
         when DT_GNU_CONFLICTSZ =>
            return "GNU_CONFLICTSZ";
         when DT_GNU_LIBLISTSZ =>
            return "GNU_LIBLISTSZ";
         when DT_CHECKSUM =>
            return "CHECKSUM";
         when DT_PLTPADSZ =>
            return "PLTPADSZ";
         when DT_MOVEENT =>
            return "MOVEENT";
         when DT_MOVESZ =>
            return "MOVESZ";
         when DT_FEATURE_1 =>
            return "FEATURE_1";
         when DT_POSFLAG_1 =>
            return "POSFLAG_1";
         when DT_SYMINSZ =>
            return "SYMINSZ";
         when DT_SYMINENT =>
            return "SYMINENT";
--           when DT_VALRNGHI =>
--              return "VALRNGHI";
         when DT_ADDRRNGLO =>
            return "ADDRRNGLO";
         when DT_GNU_CONFLICT =>
            return "GNU_CONFLICT";
         when DT_GNU_LIBLIST =>
            return "GNU_LIBLIST";
         when DT_CONFIG =>
            return "CONFIG";
         when DT_DEPAUDIT =>
            return "DEPAUDIT";
         when DT_AUDIT =>
            return "AUDIT";
         when DT_PLTPAD =>
            return "PLTPAD";
         when DT_MOVETAB =>
            return "MOVETAB";
         when DT_SYMINFO =>
            return "SYMINFO";
--           when DT_ADDRRNGHI =>
--              return "ADDRRNGHI";
         when DT_VERSYM =>
            return "VERSYM";
         when DT_RELACOUNT =>
            return "RELACOUNT";
         when DT_RELCOUNT =>
            return "RELCOUNT";
         when DT_FLAGS_1 =>
            return "FLAGS_1";
         when DT_VERDEF =>
            return "VERDEF";
         when DT_VERDEFNUM =>
            return "VERDEFNUM";
         when DT_VERNEED =>
            return "VERNEED";
         when DT_VERNEEDNUM =>
            return "VERNEEDNUM";
         when DT_AUXILIARY =>
            return "AUXILIARY";
         when DT_FILTER =>
            return "FILTER";
         when others =>
            return "?unknown?";
      end case;
   end Get_Dt_Name;

   procedure Disp_Dynamic (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;
      Tag : Unsigned_32;
      Val : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);
      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Read_Word4 (Base, Off, Tag);
         Read_Word4 (Base, Off, Val);
         Put ("tag : ");
         Put (Hex_Image (Tag));
         Put ("  (");
         Put (Get_Dt_Name (Tag));
         Put (")");
         Set_Col (34);
         Put ("val : ");
         Put (Hex_Image (Val));
         New_Line;
      end loop;
   end Disp_Dynamic;

   function Get_Dwarf_Form_Name (Name : Unsigned_32) return String
   is
      use Dwarf;
   begin
      case Name is
         when DW_FORM_Addr =>
            return "addr";
         when DW_FORM_Block2 =>
            return "block2";
         when DW_FORM_Block4 =>
            return "block4";
         when DW_FORM_Data2 =>
            return "data2";
         when DW_FORM_Data4 =>
            return "data4";
         when DW_FORM_Data8 =>
            return "data8";
         when DW_FORM_String =>
            return "string";
         when DW_FORM_Block =>
            return "block";
         when DW_FORM_Block1 =>
            return "block1";
         when DW_FORM_Data1 =>
            return "data1";
         when DW_FORM_Flag =>
            return "flag";
         when DW_FORM_Sdata =>
            return "sdata";
         when DW_FORM_Strp =>
            return "strp";
         when DW_FORM_Udata =>
            return "udata";
         when DW_FORM_Ref_Addr =>
            return "ref_addr";
         when DW_FORM_Ref1 =>
            return "ref1";
         when DW_FORM_Ref2 =>
            return "ref2";
         when DW_FORM_Ref4 =>
            return "ref4";
         when DW_FORM_Ref8 =>
            return "ref8";
         when DW_FORM_Ref_Udata =>
            return "ref_udata";
         when DW_FORM_Indirect =>
            return "indirect";
         when others =>
            return "unknown";
      end case;
   end Get_Dwarf_Form_Name;

   function Get_Dwarf_Tag_Name (Tag : Unsigned_32) return String
   is
      use Dwarf;
   begin
      case Tag is
         when DW_TAG_Array_Type =>
            return "array_type";
         when DW_TAG_Class_Type =>
            return "class_type";
         when DW_TAG_Entry_Point =>
            return "entry_point";
         when DW_TAG_Enumeration_Type =>
            return "enumeration_type";
         when DW_TAG_Formal_Parameter =>
            return "formal_parameter";
         when DW_TAG_Imported_Declaration =>
            return "imported_declaration";
         when DW_TAG_Label =>
            return "label";
         when DW_TAG_Lexical_Block =>
            return "lexical_block";
         when DW_TAG_Member =>
            return "member";
         when DW_TAG_Pointer_Type =>
            return "pointer_type";
         when DW_TAG_Reference_Type =>
            return "reference_type";
         when DW_TAG_Compile_Unit =>
            return "compile_unit";
         when DW_TAG_String_Type =>
            return "string_type";
         when DW_TAG_Structure_Type =>
            return "structure_type";
         when DW_TAG_Subroutine_Type =>
            return "subroutine_type";
         when DW_TAG_Typedef =>
            return "typedef";
         when DW_TAG_Union_Type =>
            return "union_type";
         when DW_TAG_Unspecified_Parameters =>
            return "unspecified_parameters";
         when DW_TAG_Variant =>
            return "variant";
         when DW_TAG_Common_Block =>
            return "common_block";
         when DW_TAG_Common_Inclusion =>
            return "common_inclusion";
         when DW_TAG_Inheritance =>
            return "inheritance";
         when DW_TAG_Inlined_Subroutine =>
            return "inlined_subroutine";
         when DW_TAG_Module =>
            return "module";
         when DW_TAG_Ptr_To_Member_Type =>
            return "ptr_to_member_type";
         when DW_TAG_Set_Type =>
            return "set_type";
         when DW_TAG_Subrange_Type =>
            return "subrange_type";
         when DW_TAG_With_Stmt =>
            return "with_stmt";
         when DW_TAG_Access_Declaration =>
            return "access_declaration";
         when DW_TAG_Base_Type =>
            return "base_type";
         when DW_TAG_Catch_Block =>
            return "catch_block";
         when DW_TAG_Const_Type =>
            return "const_type";
         when DW_TAG_Constant =>
            return "constant";
         when DW_TAG_Enumerator =>
            return "enumerator";
         when DW_TAG_File_Type =>
            return "file_type";
         when DW_TAG_Friend =>
            return "friend";
         when DW_TAG_Namelist =>
            return "namelist";
         when DW_TAG_Namelist_Item =>
            return "namelist_item";
         when DW_TAG_Packed_Type =>
            return "packed_type";
         when DW_TAG_Subprogram =>
            return "subprogram";
         when DW_TAG_Template_Type_Parameter =>
            return "template_type_parameter";
         when DW_TAG_Template_Value_Parameter =>
            return "template_value_parameter";
         when DW_TAG_Thrown_Type =>
            return "thrown_type";
         when DW_TAG_Try_Block =>
            return "try_block";
         when DW_TAG_Variant_Part =>
            return "variant_part";
         when DW_TAG_Variable =>
            return "variable";
         when DW_TAG_Volatile_Type =>
            return "volatile_type";
         when DW_TAG_Dwarf_Procedure =>
            return "dwarf_procedure";
         when DW_TAG_Restrict_Type =>
            return "restrict_type";
         when DW_TAG_Interface_Type =>
            return "interface_type";
         when DW_TAG_Namespace =>
            return "namespace";
         when DW_TAG_Imported_Module =>
            return "imported_module";
         when DW_TAG_Unspecified_Type =>
            return "unspecified_type";
         when DW_TAG_Partial_Unit =>
            return "partial_unit";
         when DW_TAG_Imported_Unit =>
            return "imported_unit";
         when DW_TAG_Mutable_Type =>
            return "mutable_type";
         when others =>
            return "unknown";
      end case;
   end Get_Dwarf_Tag_Name;

   function Get_Dwarf_At_Name (Attr : Unsigned_32) return String
   is
      use Dwarf;
   begin
      case Attr is
         when DW_AT_Sibling =>
            return "sibling";
         when DW_AT_Location =>
            return "location";
         when DW_AT_Name =>
            return "name";
         when DW_AT_Ordering =>
            return "ordering";
         when DW_AT_Byte_Size =>
            return "byte_size";
         when DW_AT_Bit_Offset =>
            return "bit_offset";
         when DW_AT_Bit_Size =>
            return "bit_size";
         when DW_AT_Stmt_List =>
            return "stmt_list";
         when DW_AT_Low_Pc =>
            return "low_pc";
         when DW_AT_High_Pc =>
            return "high_pc";
         when DW_AT_Language =>
            return "language";
         when DW_AT_Discr =>
            return "discr";
         when DW_AT_Discr_Value =>
            return "discr_value";
         when DW_AT_Visibility =>
            return "visibility";
         when DW_AT_Import =>
            return "import";
         when DW_AT_String_Length =>
            return "string_length";
         when DW_AT_Common_Reference =>
            return "common_reference";
         when DW_AT_Comp_Dir =>
            return "comp_dir";
         when DW_AT_Const_Value =>
            return "const_value";
         when DW_AT_Containing_Type =>
            return "containing_type";
         when DW_AT_Default_Value =>
            return "default_value";
         when DW_AT_Inline =>
            return "inline";
         when DW_AT_Is_Optional =>
            return "is_optional";
         when DW_AT_Lower_Bound =>
            return "lower_bound";
         when DW_AT_Producer =>
            return "producer";
         when DW_AT_Prototyped =>
            return "prototyped";
         when DW_AT_Return_Addr =>
            return "return_addr";
         when DW_AT_Start_Scope =>
            return "start_scope";
         when DW_AT_Stride_Size =>
            return "stride_size";
         when DW_AT_Upper_Bound =>
            return "upper_bound";
         when DW_AT_Abstract_Origin =>
            return "abstract_origin";
         when DW_AT_Accessibility =>
            return "accessibility";
         when DW_AT_Address_Class =>
            return "address_class";
         when DW_AT_Artificial =>
            return "artificial";
         when DW_AT_Base_Types =>
            return "base_types";
         when DW_AT_Calling_Convention =>
            return "calling_convention";
         when DW_AT_Count =>
            return "count";
         when DW_AT_Data_Member_Location =>
            return "data_member_location";
         when DW_AT_Decl_Column =>
            return "decl_column";
         when DW_AT_Decl_File =>
            return "decl_file";
         when DW_AT_Decl_Line =>
            return "decl_line";
         when DW_AT_Declaration =>
            return "declaration";
         when DW_AT_Discr_List =>
            return "discr_list";
         when DW_AT_Encoding =>
            return "encoding";
         when DW_AT_External =>
            return "external";
         when DW_AT_Frame_Base =>
            return "frame_base";
         when DW_AT_Friend =>
            return "friend";
         when DW_AT_Identifier_Case =>
            return "identifier_case";
         when DW_AT_Macro_Info =>
            return "macro_info";
         when DW_AT_Namelist_Item =>
            return "namelist_item";
         when DW_AT_Priority =>
            return "priority";
         when DW_AT_Segment =>
            return "segment";
         when DW_AT_Specification =>
            return "specification";
         when DW_AT_Static_Link =>
            return "static_link";
         when DW_AT_Type =>
            return "type";
         when DW_AT_Use_Location =>
            return "use_location";
         when DW_AT_Variable_Parameter =>
            return "variable_parameter";
         when DW_AT_Virtuality =>
            return "virtuality";
         when DW_AT_Vtable_Elem_Location =>
            return "vtable_elem_location";
         when DW_AT_Allocated =>
            return "allocated";
         when DW_AT_Associated =>
            return "associated";
         when DW_AT_Data_Location =>
            return "data_location";
         when DW_AT_Stride =>
            return "stride";
         when DW_AT_Entry_Pc =>
            return "entry_pc";
         when DW_AT_Use_UTF8 =>
            return "use_utf8";
         when DW_AT_Extension =>
            return "extension";
         when DW_AT_Ranges =>
            return "ranges";
         when DW_AT_Trampoline =>
            return "trampoline";
         when DW_AT_Call_Column =>
            return "call_column";
         when DW_AT_Call_File =>
            return "call_file";
         when DW_AT_Call_Line =>
            return "call_line";
         when DW_AT_Description =>
            return "description";
         when others =>
            return "unknown";
      end case;
   end Get_Dwarf_At_Name;

   procedure Disp_Debug_Abbrev (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Old_Off : Storage_Offset;
      Off : Storage_Offset;
      V : Unsigned_32;
      Tag : Unsigned_32;
      Name : Unsigned_32;
      Form : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Old_Off := Off;
         Read_ULEB128 (Base, Off, V);
         Put_Line ("abbrev #" & Hex_Image (V) & " at "
                   & Hex_Image (Unsigned_32 (Old_Off)) & ':');
         if V = 0 then
            Put_Line ("pad");
            goto Again;
         end if;
         Read_ULEB128 (Base, Off, Tag);
         Put (" tag: " & Hex_Image (Tag));
         Put (" (");
         Put (Get_Dwarf_Tag_Name (Tag));
         Put ("),  children: " & Hex_Image (Read_Byte (Base + Off)));
         New_Line;
         Off := Off + 1;
         loop
            Read_ULEB128 (Base, Off, Name);
            Read_ULEB128 (Base, Off, Form);
            Put ("   name: " & Hex_Image (Name));
            Put (" (");
            Put (Get_Dwarf_At_Name (Name));
            Put (")");
            Set_Col (42);
            Put ("form: " & Hex_Image (Form));
            Put (" (");
            Put (Get_Dwarf_Form_Name (Form));
            Put (")");
            New_Line;
            exit when Name = 0 and Form = 0;
         end loop;
         << Again >> null;
      end loop;
   end Disp_Debug_Abbrev;

   type Abbrev_Map_Type is array (Unsigned_32 range <>) of Address;
   type Abbrev_Map_Acc is access Abbrev_Map_Type;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Abbrev_Map_Type, Abbrev_Map_Acc);

   procedure Build_Abbrev_Map (Base : Address; Res : out Abbrev_Map_Acc)
   is
      Max : Unsigned_32;
      Off : Storage_Offset;
      V : Unsigned_32;
      V1 : Unsigned_32;
      N_Res : Abbrev_Map_Acc;
   begin
      Off := 0;
      Max := 0;
      Res := new Abbrev_Map_Type (0 .. 128);
      Res.all := (others => Null_Address);
      loop
         Read_ULEB128 (Base, Off, V);
         if V > Max then
            Max := V;
         end if;
         exit when V = 0;
         if Max > Res.all'Last then
            N_Res := new Abbrev_Map_Type (0 .. 2 * Max);
            N_Res (Res'Range) := Res.all;
            N_Res (Res'Last + 1 .. N_Res'Last) := (others => Null_Address);
            Unchecked_Deallocation (Res);
            Res := N_Res;
         end if;
         if Res (V) /= Null_Address then
            Put_Line ("!! abbrev override !!");
            return;
         end if;
         Res (V) := Base + Off;
         Read_ULEB128 (Base, Off, V);
         --  Skip child flag.
         Off := Off + 1;
         loop
            Read_ULEB128 (Base, Off, V);
            Read_ULEB128 (Base, Off, V1);
            exit when V = 0 and V1 = 0;
         end loop;
      end loop;
   end Build_Abbrev_Map;

   procedure Disp_Block (Base : Address;
                         Off : in out Storage_Offset;
                         Cnt : Unsigned_32)
   is
   begin
      for I in 1 .. Cnt loop
         Put (" ");
         Put (Hex_Image (Read_Byte (Base + Off + Storage_Offset (I - 1))));
      end loop;
      Off := Off + Storage_Offset (Cnt);
   end Disp_Block;

   procedure Disp_Dwarf_Form (Base : Address;
                              Off : in out Storage_Offset;
                              Form : Unsigned_32)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_Addr =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Base, Off, V);
               Put ("address: " & Hex_Image (V));
            end;
         when DW_FORM_Flag =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Put ("flag: " & Hex_Image (V));
            end;
         when DW_FORM_Block1 =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Put ("block1: " & Hex_Image (V));
               Disp_Block (Base, Off, Unsigned_32 (V));
            end;
         when DW_FORM_Data1 =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Put ("data1: " & Hex_Image (V));
            end;
         when DW_FORM_Data2 =>
            declare
               V : Unsigned_16;
            begin
               Read_Word2 (Base, Off, V);
               Put ("data2: " & Hex_Image (V));
            end;
         when DW_FORM_Data4 =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Base, Off, V);
               Put ("data4: " & Hex_Image (V));
            end;
         when DW_FORM_Sdata =>
            declare
               V : Unsigned_32;
            begin
               Read_SLEB128 (Base, Off, V);
               Put ("sdata: " & Hex_Image (V));
            end;
         when DW_FORM_Udata =>
            declare
               V : Unsigned_32;
            begin
               Read_ULEB128 (Base, Off, V);
               Put ("udata: " & Hex_Image (V));
            end;
         when DW_FORM_Ref4 =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Base, Off, V);
               Put ("ref4: " & Hex_Image (V));
            end;
         when DW_FORM_Strp =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Base, Off, V);
               Put ("strp: " & Hex_Image (V));
            end;
         when DW_FORM_String =>
            declare
               C : Unsigned_8;
            begin
               Put ("string: ");
               loop
                  Read_Byte (Base, Off, C);
                  exit when C = 0;
                  Put (Character'Val (C));
               end loop;
            end;
         when others =>
            Put ("???");
            raise Program_Error;
      end case;
   end Disp_Dwarf_Form;

   function Get_Dwarf_ATE_Name (Val : Unsigned_32) return String
   is
      use Dwarf;
   begin
      case Val is
         when DW_ATE_Address =>
            return "address";
         when DW_ATE_Boolean =>
            return "boolean";
         when DW_ATE_Complex_Float =>
            return "complex_float";
         when DW_ATE_Float =>
            return "float";
         when DW_ATE_Signed =>
            return "signed";
         when DW_ATE_Signed_Char =>
            return "signed_char";
         when DW_ATE_Unsigned =>
            return "unsigned";
         when DW_ATE_Unsigned_Char =>
            return "unsigned_char";
         when DW_ATE_Imaginary_Float =>
            return "imaginary_float";
         when others =>
            return "unknown";
      end case;
   end Get_Dwarf_ATE_Name;

   procedure Read_Dwarf_Constant (Base : Address;
                                  Off : in out Storage_Offset;
                                  Form : Unsigned_32;
                                  Res : out Unsigned_32)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_Data1 =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Res := Unsigned_32 (V);
            end;
         when DW_FORM_Data2 =>
            declare
               V : Unsigned_16;
            begin
               Read_Word2 (Base, Off, V);
               Res := Unsigned_32 (V);
            end;
         when DW_FORM_Data4 =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Base, Off, V);
               Res := V;
            end;
         when DW_FORM_Sdata =>
            declare
               V : Unsigned_32;
            begin
               Read_SLEB128 (Base, Off, V);
               Res := V;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Read_Dwarf_Constant;

   procedure Disp_Dwarf_Encoding
     (Base : Address; Off : in out Storage_Offset; Form : Unsigned_32)
   is
      Val : Unsigned_32;
   begin
      Read_Dwarf_Constant (Base, Off, Form, Val);
      Put (Get_Dwarf_ATE_Name (Val));
   end Disp_Dwarf_Encoding;

   function Get_Dwarf_Lang_Name (Lang : Unsigned_32) return String
   is
      use Dwarf;
   begin
      case Lang is
         when DW_LANG_C89 =>
            return "C89";
         when DW_LANG_C =>
            return "C";
         when DW_LANG_Ada83 =>
            return "Ada83";
         when DW_LANG_C_Plus_Plus =>
            return "C_Plus_Plus";
         when DW_LANG_Cobol74 =>
            return "Cobol74";
         when DW_LANG_Cobol85 =>
            return "Cobol85";
         when DW_LANG_Fortran77 =>
            return "Fortran77";
         when DW_LANG_Fortran90 =>
            return "Fortran90";
         when DW_LANG_Pascal83 =>
            return "Pascal83";
         when DW_LANG_Modula2 =>
            return "Modula2";
         when DW_LANG_Java =>
            return "Java";
         when DW_LANG_C99 =>
            return "C99";
         when DW_LANG_Ada95 =>
            return "Ada95";
         when DW_LANG_Fortran95 =>
            return "Fortran95";
         when DW_LANG_PLI =>
            return "PLI";
         when others =>
            return "?unknown?";
      end case;
   end Get_Dwarf_Lang_Name;

   procedure Disp_Dwarf_Language
     (Base : Address; Off : in out Storage_Offset; Form : Unsigned_32)
   is
      Val : Unsigned_32;
   begin
      Read_Dwarf_Constant (Base, Off, Form, Val);
      Put (Get_Dwarf_Lang_Name (Val));
   end Disp_Dwarf_Language;

   function Get_Dwarf_Op_Name (Op : Unsigned_8) return String
   is
      use Dwarf;
   begin
      case Op is
         when DW_OP_Addr =>
            return "addr";
         when DW_OP_Deref =>
            return "deref";
         when DW_OP_Const1u =>
            return "const1u";
         when DW_OP_Const1s =>
            return "const1s";
         when DW_OP_Const2u =>
            return "const2u";
         when DW_OP_Const2s =>
            return "const2s";
         when DW_OP_Const4u =>
            return "const4u";
         when DW_OP_Const4s =>
            return "const4s";
         when DW_OP_Const8u =>
            return "const8u";
         when DW_OP_Const8s =>
            return "const8s";
         when DW_OP_Constu =>
            return "constu";
         when DW_OP_Consts =>
            return "consts";
         when DW_OP_Dup =>
            return "dup";
         when DW_OP_Drop =>
            return "drop";
         when DW_OP_Over =>
            return "over";
         when DW_OP_Pick =>
            return "pick";
         when DW_OP_Swap =>
            return "swap";
         when DW_OP_Rot =>
            return "rot";
         when DW_OP_Xderef =>
            return "xderef";
         when DW_OP_Abs =>
            return "abs";
         when DW_OP_And =>
            return "and";
         when DW_OP_Div =>
            return "div";
         when DW_OP_Minus =>
            return "minus";
         when DW_OP_Mod =>
            return "mod";
         when DW_OP_Mul =>
            return "mul";
         when DW_OP_Neg =>
            return "neg";
         when DW_OP_Not =>
            return "not";
         when DW_OP_Or =>
            return "or";
         when DW_OP_Plus =>
            return "plus";
         when DW_OP_Plus_Uconst =>
            return "plus_uconst";
         when DW_OP_Shl =>
            return "shl";
         when DW_OP_Shr =>
            return "shr";
         when DW_OP_Shra =>
            return "shra";
         when DW_OP_Xor =>
            return "xor";
         when DW_OP_Skip =>
            return "skip";
         when DW_OP_Bra =>
            return "bra";
         when DW_OP_Eq =>
            return "eq";
         when DW_OP_Ge =>
            return "ge";
         when DW_OP_Gt =>
            return "gt";
         when DW_OP_Le =>
            return "le";
         when DW_OP_Lt =>
            return "lt";
         when DW_OP_Ne =>
            return "ne";
         when DW_OP_Lit0 =>
            return "lit0";
         when DW_OP_Lit1 =>
            return "lit1";
         when DW_OP_Lit2 =>
            return "lit2";
         when DW_OP_Lit3 =>
            return "lit3";
         when DW_OP_Lit4 =>
            return "lit4";
         when DW_OP_Lit5 =>
            return "lit5";
         when DW_OP_Lit6 =>
            return "lit6";
         when DW_OP_Lit7 =>
            return "lit7";
         when DW_OP_Lit8 =>
            return "lit8";
         when DW_OP_Lit9 =>
            return "lit9";
         when DW_OP_Lit10 =>
            return "lit10";
         when DW_OP_Lit11 =>
            return "lit11";
         when DW_OP_Lit12 =>
            return "lit12";
         when DW_OP_Lit13 =>
            return "lit13";
         when DW_OP_Lit14 =>
            return "lit14";
         when DW_OP_Lit15 =>
            return "lit15";
         when DW_OP_Lit16 =>
            return "lit16";
         when DW_OP_Lit17 =>
            return "lit17";
         when DW_OP_Lit18 =>
            return "lit18";
         when DW_OP_Lit19 =>
            return "lit19";
         when DW_OP_Lit20 =>
            return "lit20";
         when DW_OP_Lit21 =>
            return "lit21";
         when DW_OP_Lit22 =>
            return "lit22";
         when DW_OP_Lit23 =>
            return "lit23";
         when DW_OP_Lit24 =>
            return "lit24";
         when DW_OP_Lit25 =>
            return "lit25";
         when DW_OP_Lit26 =>
            return "lit26";
         when DW_OP_Lit27 =>
            return "lit27";
         when DW_OP_Lit28 =>
            return "lit28";
         when DW_OP_Lit29 =>
            return "lit29";
         when DW_OP_Lit30 =>
            return "lit30";
         when DW_OP_Lit31 =>
            return "lit31";
         when DW_OP_Reg0 =>
            return "reg0";
         when DW_OP_Reg1 =>
            return "reg1";
         when DW_OP_Reg2 =>
            return "reg2";
         when DW_OP_Reg3 =>
            return "reg3";
         when DW_OP_Reg4 =>
            return "reg4";
         when DW_OP_Reg5 =>
            return "reg5";
         when DW_OP_Reg6 =>
            return "reg6";
         when DW_OP_Reg7 =>
            return "reg7";
         when DW_OP_Reg8 =>
            return "reg8";
         when DW_OP_Reg9 =>
            return "reg9";
         when DW_OP_Reg10 =>
            return "reg10";
         when DW_OP_Reg11 =>
            return "reg11";
         when DW_OP_Reg12 =>
            return "reg12";
         when DW_OP_Reg13 =>
            return "reg13";
         when DW_OP_Reg14 =>
            return "reg14";
         when DW_OP_Reg15 =>
            return "reg15";
         when DW_OP_Reg16 =>
            return "reg16";
         when DW_OP_Reg17 =>
            return "reg17";
         when DW_OP_Reg18 =>
            return "reg18";
         when DW_OP_Reg19 =>
            return "reg19";
         when DW_OP_Reg20 =>
            return "reg20";
         when DW_OP_Reg21 =>
            return "reg21";
         when DW_OP_Reg22 =>
            return "reg22";
         when DW_OP_Reg23 =>
            return "reg23";
         when DW_OP_Reg24 =>
            return "reg24";
         when DW_OP_Reg25 =>
            return "reg25";
         when DW_OP_Reg26 =>
            return "reg26";
         when DW_OP_Reg27 =>
            return "reg27";
         when DW_OP_Reg28 =>
            return "reg28";
         when DW_OP_Reg29 =>
            return "reg29";
         when DW_OP_Reg30 =>
            return "reg30";
         when DW_OP_Reg31 =>
            return "reg31";
         when DW_OP_Breg0 =>
            return "breg0";
         when DW_OP_Breg1 =>
            return "breg1";
         when DW_OP_Breg2 =>
            return "breg2";
         when DW_OP_Breg3 =>
            return "breg3";
         when DW_OP_Breg4 =>
            return "breg4";
         when DW_OP_Breg5 =>
            return "breg5";
         when DW_OP_Breg6 =>
            return "breg6";
         when DW_OP_Breg7 =>
            return "breg7";
         when DW_OP_Breg8 =>
            return "breg8";
         when DW_OP_Breg9 =>
            return "breg9";
         when DW_OP_Breg10 =>
            return "breg10";
         when DW_OP_Breg11 =>
            return "breg11";
         when DW_OP_Breg12 =>
            return "breg12";
         when DW_OP_Breg13 =>
            return "breg13";
         when DW_OP_Breg14 =>
            return "breg14";
         when DW_OP_Breg15 =>
            return "breg15";
         when DW_OP_Breg16 =>
            return "breg16";
         when DW_OP_Breg17 =>
            return "breg17";
         when DW_OP_Breg18 =>
            return "breg18";
         when DW_OP_Breg19 =>
            return "breg19";
         when DW_OP_Breg20 =>
            return "breg20";
         when DW_OP_Breg21 =>
            return "breg21";
         when DW_OP_Breg22 =>
            return "breg22";
         when DW_OP_Breg23 =>
            return "breg23";
         when DW_OP_Breg24 =>
            return "breg24";
         when DW_OP_Breg25 =>
            return "breg25";
         when DW_OP_Breg26 =>
            return "breg26";
         when DW_OP_Breg27 =>
            return "breg27";
         when DW_OP_Breg28 =>
            return "breg28";
         when DW_OP_Breg29 =>
            return "breg29";
         when DW_OP_Breg30 =>
            return "breg30";
         when DW_OP_Breg31 =>
            return "breg31";
         when DW_OP_Regx =>
            return "regx";
         when DW_OP_Fbreg =>
            return "fbreg";
         when DW_OP_Bregx =>
            return "bregx";
         when DW_OP_Piece =>
            return "piece";
         when DW_OP_Deref_Size =>
            return "deref_size";
         when DW_OP_Xderef_Size =>
            return "xderef_size";
         when DW_OP_Nop =>
            return "nop";
         when DW_OP_Push_Object_Address =>
            return "push_object_address";
         when DW_OP_Call2 =>
            return "call2";
         when DW_OP_Call4 =>
            return "call4";
         when DW_OP_Call_Ref =>
            return "call_ref";
         when others =>
            return "unknown";
      end case;
   end Get_Dwarf_Op_Name;

   procedure Read_Dwarf_Block (Base : Address;
                               Off : in out Storage_Offset;
                               Form : Unsigned_32;
                               B : out Address;
                               L : out Unsigned_32)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_Block1 =>
            B := Base + Off + 1;
            L := Unsigned_32 (Read_Byte (Base + Off));
            Off := Off + 1;
         when others =>
            raise Program_Error;
      end case;
      Off := Off + Storage_Offset (L);
   end Read_Dwarf_Block;

   procedure Disp_Dwarf_Location
     (Base : Address; Off : in out Storage_Offset; Form : Unsigned_32)
   is
      use Dwarf;
      B : Address;
      L : Unsigned_32;
      Op : Unsigned_8;
      Boff : Storage_Offset;
      Is_Full : Boolean;
   begin
      Read_Dwarf_Block (Base, Off, Form, B, L);
      if L = 0 then
         return;
      end if;
      Is_Full := L > 6;
      Boff := 0;
      while Boff < Storage_Offset (L) loop
         if Is_Full then
            New_Line;
            Put ("   ");
            Put (Hex_Image (Unsigned_32 (Boff)));
            Put (": ");
         end if;
         Op := Read_Byte (B + Boff);
         Put (' ');
         Put (Get_Dwarf_Op_Name (Op));
         Boff := Boff + 1;
         case Op is
            when DW_OP_Addr =>
               declare
                  V : Unsigned_32;
               begin
                  Read_Word4 (B, Boff, V);
                  Put (':');
                  Put (Hex_Image (V));
               end;
            when DW_OP_Deref =>
               null;
            when DW_OP_Const1u
              | DW_OP_Const1s =>
               declare
                  V : Unsigned_8;
               begin
                  Read_Byte (B, Boff, V);
                  Put (':');
                  Put (Hex_Image (V));
               end;
--     DW_OP_Const2u     : constant := 16#0a#; -- 1 2-byte constant
--     DW_OP_Const2s     : constant := 16#0b#; -- 1 2-byte constant
--     DW_OP_Const4u     : constant := 16#0c#; -- 1 4-byte constant
--     DW_OP_Const4s     : constant := 16#0d#; -- 1 4-byte constant
--     DW_OP_Const8u     : constant := 16#0e#; -- 1 8-byte constant
--     DW_OP_Const8s     : constant := 16#0f#; -- 1 8-byte constant
--     DW_OP_Constu      : constant := 16#10#; -- 1 ULEB128 constant
--     DW_OP_Consts      : constant := 16#11#; -- 1 SLEB128 constant
--     DW_OP_Dup         : constant := 16#12#; -- 0
--     DW_OP_Drop        : constant := 16#13#; -- 0
--     DW_OP_Over        : constant := 16#14#; -- 0
--     DW_OP_Pick        : constant := 16#15#; -- 1 1-byte stack index

            when DW_OP_Swap
              | DW_OP_Rot
              | DW_OP_Xderef
              | DW_OP_Abs
              | DW_OP_And
              | DW_OP_Div
              | DW_OP_Minus
              | DW_OP_Mod
              | DW_OP_Mul
              | DW_OP_Neg
              | DW_OP_Not
              | DW_OP_Or
              | DW_OP_Plus =>
               null;
            when DW_OP_Plus_Uconst
              | DW_OP_Piece
              | DW_OP_Regx =>
               declare
                  V : Unsigned_32;
               begin
                  Read_ULEB128 (B, Boff, V);
                  Put (':');
                  Put (Hex_Image (V));
               end;
            when DW_OP_Shl
              | DW_OP_Shr
              | DW_OP_Shra
              | DW_OP_Xor =>
               null;
            when DW_OP_Skip
              | DW_OP_Bra =>
               declare
                  V : Unsigned_16;
               begin
                  Read_Word2 (B, Boff, V);
                  Put (':');
                  Put (Hex_Image (V));
                  Put (" (@");
                  --  FIXME: signed
                  Put (Hex_Image (Unsigned_32 (Boff) + Unsigned_32 (V)));
                  Put (")");
               end;
            when DW_OP_Eq
              | DW_OP_Ge
              | DW_OP_Gt
              | DW_OP_Le
              | DW_OP_Lt
              | DW_OP_Ne =>
               null;
            when DW_OP_Lit0 .. DW_OP_Lit31 =>
               null;
            when DW_OP_Reg0 .. DW_OP_Reg31 =>
               null;
            when DW_OP_Breg0 .. DW_OP_Breg31
              | DW_OP_Fbreg =>
               declare
                  V : Unsigned_32;
               begin
                  Read_SLEB128 (B, Boff, V);
                  Put (':');
                  Put (Hex_Image (V));
               end;

--   DW_OP_Regx        : constant := 16#90#; -- 1 ULEB128 register
--   DW_OP_Bregx       : constant := 16#92#; -- 2 ULEB128 reg + SLEB128 offset
--   DW_OP_Deref_Size  : constant := 16#94#; -- 1 1-byte size of data retrieved
--   DW_OP_Xderef_Size : constant := 16#95#; -- 1 1-byte size of data retrieved
            when DW_OP_Nop =>
               null;
--     DW_OP_Push_Object_Address : constant := 16#97#; -- 0
--     DW_OP_Call2       : constant := 16#98#; -- 1 2-byte offset of DIE
--     DW_OP_Call4       : constant := 16#99#; -- 1 4-byte offset of DIE
--     DW_OP_Call_Ref    : constant := 16#9a#; -- 1 4- or 8-byte offset of DIE
            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Disp_Dwarf_Location;

   procedure Disp_Debug_Info (File : Elf_File; Index : Elf_Half)
   is
      use Dwarf;

      Abbrev_Index : Elf_Half;
      Abbrev_Base : Address;
      Map : Abbrev_Map_Acc;
      Abbrev : Address;

      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;
      Aoff : Storage_Offset;
      Old_Off : Storage_Offset;

      Len : Unsigned_32;
      Ver : Unsigned_16;
      Abbrev_Off : Unsigned_32;
      Ptr_Sz : Unsigned_8;
      Last : Storage_Offset;
      Num : Unsigned_32;

      Tag : Unsigned_32;
      Name : Unsigned_32;
      Form : Unsigned_32;

      Level : Unsigned_8;
   begin
      Abbrev_Index := Get_Section_By_Name (File, ".debug_abbrev");
      Abbrev_Base := Get_Section_Base (File, Abbrev_Index);
      Map := null;

      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Put_Line ("Compilation unit at #"
                   & Hex_Image (Unsigned_32 (Off)) & ":");
         Read_Word4 (Base, Off, Len);
         Last := Off + Storage_Offset (Len);
         Read_Word2 (Base, Off, Ver);
         Read_Word4 (Base, Off, Abbrev_Off);
         Read_Byte (Base, Off, Ptr_Sz);
         Put (' ');
         Put ("length: " & Hex_Image (Len));
         Put (", version: " & Hex_Image (Ver));
         Put (", abbrev offset: " & Hex_Image (Abbrev_Off));
         Put (", ptr_sz: " & Hex_Image (Ptr_Sz));
         New_Line;
         Level := 0;

         Build_Abbrev_Map (Abbrev_Base + Storage_Offset (Abbrev_Off), Map);
         loop
            << Again >> null;
            exit when Off >= Last;
            Old_Off := Off;
            Read_ULEB128 (Base, Off, Num);
            Put ("<" & Hex_Image (Unsigned_32 (Old_Off)) & ">");
            Put ("<" & Hex_Image (Level) & ">");
            Put (" with abbrev #" & Hex_Image (Num));
            if Num = 0 then
               Level := Level - 1;
               New_Line;
               goto Again;
            end if;
            if Num <= Map.all'Last then
               Abbrev := Map (Num);
            else
               Abbrev := Null_Address;
            end if;
            if Abbrev = Null_Address then
               New_Line;
               Put ("!! abbrev #" & Hex_Image (Num) & " does not exist !!");
               New_Line;
               return;
            end if;
            Aoff := 0;
            Read_ULEB128 (Abbrev, Aoff, Tag);
            if Read_Byte (Abbrev + Aoff) /= 0 then
               Put (" [has_child]");
               Level := Level + 1;
            end if;
            New_Line;

            --  skip child.
            Aoff := Aoff + 1;
            Put (" tag: " & Hex_Image (Tag));
            Put (" (");
            Put (Get_Dwarf_Tag_Name (Tag));
            Put (")");
            New_Line;

            loop
               Read_ULEB128 (Abbrev, Aoff, Name);
               Read_ULEB128 (Abbrev, Aoff, Form);
               exit when Name = 0 and Form = 0;
               Put ("  ");
               Put (Get_Dwarf_At_Name (Name));
               Set_Col (24);
               Put (": ");
               Old_Off := Off;
               Disp_Dwarf_Form (Base, Off, Form);
               case Name is
                  when DW_AT_Encoding =>
                     Put (": ");
                     Disp_Dwarf_Encoding (Base, Old_Off, Form);
                  when DW_AT_Location
                    | DW_AT_Frame_Base
                    | DW_AT_Data_Member_Location =>
                     Put (":");
                     Disp_Dwarf_Location (Base, Old_Off, Form);
                  when DW_AT_Language =>
                     Put (": ");
                     Disp_Dwarf_Language (Base, Old_Off, Form);
                  when others =>
                     null;
               end case;
               New_Line;
            end loop;
         end loop;
         Unchecked_Deallocation (Map);
         New_Line;
      end loop;
   end Disp_Debug_Info;

   function Get_Phdr_Type_Name (Ptype : Elf_Word) return String is
   begin
      case Ptype is
         when PT_NULL =>
            return "NULL";
         when PT_LOAD =>
            return "LOAD";
         when PT_DYNAMIC =>
            return "DYNAMIC";
         when PT_INTERP =>
            return "INTERP";
         when PT_NOTE =>
            return "NOTE";
         when PT_SHLIB =>
            return "SHLIB";
         when PT_PHDR =>
            return "PHDR";
         when PT_TLS =>
            return "TLS";
         when PT_NUM =>
            return "NUM";
         when PT_GNU_EH_FRAME =>
            return "GNU_EH_FRAME";
         when PT_SUNWBSS =>
            return "SUNWBSS";
         when PT_SUNWSTACK =>
            return "SUNWSTACK";
         when others =>
            return "?unknown?";
      end case;
   end Get_Phdr_Type_Name;

   procedure Disp_Phdr (Phdr : Elf_Phdr)
   is
   begin
      Put ("type  : " & Hex_Image (Phdr.P_Type));
      Put ("  ");
      Put (Get_Phdr_Type_Name (Phdr.P_Type));
      New_Line;
      Put ("offset: " & Hex_Image (Phdr.P_Offset));
      Put ("  vaddr: " & Hex_Image (Phdr.P_Vaddr));
      Put ("  paddr: " & Hex_Image (Phdr.P_Paddr));
      New_Line;
      Put ("filesz: " & Hex_Image (Phdr.P_Filesz));
      Put ("  memsz: " & Hex_Image (Phdr.P_Memsz));
      Put ("  align: " & Hex_Image (Phdr.P_Align));
      --New_Line;
      Put ("  flags: " & Hex_Image (Phdr.P_Flags));
      Put (" (");
      if (Phdr.P_Flags and PF_X) /= 0 then
         Put ('X');
      end if;
      if (Phdr.P_Flags and PF_W) /= 0 then
         Put ('W');
      end if;
      if (Phdr.P_Flags and PF_R) /= 0 then
         Put ('R');
      end if;
      Put (")");
      New_Line;
   end Disp_Phdr;

   procedure Disp_Debug_Pubnames (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;
      B : Unsigned_8;

      Len : Unsigned_32;
      Ver : Unsigned_16;
      Info_Off : Unsigned_32;
      Info_Length : Unsigned_32;
      Last : Storage_Offset;
      Ioff : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Read_Word4 (Base, Off, Len);
         Last := Off + Storage_Offset (Len);
         Read_Word2 (Base, Off, Ver);
         Read_Word4 (Base, Off, Info_Off);
         Read_Word4 (Base, Off, Info_Length);
         Put ("length: " & Hex_Image (Len));
         Put (", version: " & Hex_Image (Ver));
         Put (", offset: " & Hex_Image (Info_Off));
         Put (", length: " & Hex_Image (Info_Length));
         New_Line;

         loop
            Read_Word4 (Base, Off, Ioff);
            Put ("  ");
            Put (Hex_Image (Ioff));
            if Ioff /= 0 then
               Put (": ");
               loop
                  Read_Byte (Base, Off, B);
                  exit when B = 0;
                  Put (Character'Val (B));
               end loop;
            end if;
            New_Line;
            exit when Ioff = 0;
         end loop;
      end loop;
   end Disp_Debug_Pubnames;

   procedure Disp_Debug_Aranges (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;

      Set_Len : Unsigned_32;
      Ver : Unsigned_16;
      Info_Off : Unsigned_32;
      Last : Storage_Offset;
      Addr_Sz : Unsigned_8;
      Seg_Sz : Unsigned_8;
      Pad : Unsigned_32;

      Addr : Unsigned_32;
      Len : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Read_Word4 (Base, Off, Set_Len);
         Last := Off + Storage_Offset (Set_Len);
         Read_Word2 (Base, Off, Ver);
         Read_Word4 (Base, Off, Info_Off);
         Read_Byte (Base, Off, Addr_Sz);
         Read_Byte (Base, Off, Seg_Sz);
         Read_Word4 (Base, Off, Pad);
         Put ("length: " & Hex_Image (Set_Len));
         Put (", version: " & Hex_Image (Ver));
         Put (", offset: " & Hex_Image (Info_Off));
         Put (", ptr_sz: " & Hex_Image (Addr_Sz));
         Put (", seg_sz: " & Hex_Image (Seg_Sz));
         New_Line;

         loop
            Read_Word4 (Base, Off, Addr);
            Read_Word4 (Base, Off, Len);
            Put ("  ");
            Put (Hex_Image (Addr));
            Put ('+');
            Put (Hex_Image (Len));
            New_Line;
            exit when Addr = 0 and Len = 0;
         end loop;
      end loop;
   end Disp_Debug_Aranges;

   procedure Disp_String (Base : Address; Off : in out Storage_Offset)
   is
      B : Unsigned_8;
   begin
      loop
         B := Read_Byte (Base + Off);
         Off := Off + 1;
         exit when B = 0;
         Put (Character'Val (B));
      end loop;
   end Disp_String;

   procedure Read_String (Base : Address; Off : in out Storage_Offset)
   is
      B : Unsigned_8;
   begin
      loop
         Read_Byte (Base, Off, B);
         exit when B = 0;
      end loop;
   end Read_String;

   function Get_Dwarf_LNS_Name (Lns : Unsigned_8) return String
   is
      use Dwarf;
   begin
      case Lns is
         when DW_LNS_Copy =>
            return "copy";
         when DW_LNS_Advance_Pc =>
            return "advance_pc";
         when DW_LNS_Advance_Line =>
            return "advance_line";
         when DW_LNS_Set_File =>
            return "set_file";
         when DW_LNS_Set_Column =>
            return "set_column";
         when DW_LNS_Negate_Stmt =>
            return "negate_stmt";
         when DW_LNS_Set_Basic_Block =>
            return "set_basic_block";
         when DW_LNS_Const_Add_Pc =>
            return "const_add_pc";
         when DW_LNS_Fixed_Advance_Pc =>
            return "fixed_advance_pc";
         when DW_LNS_Set_Prologue_End =>
            return "set_prologue_end";
         when DW_LNS_Set_Epilogue_Begin =>
            return "set_epilogue_begin";
         when DW_LNS_Set_Isa =>
            return "set_isa";
         when others =>
            return "?unknown?";
      end case;
   end Get_Dwarf_LNS_Name;

   procedure Disp_Debug_Line (File : Elf_File; Index : Elf_Half)
   is
      use Dwarf;
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;

      type Opc_Length_Type is array (Unsigned_8 range <>) of Unsigned_8;
      type Opc_Length_Acc is access Opc_Length_Type;
      Opc_Length : Opc_Length_Acc;

      Total_Len : Unsigned_32;
      Version : Unsigned_16;
      Prolog_Len : Unsigned_32;
      Min_Insn_Len : Unsigned_8;
      Dflt_Is_Stmt : Unsigned_8;
      Line_Base : Unsigned_8;
      Line_Range : Unsigned_8;
      Opc_Base : Unsigned_8;

      B : Unsigned_8;
      Arg : Unsigned_32;

      Old_Off : Storage_Offset;
      File_Dir : Unsigned_32;
      File_Time : Unsigned_32;
      File_Len : Unsigned_32;

      Ext_Len : Unsigned_32;
      Ext_Opc : Unsigned_8;

      Last : Storage_Offset;

      Pc : Unsigned_32;
      Line : Unsigned_32;
      Line_Base2 : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Read_Word4 (Base, Off, Total_Len);
         Last := Off + Storage_Offset (Total_Len);
         Read_Word2 (Base, Off, Version);
         Read_Word4 (Base, Off, Prolog_Len);
         Read_Byte (Base, Off, Min_Insn_Len);
         Read_Byte (Base, Off, Dflt_Is_Stmt);
         Read_Byte (Base, Off, Line_Base);
         Read_Byte (Base, Off, Line_Range);
         Read_Byte (Base, Off, Opc_Base);

         Pc := 0;
         Line := 1;

         Put ("length: " & Hex_Image (Total_Len));
         Put (", version: " & Hex_Image (Version));
         Put (", prolog_len: " & Hex_Image (Prolog_Len));
         New_Line;
         Put (" minimum_instruction_len: " & Hex_Image (Min_Insn_Len));
         Put (", default_is_stmt: " & Hex_Image (Dflt_Is_Stmt));
         New_Line;
         Put (" line_base: " & Hex_Image (Line_Base));
         Put (", line_range: " & Hex_Image (Line_Range));
         Put (", opc_base: " & Hex_Image (Opc_Base));
         New_Line;
         Line_Base2 := Unsigned_32 (Line_Base);
         if (Line_Base and 16#80#) /= 0 then
            Line_Base2 := Line_Base2 or 16#Ff_Ff_Ff_00#;
         end if;
         Put_Line ("standard_opcode_length:");
         Opc_Length := new Opc_Length_Type (1 .. Opc_Base - 1);
         for I in 1 .. Opc_Base - 1 loop
            Read_Byte (Base, Off, B);
            Put (' ');
            Put (Hex_Image (I));
            Put (" => ");
            Put (Hex_Image (B));
            Opc_Length (I) := B;
            New_Line;
         end loop;
         Put_Line ("include_directories:");
         loop
            B := Read_Byte (Base + Off);
            exit when B = 0;
            Put (' ');
            Disp_String (Base, Off);
            New_Line;
         end loop;
         Off := Off + 1;
         Put_Line ("file_names:");
         loop
            B := Read_Byte (Base + Off);
            exit when B = 0;
            Old_Off := Off;
            Read_String (Base, Off);
            Read_ULEB128 (Base, Off, File_Dir);
            Read_ULEB128 (Base, Off, File_Time);
            Read_ULEB128 (Base, Off, File_Len);
            Put (' ');
            Put (Hex_Image (File_Dir));
            Put (' ');
            Put (Hex_Image (File_Time));
            Put (' ');
            Put (Hex_Image (File_Len));
            Put (' ');
            Disp_String (Base, Old_Off);
            New_Line;
         end loop;
         Off := Off + 1;

         while Off < Last loop
            Put ("  ");
            Read_Byte (Base, Off, B);
            Put (Hex_Image (B));
            Old_Off := Off;
            if B < Opc_Base then
               case B is
                  when 0 =>
                     Put (" (extended)");
                     Read_ULEB128 (Base, Off, Ext_Len);
                     Put (", len: ");
                     Put (Hex_Image (Ext_Len));
                     Old_Off := Off;
                     Read_Byte (Base, Off, Ext_Opc);
                     Put (" opc:");
                     Put (Hex_Image (Ext_Opc));
                     Off := Old_Off + Storage_Offset (Ext_Len);
                  when others =>
                     Put (" (");
                     Put (Get_Dwarf_LNS_Name (B));
                     Put (")");
                     Set_Col (20);
                     for J in 1 .. Opc_Length (B) loop
                        Read_ULEB128 (Base, Off, Arg);
                        Put (" ");
                        Put (Hex_Image (Arg));
                     end loop;
               end case;
               case B is
                  when DW_LNS_Copy =>
                     Put (" pc=");
                     Put (Hex_Image (Pc));
                     Put (", line=");
                     Put (Unsigned_32'Image (Line));
                  when DW_LNS_Advance_Pc =>
                     Read_ULEB128 (Base, Old_Off, Arg);
                     Pc := Pc + Arg * Unsigned_32 (Min_Insn_Len);
                     Put ("  pc=");
                     Put (Hex_Image (Pc));
                  when DW_LNS_Advance_Line =>
                     Read_SLEB128 (Base, Old_Off, Arg);
                     Line := Line + Arg;
                     Put ("  line=");
                     Put (Unsigned_32'Image (Line));
                  when DW_LNS_Set_File =>
                     null;
                  when DW_LNS_Set_Column =>
                     null;
                  when DW_LNS_Negate_Stmt =>
                     null;
                  when DW_LNS_Set_Basic_Block =>
                     null;
                  when DW_LNS_Const_Add_Pc =>
                     Pc := Pc + Unsigned_32 ((255 - Opc_Base) / Line_Range)
                       * Unsigned_32 (Min_Insn_Len);
                     Put ("  pc=");
                     Put (Hex_Image (Pc));
                  when others =>
                     null;
               end case;
               New_Line;
            else
               B := B - Opc_Base;
               Pc := Pc + Unsigned_32 (B / Line_Range)
                 * Unsigned_32 (Min_Insn_Len);
               Line := Line + Line_Base2 + Unsigned_32 (B mod Line_Range);
               Put (" pc=");
               Put (Hex_Image (Pc));
               Put (", line=");
               Put (Unsigned_32'Image (Line));
               New_Line;
            end if;
         end loop;
      end loop;
   end Disp_Debug_Line;

   function Get_Dwarf_Cfi_Name (Cfi : Unsigned_8) return String
   is
      use Dwarf;
   begin
      case Cfi is
         when DW_CFA_Advance_Loc_Min .. DW_CFA_Advance_Loc_Max =>
            return "advance_loc";
         when DW_CFA_Offset_Min .. DW_CFA_Offset_Max =>
            return "offset";
         when DW_CFA_Restore_Min .. DW_CFA_Restore_Max =>
            return "restore";
         when DW_CFA_Nop =>
            return "nop";
         when DW_CFA_Set_Loc =>
            return "set_loc";
         when DW_CFA_Advance_Loc1 =>
            return "advance_loc1";
         when DW_CFA_Advance_Loc2 =>
            return "advance_loc2";
         when DW_CFA_Advance_Loc4 =>
            return "advance_loc4";
         when DW_CFA_Offset_Extended =>
            return "offset_extended";
         when DW_CFA_Restore_Extended =>
            return "restore_extended";
         when DW_CFA_Undefined =>
            return "undefined";
         when DW_CFA_Same_Value =>
            return "same_value";
         when DW_CFA_Register =>
            return "register";
         when DW_CFA_Remember_State =>
            return "remember_state";
         when DW_CFA_Restore_State =>
            return "restore_state";
         when DW_CFA_Def_Cfa =>
            return "def_cfa";
         when DW_CFA_Def_Cfa_Register =>
            return "def_cfa_register";
         when DW_CFA_Def_Cfa_Offset =>
            return "def_cfa_offset";
         when DW_CFA_Def_Cfa_Expression =>
            return "def_cfa_expression";
         when others =>
            return "?unknown?";
      end case;
   end Get_Dwarf_Cfi_Name;

   procedure Disp_Cfi (Base : Address; Length : Storage_Count)
   is
      use Dwarf;
      L : Storage_Offset;
      Op : Unsigned_8;
      Off : Unsigned_32;
      Reg : Unsigned_32;
   begin
      L := 0;
      while L < Length loop
         Op := Read_Byte (Base + L);
         Put (" ");
         Put (Hex_Image (Op));
         Put (" ");
         Put (Get_Dwarf_Cfi_Name (Op));
         Put (" ");
         L := L + 1;
         case Op is
            when DW_CFA_Nop =>
               null;
            when DW_CFA_Advance_Loc_Min .. DW_CFA_Advance_Loc_Max =>
               Put (Hex_Image (Op and 16#3f#));
            when DW_CFA_Offset_Min .. DW_CFA_Offset_Max =>
               Read_ULEB128 (Base, L, Off);
               Put ("reg:");
               Put (Hex_Image (Op and 16#3f#));
               Put (", offset:");
               Put (Hex_Image (Off));
            when DW_CFA_Def_Cfa =>
               Read_ULEB128 (Base, L, Reg);
               Read_ULEB128 (Base, L, Off);
               Put ("reg:");
               Put (Hex_Image (Reg));
               Put (", offset:");
               Put (Hex_Image (Off));
            when DW_CFA_Def_Cfa_Offset =>
               Read_ULEB128 (Base, L, Off);
               Put (Hex_Image (Off));
            when DW_CFA_Def_Cfa_Register =>
               Read_ULEB128 (Base, L, Reg);
               Put ("reg:");
               Put (Hex_Image (Reg));
            when others =>
               Put ("?unknown?");
               New_Line;
               exit;
         end case;
         New_Line;
      end loop;
   end Disp_Cfi;

   procedure Disp_Debug_Frame (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;
      Old_Off : Storage_Offset;

      Length : Unsigned_32;
      Cie_Id : Unsigned_32;
      Version : Unsigned_8;
      Augmentation : Unsigned_8;
      Code_Align : Unsigned_32;
      Data_Align : Unsigned_32;
      Ret_Addr_Reg : Unsigned_8;

      Init_Loc : Unsigned_32;
      Addr_Rng : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Read_Word4 (Base, Off, Length);
         Old_Off := Off;

         Read_Word4 (Base, Off, Cie_Id);
         if Cie_Id = 16#Ff_Ff_Ff_Ff# then
            Read_Byte (Base, Off, Version);
            Read_Byte (Base, Off, Augmentation);
            Put ("length: ");
            Put (Hex_Image (Length));
            Put (", CIE_id: ");
            Put (Hex_Image (Cie_Id));
            Put (", version: ");
            Put (Hex_Image (Version));
            if Augmentation /= 0 then
               Put (" +augmentation");
               New_Line;
            else
               New_Line;
               Read_ULEB128 (Base, Off, Code_Align);
               Read_SLEB128 (Base, Off, Data_Align);
               Read_Byte (Base, Off, Ret_Addr_Reg);
               Put ("code_align: ");
               Put (Hex_Image (Code_Align));
               Put (", data_align: ");
               Put (Hex_Image (Data_Align));
               Put (", ret_addr_reg: ");
               Put (Hex_Image (Ret_Addr_Reg));
               New_Line;
               Put ("initial instructions:");
               New_Line;
               Disp_Cfi (Base + Off, Old_Off + Storage_Offset (Length) - Off);
            end if;
         else
            Read_Word4 (Base, Off, Init_Loc);
            Read_Word4 (Base, Off, Addr_Rng);
            Put ("length: ");
            Put (Hex_Image (Length));
            Put (", CIE_pointer: ");
            Put (Hex_Image (Cie_Id));
            Put (", address_range: ");
            Put (Hex_Image (Init_Loc));
            Put ("-");
            Put (Hex_Image (Init_Loc + Addr_Rng));
            New_Line;
            Put ("instructions:");
            New_Line;
            Disp_Cfi (Base + Off, Old_Off + Storage_Offset (Length) - Off);
         end if;
         Off := Old_Off + Storage_Offset (Length);
      end loop;
   end Disp_Debug_Frame;

   procedure Read_Coded (Base : Address;
                         Offset : in out Storage_Offset;
                         Code : Unsigned_8;
                         Val : out Unsigned_32)
   is
      use Dwarf;

      V2 : Unsigned_16;
   begin
      if Code = DW_EH_PE_Omit then
         return;
      end if;
      case Code and DW_EH_PE_Format_Mask is
         when DW_EH_PE_Uleb128 =>
            Read_ULEB128 (Base, Offset, Val);
         when DW_EH_PE_Udata2 =>
            Read_Word2 (Base, Offset, V2);
            Val := Unsigned_32 (V2);
         when DW_EH_PE_Udata4 =>
            Read_Word4 (Base, Offset, Val);
         when DW_EH_PE_Sleb128 =>
            Read_SLEB128 (Base, Offset, Val);
         when DW_EH_PE_Sdata2 =>
            Read_Word2 (Base, Offset, V2);
            Val := Unsigned_32 (V2);
            if (V2 and 16#80_00#) /= 0 then
               Val := Val or 16#Ff_Ff_00_00#;
            end if;
         when DW_EH_PE_Sdata4 =>
            Read_Word4 (Base, Offset, Val);
         when others =>
            raise Program_Error;
      end case;
   end Read_Coded;

   procedure Disp_Eh_Frame_Hdr (File : Elf_File; Index : Elf_Half)
   is
      Shdr : Elf_Shdr_Acc;
      Base : Address;
      Off : Storage_Offset;

      Version : Unsigned_8;
      Eh_Frame_Ptr_Enc : Unsigned_8;
      Fde_Count_Enc : Unsigned_8;
      Table_Enc : Unsigned_8;

      Eh_Frame_Ptr : Unsigned_32;
      Fde_Count : Unsigned_32;

      Loc : Unsigned_32;
      Addr : Unsigned_32;
   begin
      Shdr := Get_Shdr (File, Index);
      Base := Get_Section_Base (File, Shdr.all);

      Off := 0;
      while Off < Storage_Offset (Shdr.Sh_Size) loop
         Read_Byte (Base, Off, Version);
         Read_Byte (Base, Off, Eh_Frame_Ptr_Enc);
         Read_Byte (Base, Off, Fde_Count_Enc);
         Read_Byte (Base, Off, Table_Enc);
         Put ("version: ");
         Put (Hex_Image (Version));
         Put (", encodings: ptr:");
         Put (Hex_Image (Eh_Frame_Ptr_Enc));
         Put (" count:");
         Put (Hex_Image (Fde_Count_Enc));
         Put (" table:");
         Put (Hex_Image (Table_Enc));
         New_Line;
         Read_Coded (Base, Off, Eh_Frame_Ptr_Enc, Eh_Frame_Ptr);
         Read_Coded (Base, Off, Fde_Count_Enc, Fde_Count);
         Put ("eh_frame_ptr: ");
         Put (Hex_Image (Eh_Frame_Ptr));
         Put (", fde_count: ");
         Put (Hex_Image (Fde_Count));
         New_Line;
         for I in 1 .. Fde_Count loop
            Read_Coded (Base, Off, Table_Enc, Loc);
            Read_Coded (Base, Off, Table_Enc, Addr);
            Put ("  init loc: ");
            Put (Hex_Image (Loc));
            Put (", addr : ");
            Put (Hex_Image (Addr));
            New_Line;
         end loop;
      end loop;
   end Disp_Eh_Frame_Hdr;
end Elfdumper;
