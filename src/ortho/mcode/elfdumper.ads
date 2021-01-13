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
with System; use System;
with Elf_Common; use Elf_Common;
with Elf_Arch; use Elf_Arch;
with Ada.Unchecked_Conversion;

package Elfdumper is
   procedure Disp_Ehdr (Ehdr : Elf_Ehdr);

   type Strtab_Fat_Type is array (Elf_Size) of Character;
   type Strtab_Fat_Acc is access all Strtab_Fat_Type;

   type Strtab_Type is record
      Base : Strtab_Fat_Acc;
      Length : Elf_Size;
   end record;

   Null_Strtab : constant Strtab_Type := (null, 0);

   Nul : constant Character := Character'Val (0);

   function Get_String (Strtab : Strtab_Type; N : Elf_Size)
                       return String;

   procedure Disp_Shdr (Shdr : Elf_Shdr; Sh_Strtab : Strtab_Type);

   type Elf_Shdr_Array is array (Elf_Half range <>) of Elf_Shdr;

   type Elf_File is limited private;
   type Elf_File_Status is
     (
      --  No error.
      Status_Ok,

      --  Cannot open file.
      Status_Open_Failure,

      Status_Bad_File,
      Status_Memory,
      Status_Read_Error,
      Status_Bad_Magic,
      Status_Bad_Class
      );

   procedure Open_File (File : out Elf_File; Filename : String);

   function Get_Status (File : Elf_File) return Elf_File_Status;

   type Elf_Ehdr_Acc is access all Elf_Ehdr;

   function Get_Ehdr (File : Elf_File) return Elf_Ehdr_Acc;

   procedure Load_Shdr (File : in out Elf_File);

   type Elf_Shdr_Acc is access all Elf_Shdr;

   function Get_Shdr (File : Elf_File; Index : Elf_Half)
                     return Elf_Shdr_Acc;

   function Get_Shdr_Type_Name (Stype : Elf_Word) return String;

   procedure Load_Phdr (File : in out Elf_File);

   type Elf_Phdr_Acc is access all Elf_Phdr;

   function Get_Phdr (File : Elf_File; Index : Elf_Half)
                     return Elf_Phdr_Acc;

   function Get_Segment_Base (File : Elf_File; Index : Elf_Half)
                             return Address;

   function Get_Sh_Strtab (File : Elf_File) return Strtab_Type;

   procedure Disp_Sym (File : Elf_File;
                       Sym : Elf_Sym;
                       Strtab : Strtab_Type);

   procedure Disp_Symtab (File : Elf_File; Index : Elf_Half);
   procedure Disp_Strtab (File : Elf_File; Index : Elf_Half);

   function Get_Section_Name (File : Elf_File; Index : Elf_Half)
                             return String;

   function Get_Section_By_Name (File : Elf_File; Name : String)
                                return Elf_Half;

   procedure Disp_Debug_Abbrev (File : Elf_File; Index : Elf_Half);
   procedure Disp_Debug_Info (File : Elf_File; Index : Elf_Half);
   procedure Disp_Debug_Pubnames (File : Elf_File; Index : Elf_Half);
   procedure Disp_Debug_Aranges (File : Elf_File; Index : Elf_Half);
   procedure Disp_Debug_Line (File : Elf_File; Index : Elf_Half);
   procedure Disp_Debug_Frame (File : Elf_File; Index : Elf_Half);
   procedure Disp_Eh_Frame_Hdr (File : Elf_File; Index : Elf_Half);

   procedure Disp_Phdr (Phdr : Elf_Phdr);

   procedure Disp_Segment_Note (File : Elf_File; Index : Elf_Half);
   procedure Disp_Section_Note (File : Elf_File; Index : Elf_Half);

   procedure Disp_Dynamic (File : Elf_File; Index : Elf_Half);
private
   use System;

   function To_Strtab_Fat_Acc is new Ada.Unchecked_Conversion
     (Address, Strtab_Fat_Acc);

   type String_Acc is access String;

   function To_Elf_Ehdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Ehdr_Acc);

   function To_Elf_Phdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Phdr_Acc);

   function To_Elf_Shdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Shdr_Acc);

   type Elf_Sym_Acc is access all Elf_Sym;
   function To_Elf_Sym_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Sym_Acc);

   type Elf_Shdr_Arr is array (Elf_Half) of Elf_Shdr;

   type Elf_Shdr_Arr_Acc is access all Elf_Shdr_Arr;
   function To_Elf_Shdr_Arr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Shdr_Arr_Acc);

   type Elf_File is record
      --  Name of the file.
      Filename : String_Acc;

      --  Status, used to report errors.
      Status : Elf_File_Status;

      --  Length of the file.
      Length : Elf_Off;

      --  File contents.
      Base : Address;

      Ehdr : Elf_Ehdr_Acc;

      Shdr_Base : Address;
      Sh_Strtab : Strtab_Type;

      Phdr_Base : Address;
   end record;
end Elfdumper;
