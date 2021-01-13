--  Macho definitions.
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
with Interfaces; use Interfaces;
with System; use System;

package Macho is
   type Header_32 is record
      Magic      : Unsigned_32;
      Cputype    : Unsigned_32;
      Cpusubtype : Unsigned_32;
      Filetype   : Unsigned_32;
      Ncmds      : Unsigned_32;
      Sizeofcmds : Unsigned_32;
      Flags      : Unsigned_32;
   end record;

   type Header_64 is record
      Magic      : Unsigned_32;
      Cputype    : Unsigned_32;
      Cpusubtype : Unsigned_32;
      Filetype   : Unsigned_32;
      Ncmds      : Unsigned_32;
      Sizeofcmds : Unsigned_32;
      Flags      : Unsigned_32;
      Reserved   : Unsigned_32;
   end record;

   --  Size of Filehdr.
   Header_32_Size : constant Natural := Header_32'Size / Storage_Unit;
   Header_64_Size : constant Natural := Header_64'Size / Storage_Unit;

   --  Magic numbers.
   Magic_32 : constant Unsigned_32 := 16#feed_face#;
   Magic_64 : constant Unsigned_32 := 16#feed_facf#;

   Cpu_Arch_64  : constant Unsigned_32 := 16#0100_0000#;
   Cputype_I386 : constant Unsigned_32 := 7;
   Cpusubtype_I386_All : constant Unsigned_32 := 3;

   Mh_Object  : constant Unsigned_32 := 1;
   Mh_Execute : constant Unsigned_32 := 2;

   --  Load commands.
   type Load_Command is record
      Cmd : Unsigned_32;
      Cmdsize : Unsigned_32;
   end record;
   Lc_Size : constant Natural := Load_Command'Size / Storage_Unit;

   Lc_Segment_32 : constant Unsigned_32 := 1;
   type Segment_Command_32 is record
      Segname : String (1 .. 16);
      Vmaddr : Unsigned_32;
      Vmsize : Unsigned_32;
      Fileoff : Unsigned_32;
      Filesize : Unsigned_32;
      Maxprot : Unsigned_32;
      Initprot : Unsigned_32;
      Nsects : Unsigned_32;
      Flags : Unsigned_32;
   end record;
   Segment_Command_32_Size : constant Natural :=
     Segment_Command_32'Size / Storage_Unit;

   type Section_32 is record
      Sectname : String (1 .. 16);
      Segname : String (1 .. 16);
      Addr : Unsigned_32;
      Size : Unsigned_32;
      Offset : Unsigned_32;
      Align : Unsigned_32;
      Reloff : Unsigned_32;
      Nreloc : Unsigned_32;
      Flags  : Unsigned_32;
      Reserved1 : Unsigned_32;
      Reserved2 : Unsigned_32;
   end record;
   Section_32_Size : constant Natural := Section_32'Size / Storage_Unit;

   Lc_Segment_64 : constant Unsigned_32 := 16#19#;
   type Segment_Command_64 is record
      Segname : String (1 .. 16);
      Vmaddr : Unsigned_64;
      Vmsize : Unsigned_64;
      Fileoff : Unsigned_64;
      Filesize : Unsigned_64;
      Maxprot : Unsigned_32;
      Initprot : Unsigned_32;
      Nsects : Unsigned_32;
      Flags : Unsigned_32;
   end record;
   Segment_Command_64_Size : constant Natural :=
     Segment_Command_64'Size / Storage_Unit;

   type Section_64 is record
      Sectname : String (1 .. 16);
      Segname : String (1 .. 16);
      Addr : Unsigned_64;
      Size : Unsigned_64;
      Offset : Unsigned_32;
      Align : Unsigned_32;
      Reloff : Unsigned_32;
      Nreloc : Unsigned_32;
      Flags  : Unsigned_32;
      Reserved1 : Unsigned_32;
      Reserved2 : Unsigned_32;
      Reserved3 : Unsigned_32;
   end record;
   Section_64_Size : constant Natural := Section_64'Size / Storage_Unit;

   Lc_Symtab : constant Unsigned_32 := 2;
   type Symtab_Command is record
      Symoff : Unsigned_32;
      Nsyms : Unsigned_32;
      Stroff : Unsigned_32;
      Strsize : Unsigned_32;
   end record;
   Symtab_Command_Size : constant Natural :=
     Symtab_Command'Size / Storage_Unit;

   type Nlist_32 is record
      N_Strx : Unsigned_32;
      N_Type : Unsigned_8;
      N_Sect : Unsigned_8;
      N_Desc : Unsigned_16;
      N_Value : Unsigned_32;
   end record;

   Nlist_32_Size : constant Natural := Nlist_32'Size / Storage_Unit;

   type Nlist_64 is record
      N_Strx : Unsigned_32;
      N_Type : Unsigned_8;
      N_Sect : Unsigned_8;
      N_Desc : Unsigned_16;
      N_Value : Unsigned_64;
   end record;

   Nlist_64_Size : constant Natural := Nlist_64'Size / Storage_Unit;

   N_Undf : constant Unsigned_8 := 16#00#;
   N_Ext  : constant Unsigned_8 := 16#01#;
   N_Sect : constant Unsigned_8 := 16#0e#;
end Macho;
