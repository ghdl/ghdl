--  Ortho JIT implementation for mcode.
--  Copyright (C) 2009 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

with Binary_File; use Binary_File;
with Binary_File.Memory;
with Ortho_Mcode; use Ortho_Mcode;
with Ortho_Code.Flags; use Ortho_Code.Flags;
with Ortho_Code.Binary;
with Ortho_Code.Debug;
with Ortho_Code.Abi;
with Binary_File.Elf;

package body Ortho_Jit is
   Snap_Filename : GNAT.OS_Lib.String_Access := null;

   --  Initialize the whole engine.
   procedure Init is
   begin
      Ortho_Mcode.Init;
      Binary_File.Memory.Write_Memory_Init;
   end Init;

   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address)
   is
      use Ortho_Code.Binary;
   begin
      Binary_File.Memory.Set_Symbol_Address (Get_Decl_Symbol (Decl), Addr);
   end Set_Address;

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address
   is
      use Ortho_Code.Binary;

      function Conv is new Ada.Unchecked_Conversion
        (Source => Pc_Type, Target => Address);
   begin
      return Conv (Get_Symbol_Vaddr (Get_Decl_Symbol (Decl)));
   end Get_Address;

   --  Do link.
   procedure Link (Status : out Boolean) is
   begin
      if Ortho_Code.Debug.Flag_Debug_Hli then
         --  Can't generate code in HLI.
         Status := True;
         return;
      end if;

      Ortho_Mcode.Finish;

      Ortho_Code.Abi.Link_Intrinsics;

      Binary_File.Memory.Write_Memory_Relocate (Status);
      if Status then
         return;
      end if;

      if Snap_Filename /= null then
         declare
            use Ada.Text_IO;
            Fd : File_Descriptor;
         begin
            Fd := Create_File (Snap_Filename.all, Binary);
            if Fd = Invalid_FD then
               Put_Line (Standard_Error,
                         "can't open '" & Snap_Filename.all & "'");
               Status := False;
               return;
            else
               Binary_File.Elf.Write_Elf (Fd);
               Close (Fd);
            end if;
         end;
      end if;
   end Link;

   procedure Finish is
   begin
      --  Free all the memory.
      Ortho_Mcode.Free_All;

      Binary_File.Finish;
   end Finish;

   function Decode_Option (Option : String) return Boolean
   is
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      if Opt = "-g" then
         Flag_Debug := Debug_Dwarf;
         return True;
      elsif Opt'Length > 5 and then Opt (1 .. 5) = "--be-" then
         Ortho_Code.Debug.Set_Be_Flag (Opt);
         return True;
      elsif Opt'Length > 7 and then Opt (1 .. 7) = "--snap=" then
         Snap_Filename := new String'(Opt (8 .. Opt'Last));
         return True;
      else
         return False;
      end if;
   end Decode_Option;

   procedure Disp_Help is
      use Ada.Text_IO;
   begin
      Put_Line (" -g             Generate debugging informations");
      Put_Line (" --debug-be=X   Set X internal debugging flags");
      Put_Line (" --snap=FILE    Write memory snapshot to FILE");
   end Disp_Help;

end Ortho_Jit;

