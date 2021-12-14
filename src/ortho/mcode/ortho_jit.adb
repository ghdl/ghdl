--  Ortho JIT implementation for mcode.
--  Copyright (C) 2009 - 2015 Tristan Gingold
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO;

with Binary_File; use Binary_File;
with Binary_File.Memory;
with Ortho_Mcode;
with Ortho_Mcode.Jit;
with Ortho_Code.Flags; use Ortho_Code.Flags;
with Ortho_Code.Debug;
with Ortho_Code.Abi;
with Ortho_Code.Dwarf;
with Binary_File.Format;
with Symbolizer;

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
     renames Ortho_Mcode.Jit.Set_Address;

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address
     renames Ortho_Mcode.Jit.Get_Address;

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

      Ortho_Code.Abi.Register_Unwind;

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
               Binary_File.Format.Write (Fd);
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
      elsif Opt = "-g0" then
         Flag_Debug := Debug_None;
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
      Put_Line (" -g0            Do not generate any debugging informations");
      Put_Line (" --debug-be=X   Set X internal debugging flags");
      Put_Line (" --snap=FILE    Write memory snapshot to FILE");
   end Disp_Help;

   function Get_Jit_Name return String is
   begin
      return "mcode";
   end Get_Jit_Name;

   procedure Symbolize (Pc : Address;
                        Filename : out Address;
                        Lineno : out Natural;
                        Subprg : out Address)
   is
      use Binary_File.Memory;
      use Symbolizer;

      function Get_Section_Content (Sect : Section_Acc) return Section_Content
      is
         Addr : Address;
         Size : Pc_Type;
      begin
         if Sect = null then
            return (Null_Address, 0);
         else
            Addr := Get_Section_Addr (Sect);
            Size := Get_Section_Size (Sect);
            return (Addr, Storage_Offset (Size));
         end if;
      end Get_Section_Content;

      Sections : Dwarf_Sections;
      Res : Symbolize_Result;
   begin
      Sections.Debug_Line :=
        Get_Section_Content (Ortho_Code.Dwarf.Line_Sect);
      Sections.Debug_Info :=
        Get_Section_Content (Ortho_Code.Dwarf.Info_Sect);
      Sections.Debug_Abbrev :=
        Get_Section_Content (Ortho_Code.Dwarf.Abbrev_Sect);

      Symbolize_Address (Pc, Sections, Res);

      Filename := Res.Filename;
      Lineno := Res.Line;
      Subprg := Res.Subprg_Name;
   end Symbolize;

end Ortho_Jit;
