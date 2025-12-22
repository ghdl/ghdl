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

with Ortho_Debug;
with Ortho_Debug.Disp;
pragma Unreferenced (Ortho_Debug.Disp);
with Interfaces.C_Streams; use Interfaces.C_Streams;

package body Ortho_Jit is
   procedure Init is
   begin
      Ortho_Debug.Init;
   end Init;

   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address) is
   begin
      null;
   end Set_Address;

   function Get_Address (Decl : O_Dnode) return Address
   is
      pragma Unreferenced (Decl);
   begin
      return Null_Address;
   end Get_Address;

   function Get_Byte_Size (Atype : O_Tnode) return Storage_Count
   is
      pragma Unreferenced (Atype);
   begin
      return 0;
   end Get_Byte_Size;

   function Get_Field_Offset (Field : O_Fnode) return Storage_Count
   is
      pragma Unreferenced (Field);
   begin
      return 0;
   end Get_Field_Offset;

   --  Do link.
   procedure Link (Status : out Boolean) is
   begin
      Ortho_Debug.Disp.Disp_To_File (stdout);

      --  Can't generate code in HLI.
      Status := True;
   end Link;

   procedure Finish is
   begin
      Ortho_Debug.Finish;
   end Finish;

   function Decode_Option (Option : String) return Boolean
   is
      pragma Unreferenced (Option);
   begin
      return False;
   end Decode_Option;

   procedure Disp_Help is
   begin
      null;
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
      pragma Unreferenced (Pc);
   begin
      Filename := Null_Address;
      Lineno := 0;
      Subprg := Null_Address;
   end Symbolize;

end Ortho_Jit;
