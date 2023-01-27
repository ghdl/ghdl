--  GHDL Run Time (GRT) -  VHDL files subprograms.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Interfaces;

with Grt.Types; use Grt.Types;
with Grt.Stdio;

package Grt.Files is
   type Ghdl_File_Index is new Interfaces.Integer_32;

   --  File open mode.
   Read_Mode   : constant Ghdl_I32 := 0;
   Write_Mode  : constant Ghdl_I32 := 1;
   Append_Mode : constant Ghdl_I32 := 2;

   --  file_open_status.
   Open_Ok      : constant Ghdl_I32 := 0;
   Status_Error : constant Ghdl_I32 := 1;
   Name_Error   : constant Ghdl_I32 := 2;
   Mode_Error   : constant Ghdl_I32 := 3;

   subtype C_Files is Grt.Stdio.FILEs;

   --  Create a file entry (add an entry to the table).
   --  The file is not named and not opened.
   function Create_File (Is_Text : Boolean;
                         Kind : Character;
                         Sig : Ghdl_C_String) return Ghdl_File_Index;


   --  Check INDEX is a valid index.
   function Check_File_Index (Index : Ghdl_File_Index) return Boolean;

   --  Return the file for INDEX.
   function Get_File_Stream (Index : Ghdl_File_Index) return C_Files;
   procedure Set_File_Stream (Index : Ghdl_File_Index;
                              Stream : C_Files; Kind : Character);

   --  Get the file signature.
   function Get_File_Signature (Index : Ghdl_File_Index) return Ghdl_C_String;

   --  Return True iff file for INDEX is open.
   function Is_Open (Index : Ghdl_File_Index) return Boolean;

   function Get_Kind (Index : Ghdl_File_Index) return Character;

   function Is_Text_File (Index : Ghdl_File_Index) return Boolean;

   procedure Destroy_File (Index : Ghdl_File_Index);
end Grt.Files;
