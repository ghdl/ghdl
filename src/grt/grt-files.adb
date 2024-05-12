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

with Grt.Stdio; use Grt.Stdio;
with Grt.Table;
pragma Elaborate_All (Grt.Table);

package body Grt.Files is
   type File_Entry_Type is record
      --  The corresponding C stream.
      Stream : C_Files;

      Signature : Ghdl_C_String;

      --  Open kind: r, a or w.
      Kind : Character;

      Is_Text : Boolean;

      --  True if the file entry is used.
      Is_Alive : Boolean;
   end record;

   package Files_Table is new Grt.Table
     (Table_Component_Type => File_Entry_Type,
      Table_Index_Type => Ghdl_File_Index,
      Table_Low_Bound => 1,
      Table_Initial => 2);

   function Check_File_Index (Index : Ghdl_File_Index) return Boolean is
   begin
      return Index in Files_Table.First .. Files_Table.Last;
   end Check_File_Index;

   function Get_File_Stream (Index : Ghdl_File_Index) return C_Files is
   begin
      return Files_Table.Table (Index).Stream;
   end Get_File_Stream;

   procedure Set_File_Stream (Index : Ghdl_File_Index;
                              Stream : C_Files; Kind : Character) is
   begin
      Files_Table.Table (Index).Stream := Stream;
      Files_Table.Table (Index).Kind := Kind;
   end Set_File_Stream;

   function Get_File_Signature (Index : Ghdl_File_Index)
                               return Ghdl_C_String is
   begin
      return Files_Table.Table (Index).Signature;
   end Get_File_Signature;

   function Is_Open (Index : Ghdl_File_Index) return Boolean is
   begin
      return Files_Table.Table (Index).Stream /= NULL_Stream;
   end Is_Open;

   function Get_Kind (Index : Ghdl_File_Index) return Character is
   begin
      return Files_Table.Table (Index).Kind;
   end Get_Kind;

   function Is_Text_File (Index : Ghdl_File_Index) return Boolean is
   begin
      return Files_Table.Table (Index).Is_Text;
   end Is_Text_File;

   function Create_File (Is_Text : Boolean;
                         Kind : Character;
                         Sig : Ghdl_C_String) return Ghdl_File_Index is
   begin
      Files_Table.Append ((Stream => NULL_Stream,
                           Signature => Sig,
                           Kind => Kind,
                           Is_Text => Is_Text,
                           Is_Alive => True));
      return Files_Table.Last;
   end Create_File;

   procedure Destroy_File (Index : Ghdl_File_Index) is
   begin
      Files_Table.Table (Index).Is_Alive := False;
      if Index = Files_Table.Last then
         while Files_Table.Last >= Files_Table.First
           and then Files_Table.Table (Files_Table.Last).Is_Alive = False
         loop
            Files_Table.Decrement_Last;
         end loop;
      end if;
   end Destroy_File;

end Grt.Files;
