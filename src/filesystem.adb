--  Filesystem and OS interface.
--  Copyright (C) 2023 Tristan Gingold
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

with Ada.Calendar;
with Ada.Calendar.Time_Zones;

with GNAT.Directory_Operations;

package body Filesystem is
   function Get_Current_Directory return String is
   begin
      return GNAT.Directory_Operations.Get_Current_Dir;
   end Get_Current_Directory;

   function Get_Directory_Separator return Character is
   begin
      return GNAT.OS_Lib.Directory_Separator;
   end Get_Directory_Separator;

   function Is_Absolute_Path (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Absolute_Path (Filename);
   end Is_Absolute_Path;

   function Is_Regular_File (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Regular_File (Filename);
   end Is_Regular_File;

   function Is_Directory (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Directory (Filename);
   end Is_Directory;

   procedure Delete_File (Filename : String; Success : out Boolean) is
   begin
      GNAT.OS_Lib.Delete_File (Filename, Success);
   end Delete_File;

   procedure Rename_File (Old_Filename : String;
                          New_Filename : String;
                          Success : out Boolean) is
   begin
      GNAT.OS_Lib.Rename_File (Old_Filename, New_Filename, Success);
   end Rename_File;

   procedure Split_Now_Utc (Year : out Year_Range;
                            Month : out Month_Range;
                            Day : out Day_Range;
                            Sec : out Sec_Range;
                            Ms : out Ms_Range)
   is
      use Ada.Calendar;
      use Ada.Calendar.Time_Zones;

      Now : constant Time := Clock;
      Now_UTC : constant Time := Now - Duration (UTC_Time_Offset (Now) * 60);
      Sec1 : Day_Duration;
      S : Integer;
      M : Integer;
   begin
      --  Use UTC time (like file time stamp).
      Split (Now_UTC, Year, Month, Day, Sec1);

      S := Integer (Sec1);
      if Day_Duration (S) > Sec1 then
         --  We need a truncation.
         Sec := S - 1;
      else
         Sec := S;
      end if;

      Sec1 := Sec1 - Day_Duration (Sec);
      M := Integer (Sec1 * 1000);
      if M = 1000 then
         --  We need truncation.
         Ms := 999;
      else
         Ms := M;
      end if;
   end Split_Now_Utc;

   procedure Open_Read (Fd : out File_Descriptor; Filename : String)
   is
      use GNAT.OS_Lib;
      Filename0 : constant String := Filename & Ascii.NUL;
   begin
      Fd.Fd := GNAT.OS_Lib.Open_Read (Filename0, GNAT.OS_Lib.Binary);
      Fd.Error := Fd.Fd = Invalid_FD;
   end Open_Read;

   procedure Close (Fd : in out File_Descriptor) is
   begin
      GNAT.OS_Lib.Close (Fd.Fd);
      Fd.Fd := GNAT.OS_Lib.Invalid_FD;
   end Close;

   function File_Length (Fd : File_Descriptor) return Long_Integer is
   begin
      return GNAT.OS_Lib.File_Length (Fd.Fd);
   end File_Length;

   function Is_Error (Fd : File_Descriptor) return Boolean is
   begin
      return Fd.Error;
   end Is_Error;

   procedure Read (Fd : in out File_Descriptor;
                   Buffer : System.Address;
                   Length : Long_Integer)
   is
      Len : constant Natural := Natural (Length);
      Res : Integer;
   begin
      Res := GNAT.OS_Lib.Read (Fd.Fd, Buffer, Len);
      if Res /= Len then
         Fd.Error := True;
      end if;
   end Read;
end Filesystem;
