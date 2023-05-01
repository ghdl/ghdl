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

--  Currently, it's mainly a wrapper around GNAT.OS_Lib.

with System;

with GNAT.OS_Lib;

package Filesystem is
   --  Return the current directory.
   function Get_Current_Directory return String;

   --  Returns '/' or '\'.
   function Get_Directory_Separator return Character;

   --  True iff FILENAME is an absolute path.
   function Is_Absolute_Path (Filename : String) return Boolean;

   --  True iff FILENAME designates a directory.
   function Is_Directory (Filename : String) return Boolean;

   --  True iff FILENAME designates a regular file.
   function Is_Regular_File (Filename : String) return Boolean;

   procedure Delete_File (Filename : String; Success : out Boolean);
   procedure Rename_File (Old_Filename : String;
                          New_Filename : String;
                          Success : out Boolean);

   --  Ranges for UTC time (allow leap second).
   --  Millisecond precision.
   subtype Year_Range is Natural range 2000 .. 2099;
   subtype Month_Range is Natural range 1 .. 12;
   subtype Day_Range is Natural range 1 .. 31;
   subtype Sec_Range is Natural range 0 .. 86400;
   subtype Ms_Range is Natural range 0 .. 999;

   --  Directly get current time as UTC and split it.
   procedure Split_Now_Utc (Year : out Year_Range;
                            Month : out Month_Range;
                            Day : out Day_Range;
                            Sec : out Sec_Range;
                            Ms : out Ms_Range);

   type File_Descriptor is private;

   --  Open a file for read (in binary mode).
   procedure Open_Read (Fd : out File_Descriptor; Filename : String);
   procedure Close (Fd : in out File_Descriptor);

   --  Return the length of FD.
   function File_Length (Fd : File_Descriptor) return Long_Integer;

   --  Return True in case of failure.
   function Is_Error (Fd : File_Descriptor) return Boolean;

   --  Read LENGTH bytes.
   procedure Read (Fd : in out File_Descriptor;
                   Buffer : System.Address;
                   Length : Long_Integer);

private
   type File_Descriptor is record
      Fd : GNAT.OS_Lib.File_Descriptor;
      Error : Boolean;
   end record;
end Filesystem;
