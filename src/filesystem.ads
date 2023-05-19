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

with Types; use Types;

package Filesystem is
   type OS_Time_T is private;
   function ">" (L, R : OS_Time_T) return Boolean;

   --  An Invalid OS_Time_T, returned for missing file.
   Invalid_OS_Time : constant OS_Time_T;

   --  Return the current directory.
   function Get_Current_Directory return String;

   --  Returns '/' or '\'.
   function Get_Directory_Separator return Character;
   pragma Inline (Get_Directory_Separator);

   --  True iff FILENAME is an absolute path.
   function Is_Absolute_Path (Filename : String) return Boolean;

   --  True iff FILENAME designates a directory.
   function Is_Directory (Filename : String) return Boolean;

   --  True iff FILENAME designates a regular file.
   function Is_Regular_File (Filename : String) return Boolean;

   --  True if FILENAME designates an executable file.
   function Is_Executable_File (Filename : String) return Boolean;

   procedure Delete_File (Filename : String; Success : out Boolean);
   procedure Rename_File (Old_Filename : String;
                          New_Filename : String;
                          Success : out Boolean);

   --  Get file last modification time.
   function Get_File_Modification_Time (Filename : String) return OS_Time_T;

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

   --  Open/create a file for write (in binary mode).
   procedure Open_Write (Fd : out File_Descriptor; Filename : String);

   --  Return the length of FD.
   function File_Length (Fd : File_Descriptor) return Long_Integer;

   --  Return True in case of failure.
   function Is_Error (Fd : File_Descriptor) return Boolean;

   --  Read LENGTH bytes.
   procedure Read (Fd : in out File_Descriptor;
                   Buffer : System.Address;
                   Length : Long_Integer);

   --  Write LENGTH bytes.
   procedure Write (Fd : in out File_Descriptor;
                    Buffer : System.Address;
                    Length : Long_Integer);

   --  Blocking spawn of a process.
   --  (argv[0] is built from Command argument).
   function Spawn (Command : String; Args : String_Acc_Array) return Integer;

   --  Return the environment variable NAME.
   --  Return null if it doesn't exist.
   function Getenv (Name : String) return String_Acc;

   --  Find COMMAND in path.
   --  Return null if not found.
   function Locate_Executable_On_Path (Command : String) return String_Acc;
private
   type OS_Time_T is new GNAT.OS_Lib.OS_Time;

   Invalid_OS_Time : constant OS_Time_T :=
     OS_Time_T (GNAT.OS_Lib.Invalid_Time);

   type File_Descriptor is record
      Fd : GNAT.OS_Lib.File_Descriptor;
      Error : Boolean;
   end record;
end Filesystem;
