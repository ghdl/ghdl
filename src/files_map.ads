--  Loading of source files.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Types; use Types;

package Files_Map is

   -- Source file handling
   -----------------------

   --  Create the path from DIRECTORY and NAME:
   --  If NAME is an absolute pathname, then return NAME.
   --  Otherwise, return the concatenation of DIRECTORY and NAME.
   --  If ADD_NUL is TRUE, then a trailing '\0' is appended.
   function Get_Pathname
     (Directory : Name_Id; Name : Name_Id; Add_Nul : Boolean) return String;

   --  If NAME contains a directory separator, move it to the DIRECTORY name.
   --  At the return point, NAME has no directory components.
   procedure Normalize_Pathname
     (Directory : in out Name_Id; Name : in out Name_Id);

   --  Return an entry for a filename.  Null_Identifier for DIRECTORY means
   --  current directory.
   --  Load the filename if necessary.
   --  Return No_Source_File_Entry if the file does not exist.
   function Load_Source_File (Directory : Name_Id; Name : Name_Id)
                              return Source_File_Entry;

   --  Each file in memory has two terminal EOT.
   EOT : constant Character := Character'Val (4);

   --  Create a Source_File for a virtual file name.  Used for implicit,
   --  command-line and std.standard library.
   function Create_Virtual_Source_File (Name : Name_Id)
                                       return Source_File_Entry;

   --  Create a Source_File for a possible virtual file NAME using CONTENT
   --  as content of the file.  The file must not already exist.
   function Create_Source_File_From_String (Name : Name_Id; Content : String)
                                           return Source_File_Entry;

   -- Return a buffer (access to the contents of the file) for a file entry.
   function Get_File_Source (File : Source_File_Entry) return File_Buffer_Acc;

   -- Return the length of the file (which is the size of the file buffer).
   function Get_File_Length (File : Source_File_Entry) return Source_Ptr;

   -- Return the name of the file.
   function Get_File_Name (File : Source_File_Entry) return Name_Id;

   -- Return the directory of the file.
   function Get_Source_File_Directory (File : Source_File_Entry)
                                      return Name_Id;

   --  Return the entry of the last known file.
   --  This allow the user to create a table of Source_File_Entry.
   function Get_Last_Source_File_Entry return Source_File_Entry;

   --  Time stamp handling.
   function Is_Eq (L : Time_Stamp_Id; R : Time_Stamp_Id) return Boolean;
   function Is_Gt (L : Time_Stamp_Id; R : Time_Stamp_Id) return Boolean;
   function Get_Time_Stamp_String (Ts : Time_Stamp_Id) return String;

   --  Return the checksum of the content of FILE.
   function Get_File_Checksum (File : Source_File_Entry)
                              return File_Checksum_Id;

   --  True if two file checksums are identical.
   function Is_Eq (L, R : File_Checksum_Id) return Boolean;

   --  String image of CHECKSUM.
   function Get_File_Checksum_String (Checksum : File_Checksum_Id)
                                     return String;

   -- Return the current date of the system.
   function Get_Os_Time_Stamp return Time_Stamp_Id;

   -- Return the home directory (current directory).
   function Get_Home_Directory return Name_Id;

   --  Get the path of directory DIR.
   --function Get_Directory_Path (Dir : Directory_Index) return String;

   -- Add a new entry in the lines_table.
   -- The new entry must be the next one after the last entry.
   procedure File_Add_Line_Number
     (File : Source_File_Entry; Line : Natural; Pos : Source_Ptr);

   --  Convert LOCATION into a source file FILE and an offset POS in the
   --  file.
   procedure Location_To_File_Pos (Location : Location_Type;
                                   File : out Source_File_Entry;
                                   Pos : out Source_Ptr);
   --  Convert a FILE and an offset POS in the file into a location.
   function File_Pos_To_Location (File : Source_File_Entry; Pos : Source_Ptr)
                                 return Location_Type;
   --  Convert a FILE into a location.
   function Source_File_To_Location (File : Source_File_Entry)
                                    return Location_Type;

   --  Convert a FILE+LINE into a position.
   --  Return Source_Ptr_Bad in case of error (LINE out of bounds).
   function Line_To_Position (File : Source_File_Entry; Line : Natural)
                             return Source_Ptr;

   --  Translate LOCATION into coordinate (physical position).
   --  FILE identifies the filename.
   --  LINE_POS is the offset in the file of the first character of the line,
   --  LINE is the line number (first line is 1),
   --  OFFSET is the offset of the location in the line (first character is 0,
   --     a tabulation is one character),
   procedure Location_To_Coord (Location : Location_Type;
                                File : out Source_File_Entry;
                                Line_Pos : out Source_Ptr;
                                Line : out Natural;
                                Offset : out Natural);

   --  Translate coordinate into logical position.
   --  NAME is the name of the file,
   --  COL is the column (first character is 1, tabulation are at every 8
   --    positions).
   procedure Coord_To_Position (File : Source_File_Entry;
                                Line_Pos : Source_Ptr;
                                Offset : Natural;
                                Name : out Name_Id;
                                Col : out Natural);

   --  Translate LOCATION to NAME, LINE and COL.
   --  It is like to two procedures above.
   procedure Location_To_Position (Location : Location_Type;
                                   Name : out Name_Id;
                                   Line : out Natural;
                                   Col : out Natural);

   --  Return the image of LOC using the "FILENAME:LINE:COL" format or
   --  "LINE:COL" format if FILENAME is false;
   function Image (Loc : Location_Type; Filename : Boolean := True)
                  return String;

   --  Free all memory and reinitialize.
   procedure Initialize;
end Files_Map;
