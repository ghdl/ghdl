--  Loading of source files.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Types; use Types;
with Tables;
with Dyn_Tables;
with Vhdl.Types;

--  Source file handling

package Files_Map is

   --  Range for Tab_Stop.
   subtype Tab_Stop_Range is Natural range 1 .. 120;

   --  Tab width: a tab character jumps to the next column that is one plus a
   --  multiple of this witdh (if columns are numbered from 1).
   Tab_Stop : Tab_Stop_Range := 8;

   --  Create the path from DIRECTORY and NAME:
   --  If NAME is an absolute pathname, then return NAME.
   --  Otherwise, return the concatenation of DIRECTORY and NAME.
   function Get_Pathname (Directory : Name_Id; Name : Name_Id) return String;

   --  If NAME contains a directory separator, move it to the DIRECTORY name.
   --  At the return point, NAME has no directory components.
   procedure Normalize_Pathname
     (Directory : in out Name_Id; Name : in out Name_Id);

   --  Return an entry for a filename.  Null_Identifier for DIRECTORY means
   --  current directory.
   --  Load the filename if necessary.
   --  Return No_Source_File_Entry if the file does not exist.
   function Read_Source_File (Directory : Name_Id; Name : Name_Id)
                              return Source_File_Entry;

   --  Reserve an entry, but do not read any file.
   --  The length should includes the two terminal EOT.
   function Reserve_Source_File
     (Directory : Name_Id; Name : Name_Id; Length : Source_Ptr)
     return Source_File_Entry;

   --  Each file in memory has two terminal EOT.
   EOT : constant Character := Character'Val (4);

   --  From the extension of FILENAME, extract the language.
   --  Return Language_Unknown is not known.
   function Find_Language (Filename : String) return Language_Type;

   --  Create an empty Source_File for a virtual file name.  Used for implicit,
   --  command-line and std.standard library.
   function Create_Virtual_Source_File (Name : Name_Id)
                                       return Source_File_Entry;

   --  Create a Source_File for a possible virtual file NAME using CONTENT
   --  as content of the file.  The file must not already exist.
   function Create_Source_File_From_String (Name : Name_Id; Content : String)
                                           return Source_File_Entry;

   --  Create a pseudo source file from REF for instance INST (created at
   --  location LOC).  The content of this file is the same as REF, but with
   --  new locations so that it is possible to retrieve the instance from
   --  the new locations.
   function Create_Instance_Source_File (Ref : Source_File_Entry;
                                         Loc : Location_Type;
                                         Inst : Vhdl.Types.Vhdl_Node)
                                        return Source_File_Entry;

   --  Unload last source file.  Works only with the last one.  Must be
   --  carefully used as the corresponding locations will be reused.
   procedure Unload_Last_Source_File (File : Source_File_Entry);

   --  Mark FILE as unavailable: clear the name and directory.
   --  This is needed before creating a new source file with the same name.
   procedure Discard_Source_File (File : Source_File_Entry);

   --  Free resources used by FILE, but keep the entry.
   --  (It could be recycled for files that could fit - not implemented).
   procedure Free_Source_File (File : Source_File_Entry);

   --  Relocate location LOC (which must be in the reference of INST_FILE)
   --  for instrnace INST_FILE.
   function Instance_Relocate
     (Inst_File : Source_File_Entry; Loc : Location_Type)
     return Location_Type;

   --  If LOC is a location of an instance (in a file created by
   --  create_instance_source_file), return the location where the instance
   --  has been created.  Otherwise, return No_Location.
   function Location_Instance_To_Location
     (Loc : Location_Type) return Location_Type;

   --  If POS points to the start of the gap of FILE, it will be updated
   --  to the next character after the gap.
   procedure Skip_Gap (File : Source_File_Entry; Pos : in out Source_Ptr);

   --  Return a buffer (access to the contents of the file) for a file entry.
   function Get_File_Source (File : Source_File_Entry) return File_Buffer_Acc;

   --  Likewise but return a pointer.  To be used only from non-Ada code.
   function Get_File_Buffer (File : Source_File_Entry) return File_Buffer_Ptr;

   --  Set the length of the file (which is less than the size of the
   --  file buffer).
   --  Set also append two EOT at the end of the file.
   procedure Set_File_Length (File : Source_File_Entry; Length : Source_Ptr);

   --  Get the position of the first EOT character.
   function Get_File_Length (File : Source_File_Entry) return Source_Ptr;

   --  Get the length of the content; this is the file length minus the gap,
   --  if the gap is before the end.
   function Get_Content_Length (File : Source_File_Entry) return Source_Ptr;

   --  Get the length of the buffer, which always includes the gap and the
   --  two terminal EOT.
   function Get_Buffer_Length (File : Source_File_Entry) return Source_Ptr;

   --  Return the name of the file.
   function Get_File_Name (File : Source_File_Entry) return Name_Id;

   --  Return the directory of the file.
   function Get_Directory_Name (File : Source_File_Entry) return Name_Id;

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

   --  Return the current date of the system.
   function Get_Os_Time_Stamp return Time_Stamp_Id;

   --  Return the home directory (current directory).
   function Get_Home_Directory return Name_Id;

   --  Add a new entry in the lines_table.
   --  The new entry must be the next one after the last entry.
   procedure File_Add_Line_Number
     (File : Source_File_Entry; Line : Positive; Pos : Source_Ptr);

   --  Convert LOCATION to a source file.  Return No_Source_File_Entry if
   --  LOCATION is incorrect.
   function Location_To_File (Location : Location_Type)
                             return Source_File_Entry;

   --  Convert LOCATION and FILE to a position (offset) into the source file.
   function Location_File_To_Pos
     (Location : Location_Type; File : Source_File_Entry) return Source_Ptr;

   --  Convert LOCATION and FILE to a line number.
   function Location_File_To_Line
     (Location : Location_Type; File : Source_File_Entry) return Positive;

   --  Get the offset in the line LINE of LOC.
   function Location_File_Line_To_Offset
     (Loc : Location_Type; File : Source_File_Entry; Line : Positive)
     return Natural;

   --  Get logical column (with HT expanded) from LOC, FILE and LINE.
   function Location_File_Line_To_Col
     (Loc : Location_Type; File : Source_File_Entry; Line : Positive)
     return Natural;

   --  Convert LOCATION into a source file FILE and an offset POS in the
   --  file.
   procedure Location_To_File_Pos (Location : Location_Type;
                                   File : out Source_File_Entry;
                                   Pos : out Source_Ptr);

   --  Convert a FILE and an offset POS in the file into a location.
   function File_Pos_To_Location (File : Source_File_Entry; Pos : Source_Ptr)
                                 return Location_Type;

   --  Convert a FILE into a location.
   function File_To_Location (File : Source_File_Entry) return Location_Type;

   --  Convert a FILE+LINE into a position.
   --  Return Source_Ptr_Bad in case of error (LINE out of bounds).
   function File_Line_To_Position (File : Source_File_Entry; Line : Positive)
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
                                Line : out Positive;
                                Offset : out Natural);

   --  Convert FILE and POS to coordinate.
   procedure File_Pos_To_Coord (File : Source_File_Entry;
                                Pos : Source_Ptr;
                                Line_Pos : out Source_Ptr;
                                Line : out Positive;
                                Offset : out Natural);

   --  Convert a physical column to a logical column.
   --  A physical column is the offset in byte from the first byte of the line.
   --  A logical column is the position of the character when displayed.
   --  A HT (tabulation) moves the cursor to the next position multiple of the
   --  tab stop.
   --  The first character is at position 1 and at offset 0.
   function Coord_To_Col (File : Source_File_Entry;
                          Line_Pos : Source_Ptr;
                          Offset : Natural) return Natural;

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
                                   Line : out Positive;
                                   Col : out Natural);

   --  Return the line LINE from FILE (without end of line).  The line is
   --  expanded: tabs are replaced by spaces according to Tab_Stop.  This
   --  function is slow.
   function Extract_Expanded_Line (File : Source_File_Entry;
                                   Line : Positive) return String;

   --  Return the image of LOC using the "FILENAME:LINE:COL" format or
   --  "LINE:COL" format if FILENAME is false;
   function Image (Loc : Location_Type; Filename : Boolean := True)
                  return String;

   --  Initialize.
   procedure Initialize;

   --  Free all memory.
   procedure Finalize;

private
   Lines_Table_Init : Natural := 64;

   package Lines_Tables is new Dyn_Tables
     (Table_Component_Type => Source_Ptr,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1);

   --  There are several kinds of source file.
   type Source_File_Kind is
     (
      --  A *real* source file, read from the filesystem.
      Source_File_File,

      --  A virtual source file, created from a string.
      Source_File_String,

      --  A duplicated source file (there is no copy however), created by
      --  an instantiation.
      Source_File_Instance
     );

   --  Data associed with a file.
   type Source_File_Record (Kind : Source_File_Kind := Source_File_File) is
      record
      --  All location between first and last belong to this file.
      First_Location : Location_Type;
      Last_Location : Location_Type;

      --  The name_id that identify this file.
      --  FIXME: what about file aliasing (links) ?
      File_Name : Name_Id;

      Directory : Name_Id;

      --  The buffer containing the file.
      Source : File_Buffer_Acc;

      --  Position of the EOT character after the file.  Also the length of
      --  the file + 1, unless there is a gap.
      File_Length : Source_Ptr;

      Checksum : File_Checksum_Id;

      case Kind is
         when Source_File_File =>
            --  Line table

            Lines : Lines_Tables.Instance;

            --  Cache for line table.
            Cache_Line : Positive;
            Cache_Pos : Source_Ptr;

            --  Gap
            Gap_Start : Source_Ptr;
            Gap_Last : Source_Ptr;

         when Source_File_String =>
            --  There is only one line.
            null;

         when Source_File_Instance =>
            --  The instance was created from REF.
            Ref : Source_File_Entry;
            --  The ultimate non-instance is BASE.
            Base : Source_File_Entry;

            Instance_Loc : Location_Type;
      end case;
   end record;

   package Source_Files is new Tables
     (Table_Index_Type => Source_File_Entry,
      Table_Component_Type => Source_File_Record,
      Table_Low_Bound => No_Source_File_Entry + 1,
      Table_Initial => 16);

   --  Debug procedures.

   --  Disp info about all source files
   procedure Debug_Source_Files;
end Files_Map;
