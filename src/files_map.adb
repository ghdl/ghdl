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
with System;
with Interfaces.C;
with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Tables;
with GNAT.OS_Lib;
with GNAT.SHA1;
with GNAT.Directory_Operations;
with Name_Table; use Name_Table;
with Str_Table;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;

package body Files_Map is

   -- Check validity of FILE.
   -- Raise an exception in case of error.
   procedure Check_File (File: in Source_File_Entry);

   type Lines_Table_Type is array (Positive) of Source_Ptr;
   type Lines_Table_Ptr is access all Lines_Table_Type;

   --  Data associed with a file.
   type Source_File_Record is record
      --  All location between first and last belong to this file.
      First_Location : Location_Type;
      Last_Location : Location_Type;

      -- The name_id that identify this file.
      -- FIXME: what about file aliasing (links) ?
      File_Name: Name_Id;

      Directory : Name_Id;

      -- The buffer containing the file.
      Source: File_Buffer_Acc;

      -- Length of the file, which is also the length of the buffer.
      File_Length: Natural;

      Checksum : File_Checksum_Id;

      --  Current number of line in Lines_Table.
      Nbr_Lines: Natural;

      Lines_Table: Lines_Table_Ptr;

      --  Current size of Lines_Table.
      Lines_Table_Max: Natural;

      --  Cache.
      Cache_Line : Natural;
      Cache_Pos : Source_Ptr;
   end record;

   --  Next location to use.
   Next_Location : Location_Type := Location_Nil + 1;

   package Source_Files is new Tables
     (Table_Index_Type => Source_File_Entry,
      Table_Component_Type => Source_File_Record,
      Table_Low_Bound => No_Source_File_Entry + 1,
      Table_Initial => 16);

   function Get_Last_Source_File_Entry return Source_File_Entry is
   begin
      return Source_Files.Last;
   end Get_Last_Source_File_Entry;

   Home_Dir : Name_Id := Null_Identifier;

   function Get_Home_Directory return Name_Id is
   begin
      if Home_Dir = Null_Identifier then
         GNAT.Directory_Operations.Get_Current_Dir (Nam_Buffer, Nam_Length);
         Home_Dir := Get_Identifier;
      end if;
      return Home_Dir;
   end Get_Home_Directory;

   procedure Location_To_File_Pos (Location : Location_Type;
                                   File : out Source_File_Entry;
                                   Pos : out Source_Ptr)
   is
   begin
      --  FIXME: use a cache
      --  FIXME: dicotomy
      for I in Source_Files.First .. Source_Files.Last loop
         declare
            F : Source_File_Record renames Source_Files.Table (I);
         begin
            if Location >= F.First_Location
              and then Location <= F.Last_Location
            then
               File := I;
               Pos := Source_Ptr (Location - F.First_Location);
               return;
            end if;
         end;
      end loop;
      --  File not found, location must be bad...
      raise Internal_Error;
   end Location_To_File_Pos;

   function File_Pos_To_Location (File : Source_File_Entry; Pos : Source_Ptr)
     return Location_Type
   is
   begin
      if Source_Files.Table (File).Source = null then
         raise Internal_Error;
      else
         return Source_Files.Table (File).First_Location + Location_Type (Pos);
      end if;
   end File_Pos_To_Location;

   function Source_File_To_Location (File : Source_File_Entry)
     return Location_Type
   is
   begin
      return Source_Files.Table (File).First_Location;
   end Source_File_To_Location;

   procedure Reallocate_Lines_Table
     (File: in out Source_File_Record; New_Size: Natural) is
      use Interfaces.C;

      function realloc
        (memblock : Lines_Table_Ptr;
         size     : size_t)
         return     Lines_Table_Ptr;
      pragma Import (C, realloc);

      function malloc
        (size     : size_t)
         return     Lines_Table_Ptr;
      pragma Import (C, malloc);

      New_Table: Lines_Table_Ptr;
      New_Byte_Size : size_t;
   begin
      New_Byte_Size :=
        size_t(New_Size *
                Lines_Table_Type'Component_Size / System.Storage_Unit);
      if File.Lines_Table = null then
         New_Table := malloc (New_Byte_Size);
      else
         New_Table := realloc (File.Lines_Table, New_Byte_Size);
      end if;
      if New_Table = null then
         raise Storage_Error;
      else
         File.Lines_Table := New_Table;
         File.Lines_Table (File.Lines_Table_Max + 1 .. New_Size) :=
           (others => Source_Ptr_Bad);
         File.Lines_Table_Max := New_Size;
      end if;
   end Reallocate_Lines_Table;

   -- Add a new entry in the lines_table.
   -- The new entry must be the next one after the last entry.
   procedure File_Add_Line_Number
     (File: Source_File_Entry; Line: Natural; Pos: Source_Ptr) is
      Source_File: Source_File_Record renames Source_Files.Table (File);
   begin
      -- Just check File is not out of bounds.
      if File > Source_Files.Last then
         raise Internal_Error;
      end if;

      if Line = 1 then
         -- The position of the first line is well-known.
         if Pos /= Source_Ptr_Org then
            raise Internal_Error;
         end if;
      else
         -- The position of a non first line is not the well-known value.
         if Pos <= Source_Ptr_Org then
            raise Internal_Error;
         end if;
         -- Take care of scan backtracking.
         if Line <= Source_File.Nbr_Lines then
            if Source_File.Lines_Table (Line) = Source_Ptr_Bad then
               Source_File.Lines_Table (Line) := Pos;
            elsif Pos /= Source_File.Lines_Table (Line) then
               Put_Line ("file" & Source_File_Entry'Image (File)
                         & " for line" & Natural'Image (Line)
                         & " pos =" & Source_Ptr'Image (Pos)
                         & ", lines_table = "
                         & Source_Ptr'Image (Source_File.Lines_Table (Line)));
               raise Internal_Error;
            end if;
            return;
         end if;
         -- The new entry must just follow the last entry.
--          if Line /= Source_File.Nbr_Lines + 1 then
--             raise Internal_Error;
--          end if;
      end if;
      if Line > Source_File.Lines_Table_Max then
         Reallocate_Lines_Table (Source_File, (Line / 128 + 1) * 128);
      end if;
      Source_File.Lines_Table (Line) := Pos;
      if Line > Source_File.Nbr_Lines then
         Source_File.Nbr_Lines := Line;
      end if;
      -- Source_File.Nbr_Lines := Source_File.Nbr_Lines + 1;
      if False then
         Put_Line ("file" & Source_File_Entry'Image (File)
                   & " line" & Natural'Image (Line)
                   & " at position" & Source_Ptr'Image (Pos));
      end if;
   end File_Add_Line_Number;

   --  Convert a physical column to a logical column.
   --  A physical column is the offset in byte from the first byte of the line.
   --  A logical column is the position of the character when displayed.
   --  A HT (tabulation) moves the cursor to the next position multiple of 8.
   --  The first character is at position 1 and at offset 0.
   procedure Coord_To_Position
     (File : Source_File_Entry;
      Line_Pos : Source_Ptr;
      Offset : Natural;
      Name : out Name_Id;
      Col : out Natural)
   is
      Source_File: Source_File_Record renames Source_Files.Table (File);
      Res : Positive := 1;
   begin
      Name := Source_File.File_Name;
      for I in Line_Pos .. Line_Pos + Source_Ptr (Offset) - 1 loop
         if Source_File.Source (I) = Ada.Characters.Latin_1.HT then
            Res := Res + 8 - Res mod 8;
         else
            Res := Res + 1;
         end if;
      end loop;
      Col := Res;
   end Coord_To_Position;

   --  Should only be called by Location_To_Coord.
   function Location_To_Line
     (Source_File : Source_File_Record; Pos : Source_Ptr)
     return Natural
   is
      Low, Hi, Mid : Natural;
      Mid1 : Natural;
      Lines_Table : constant Lines_Table_Ptr := Source_File.Lines_Table;
   begin
      --  Look in the cache.
      if Pos >= Source_File.Cache_Pos then
         Low := Source_File.Cache_Line;
         Hi := Source_File.Nbr_Lines;
      else
         Low := 1;
         Hi := Source_File.Cache_Line;
      end if;

      loop
         << Again >> null;
         Mid := (Hi + Low) / 2;
         if Lines_Table (Mid) = Source_Ptr_Bad then
            -- There is a hole: no position for this line.
            -- Set MID1 to a line which has a position.
            -- Try downward.
            Mid1 := Mid;
            while Lines_Table (Mid1) = Source_Ptr_Bad loop
               --  Note: Low may have no line.
               exit when Mid1 = Low;
               Mid1 := Mid1 - 1;
            end loop;
            if Mid1 /= Low then
               --  Mid1 has a line.
               if Pos < Lines_Table (Mid1) then
                  Hi := Mid1;
                  goto Again;
               end if;
               if Pos > Lines_Table (Mid1) then
                  Low := Mid1;
                  goto Again;
               end if;
               --  Found, handled just below.
            else
               --  Failed (downward is LOW): try upward.
               Mid1 := Mid;
               while Lines_Table (Mid1) = Source_Ptr_Bad loop
                  Mid1 := Mid1 + 1;
               end loop;
               if Mid1 = Hi then
                  --  Failed: no lines between LOW and HI.
                  if Pos >= Lines_Table (Hi) then
                     Mid1 := Hi;
                  else
                     Mid1 := Low;
                  end if;
                  return Mid1;
               end if;
               --  Mid1 has a line.
               if Pos < Lines_Table (Mid1) then
                  Hi := Mid1;
                  goto Again;
               end if;
               if Pos > Lines_Table (Mid1) then
                  Low := Mid1;
                  goto Again;
               end if;
            end if;
            Mid := Mid1;
         end if;
         if Pos >= Lines_Table (Mid) then
            if Mid = Source_File.Nbr_Lines
              or else Pos < Lines_Table (Mid + 1)
              or else Pos = Lines_Table (Mid)
              or else (Hi <= Mid + 1
                       and Lines_Table (Mid + 1) = Source_Ptr_Bad)
            then
               return Mid;
            end if;
         end if;
         if Pos < Lines_Table (Mid) then
            Hi := Mid - 1;
         else
            if Lines_Table (Mid + 1) /= Source_Ptr_Bad then
               Low := Mid + 1;
            else
               Low := Mid;
            end if;
         end if;
      end loop;
   end Location_To_Line;

   procedure Location_To_Coord
     (Source_File : in out Source_File_Record;
      Pos : Source_Ptr;
      Line_Pos : out Source_Ptr;
      Line : out Natural;
      Offset : out Natural)
   is
      Line_P : Source_Ptr;
      Line_Threshold : constant Natural := 4;
      Low, Hi : Natural;
   begin
      --  Look in the cache.
      if Pos >= Source_File.Cache_Pos then
         Low := Source_File.Cache_Line;
         Hi := Source_File.Nbr_Lines;

         --  Maybe adjust the threshold.
         --  Quick look.
         if Pos - Source_File.Cache_Pos <= 120
           and then Low + Line_Threshold <= Hi
         then
            for I in 1 .. Line_Threshold loop
               Line_P := Source_File.Lines_Table (Low + I);
               if Line_P > Pos then
                  Line := Low + I - 1;
                  goto Found;
               else
                  exit when Line_P = Source_Ptr_Bad;
               end if;
            end loop;
         end if;
      end if;

      Line := Location_To_Line (Source_File, Pos);

      << Found >> null;

      Line_Pos := Source_File.Lines_Table (Line);
      Offset := Natural (Pos - Source_File.Lines_Table (Line));

      --  Update cache.
      Source_File.Cache_Pos := Pos;
      Source_File.Cache_Line := Line;
   end Location_To_Coord;

   procedure Location_To_Position
     (Location : Location_Type;
      Name : out Name_Id;
      Line : out Natural;
      Col : out Natural)
   is
      File : Source_File_Entry;
      Line_Pos : Source_Ptr;
      Offset : Natural;
   begin
      Location_To_Coord (Location, File, Line_Pos, Line, Offset);
      Coord_To_Position (File, Line_Pos, Offset, Name, Col);
   end Location_To_Position;

   procedure Location_To_Coord
     (Location : Location_Type;
      File : out Source_File_Entry;
      Line_Pos : out Source_Ptr;
      Line : out Natural;
      Offset : out Natural)
   is
      Pos : Source_Ptr;
   begin
      Location_To_File_Pos (Location, File, Pos);
      Location_To_Coord (Source_Files.Table (File), Pos,
                         Line_Pos, Line, Offset);
   end Location_To_Coord;

   -- Convert the first digit of VAL into a character (base 10).
   function Digit_To_Char (Val: Natural) return Character is
   begin
      return Character'Val (Character'Pos ('0') + Val mod 10);
   end Digit_To_Char;

   function Get_Os_Time_Stamp return Time_Stamp_Id
   is
      use Ada.Calendar;
      use Ada.Calendar.Time_Zones;
      use Str_Table;

      Now : constant Time := Clock;
      Now_UTC : constant Time := Now - Duration (UTC_Time_Offset (Now) * 60);
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Sec : Day_Duration;
      S : Integer;
      S1 : Integer;
      M : Integer;
      Res: Time_Stamp_Id;
   begin
      --  Use UTC time (like file time stamp).
      Split (Now_UTC, Year, Month, Day, Sec);

      Res := Time_Stamp_Id (Create_String8);
      Append_String8_Char (Digit_To_Char (Year / 1000));
      Append_String8_Char (Digit_To_Char (Year / 100));
      Append_String8_Char (Digit_To_Char (Year / 10));
      Append_String8_Char (Digit_To_Char (Year / 1));
      Append_String8_Char (Digit_To_Char (Month / 10));
      Append_String8_Char (Digit_To_Char (Month / 1));
      Append_String8_Char (Digit_To_Char (Day / 10));
      Append_String8_Char (Digit_To_Char (Day / 1));
      S := Integer (Sec);
      if Day_Duration (S) > Sec then
         --  We need a truncation.
         S := S - 1;
      end if;
      S1 := S / 3600;
      Append_String8_Char (Digit_To_Char (S1 / 10));
      Append_String8_Char (Digit_To_Char (S1));
      S1 := (S / 60) mod 60;
      Append_String8_Char (Digit_To_Char (S1 / 10));
      Append_String8_Char (Digit_To_Char (S1));
      S1 := S mod 60;
      Append_String8_Char (Digit_To_Char (S1 / 10));
      Append_String8_Char (Digit_To_Char (S1));

      Append_String8_Char ('.');
      Sec := Sec - Day_Duration (S);
      M := Integer (Sec * 1000);
      if M = 1000 then
         --  We need truncation.
         M := 999;
      end if;
      Append_String8_Char (Digit_To_Char (M / 100));
      Append_String8_Char (Digit_To_Char (M / 10));
      Append_String8_Char (Digit_To_Char (M));
      return Res;
   end Get_Os_Time_Stamp;

   function Get_Pathname
     (Directory : Name_Id; Name : Name_Id; Add_Nul : Boolean) return String
   is
      L : Natural;
   begin
      Image (Name);
      if not GNAT.OS_Lib.Is_Absolute_Path (Nam_Buffer (1 .. Nam_Length)) then
         L := Nam_Length;
         Image (Directory);
         Nam_Buffer (Nam_Length + 1 .. Nam_Length + L) := Image (Name);
         Nam_Length := Nam_Length + L;
      end if;
      if Add_Nul then
         Nam_Length := Nam_Length + 1;
         Nam_Buffer (Nam_Length) := Character'Val (0);
      end if;
      return Nam_Buffer (1 .. Nam_Length);
   end Get_Pathname;

   procedure Normalize_Pathname
     (Directory : in out Name_Id; Name : in out Name_Id)
   is
      Separator_Pos : Natural;
      Filename : constant String := Image (Name);
   begin
      --  Find a directory part in NAME, return now if none.
      Separator_Pos := 0;
      for I in Filename'Range loop
         if Filename (I) = '/' or Filename (I) = '\' then
            Separator_Pos := I;
         end if;
      end loop;
      if Separator_Pos = 0 then
         return;
      end if;

      --  Move the directory part to DIRECTORY.
      if Directory /= Null_Identifier then
         Image (Directory);
      else
         Nam_Length := 0;
      end if;
      for I in Filename'First .. Separator_Pos loop
         Nam_Length := Nam_Length + 1;
         Nam_Buffer (Nam_Length) := Filename (I);
      end loop;
      Directory := Get_Identifier;
      Name := Get_Identifier (Filename (Separator_Pos + 1 .. Filename'Last));
   end Normalize_Pathname;

   --  Find a source_file by DIRECTORY and NAME.
   --  Return NO_SOURCE_FILE_ENTRY if not already opened.
   function Find_Source_File (Directory : Name_Id; Name: Name_Id)
     return Source_File_Entry
   is
   begin
      for I in Source_Files.First .. Source_Files.Last loop
         if Source_Files.Table (I).File_Name = Name
           and then Source_Files.Table (I).Directory = Directory
         then
            return I;
         end if;
      end loop;
      return No_Source_File_Entry;
   end Find_Source_File;

   -- Return an entry for a filename.
   -- The file is not loaded.
   function Create_Source_File_Entry (Directory : Name_Id; Name: Name_Id)
     return Source_File_Entry
   is
      Res: Source_File_Entry;
   begin
      if Find_Source_File (Directory, Name) /= No_Source_File_Entry then
         raise Internal_Error;
      end if;

      -- Create a new entry.
      Res := Source_Files.Allocate;
      Source_Files.Table (Res) := (First_Location => Next_Location,
                                   Last_Location => Next_Location,
                                   File_Name => Name,
                                   Directory => Directory,
                                   Checksum => No_File_Checksum_Id,
                                   Source => null,
                                   File_Length => 0,
                                   Nbr_Lines => 0,
                                   Lines_Table_Max => 0,
                                   Lines_Table => null,
                                   Cache_Pos => Source_Ptr_Org,
                                   Cache_Line => 1);
      File_Add_Line_Number (Res, 1, Source_Ptr_Org);
      return Res;
   end Create_Source_File_Entry;

   function Create_Source_File_From_String (Name: Name_Id; Content : String)
                                           return Source_File_Entry
   is
      Res : Source_File_Entry;
      Buffer: File_Buffer_Acc;
      Len : constant Source_Ptr := Source_Ptr (Content'Length);
   begin
      Res := Create_Source_File_Entry (Null_Identifier, Name);

      Buffer := new File_Buffer
        (Source_Ptr_Org .. Source_Ptr_Org + Len + 1);

      Buffer (Source_Ptr_Org .. Source_Ptr_Org + Len - 1) :=
        File_Buffer (Content);
      Buffer (Source_Ptr_Org + Len) := EOT;
      Buffer (Source_Ptr_Org + Len + 1) := EOT;

      Source_Files.Table (Res).Last_Location :=
        Next_Location + Location_Type (Len) + 1;
      Next_Location := Source_Files.Table (Res).Last_Location + 1;
      Source_Files.Table (Res).Source := Buffer;
      Source_Files.Table (Res).File_Length := Natural (Len);

      return Res;
   end Create_Source_File_From_String;

   function Create_Virtual_Source_File (Name: Name_Id)
                                       return Source_File_Entry
   is
   begin
      return Create_Source_File_From_String (Name, "");
   end Create_Virtual_Source_File;

   -- Return an entry for a filename.
   -- Load the filename if necessary.
   function Load_Source_File (Directory : Name_Id; Name: Name_Id)
                              return Source_File_Entry
   is
      use GNAT.OS_Lib;
      Fd: File_Descriptor;

      Res: Source_File_Entry;

      Length: Source_Ptr;
      Buffer: File_Buffer_Acc;
   begin
      --  If the file is already loaded, nothing to do!
      Res := Find_Source_File (Directory, Name);
      if Res /= No_Source_File_Entry then
         if Source_Files.Table (Res).Source = null then
            raise Internal_Error;
         end if;
         return Res;
      end if;

      --  Open the file (punt on non regular files).
      declare
         Filename : String := Get_Pathname (Directory, Name, True);
      begin
         if not Is_Regular_File (Filename) then
            return No_Source_File_Entry;
         end if;
         Fd := Open_Read (Filename'Address, Binary);
         if Fd = Invalid_FD then
            return No_Source_File_Entry;
         end if;
      end;

      Res := Create_Source_File_Entry (Directory, Name);

      Length := Source_Ptr (File_Length (Fd));

      Buffer :=
        new File_Buffer (Source_Ptr_Org .. Source_Ptr_Org + Length + 1);

      if Read (Fd, Buffer (Source_Ptr_Org)'Address, Integer (Length))
        /= Integer (Length)
      then
         Close (Fd);
         raise Internal_Error;
      end if;
      Buffer (Source_Ptr_Org + Length) := EOT;
      Buffer (Source_Ptr_Org + Length + 1) := EOT;

      if Source_Files.Table (Res).First_Location /= Next_Location then
         --  Load_Source_File call must follow its Create_Source_File.
         raise Internal_Error;
      end if;

      declare
         use GNAT.SHA1;
         use Str_Table;

         subtype Buffer_String is String (1 .. Buffer'Length - 2);
         Buffer_Digest : constant Message_Digest :=
           Digest (Buffer_String
                     (Buffer (Source_Ptr_Org .. Source_Ptr_Org + Length - 1)));
      begin
         Source_Files.Table (Res).Checksum :=
           File_Checksum_Id (Create_String8);
         for I in Buffer_Digest'Range loop
            Append_String8_Char (Buffer_Digest (I));
         end loop;
      end;

      Source_Files.Table (Res).Last_Location :=
        Next_Location + Location_Type (Length) + 1;
      Next_Location := Source_Files.Table (Res).Last_Location + 1;
      Source_Files.Table (Res).Source := Buffer;
      Source_Files.Table (Res).File_Length := Integer (Length);

      Close (Fd);

      return Res;
   end Load_Source_File;

   -- Check validity of FILE.
   -- Raise an exception in case of error.
   procedure Check_File (File: in Source_File_Entry) is
   begin
      if File > Source_Files.Last then
         raise Internal_Error;
      end if;
   end Check_File;

   -- Return a buffer (access to the contents of the file) for a file entry.
   function Get_File_Source (File: Source_File_Entry)
                             return File_Buffer_Acc is
   begin
      Check_File (File);
      return Source_Files.Table (File).Source;
   end Get_File_Source;

   -- Return the length of the file (which is the size of the file buffer).
   function Get_File_Length (File: Source_File_Entry) return Source_Ptr is
   begin
      Check_File (File);
      return Source_Ptr (Source_Files.Table (File).File_Length);
   end Get_File_Length;

   -- Return the name of the file.
   function Get_File_Name (File: Source_File_Entry) return Name_Id is
   begin
      Check_File (File);
      return Source_Files.Table (File).File_Name;
   end Get_File_Name;

   function Get_File_Checksum (File : Source_File_Entry)
                              return File_Checksum_Id is
   begin
      Check_File (File);
      return Source_Files.Table (File).Checksum;
   end Get_File_Checksum;

   function Get_Source_File_Directory (File : Source_File_Entry)
                                       return Name_Id is
   begin
      Check_File (File);
      return Source_Files.Table (File).Directory;
   end Get_Source_File_Directory;

   function Line_To_Position (File : Source_File_Entry; Line : Natural)
                             return Source_Ptr
   is
   begin
      Check_File (File);
      if Line > Source_Files.Table (File).Nbr_Lines then
         return Source_Ptr_Bad;
      else
         return Source_Files.Table (File).Lines_Table (Line);
      end if;
   end Line_To_Position;

   function Is_Eq (L : Time_Stamp_Id; R : Time_Stamp_Id) return Boolean
   is
      use Str_Table;
      L_Str : constant String8_Id := String8_Id (L);
      R_Str : constant String8_Id := String8_Id (R);
   begin
      for I in 1 .. Nat32 (Time_Stamp_String'Length) loop
         if Element_String8 (L_Str, I) /= Element_String8 (R_Str, I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Eq;

   function Is_Eq (L, R : File_Checksum_Id) return Boolean
   is
      use Str_Table;
      L_Str : constant String8_Id := String8_Id (L);
      R_Str : constant String8_Id := String8_Id (R);
   begin
      for I in 1 .. Nat32 (File_Checksum_String'Length) loop
         if Element_String8 (L_Str, I) /= Element_String8 (R_Str, I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Eq;


   function Is_Gt (L : Time_Stamp_Id; R : Time_Stamp_Id) return Boolean
   is
      use Str_Table;
      L_Str : constant String8_Id := String8_Id (L);
      R_Str : constant String8_Id := String8_Id (R);
      E_L, E_R : Nat8;
   begin
      for I in 1 .. Nat32 (Time_Stamp_String'Length) loop
         E_L := Element_String8 (L_Str, I);
         E_R := Element_String8 (R_Str, I);
         if E_L /= E_R then
            return E_L > E_R;
         end if;
      end loop;
      return False;
   end Is_Gt;

   function Get_Time_Stamp_String (Ts : Time_Stamp_Id) return String is
   begin
      if Ts = Null_Time_Stamp then
         return "NULL_TS";
      else
         return Str_Table.String_String8
           (String8_Id (Ts), Time_Stamp_String'Length);
      end if;
   end Get_Time_Stamp_String;

   function Get_File_Checksum_String (Checksum : File_Checksum_Id)
                                     return String is
   begin
      if Checksum = No_File_Checksum_Id then
         return "NO_CHECKSUM";
      else
         return Str_Table.String_String8
           (String8_Id (Checksum), File_Checksum_String'Length);
      end if;
   end Get_File_Checksum_String;

   function Image (Loc : Location_Type; Filename : Boolean := True)
                  return string
   is
      Line, Col : Natural;
      Name : Name_Id;
   begin
      if Loc = Location_Nil then
         --  Avoid a crash.
         return "??:??:??";
      else
         Location_To_Position (Loc, Name, Line, Col);
         declare
            Line_Str : constant String := Natural'Image (Line);
            Col_Str : constant String := Natural'Image (Col);
         begin
            if Filename then
               return Name_Table.Image (Name)
                 & ':' & Line_Str (Line_Str'First + 1 .. Line_Str'Last)
                 & ':' & Col_Str (Col_Str'First + 1 .. Col_Str'Last);
            else
               return Line_Str (Line_Str'First + 1 .. Line_Str'Last)
                 & ':' & Col_Str (Col_Str'First + 1 .. Col_Str'Last);
            end if;
         end;
      end if;
   end Image;

   -- Debug procedures.
   procedure Debug_Source_Lines (File: Source_File_Entry);
   pragma Unreferenced (Debug_Source_Lines);

   procedure Debug_Source_File;
   pragma Unreferenced (Debug_Source_File);

   --  Disp sources lines of a file.
   procedure Debug_Source_Lines (File: Source_File_Entry) is
      Source_File: Source_File_Record renames Source_Files.Table (File);
   begin
      Check_File (File);
      for I in Positive'First .. Source_File.Nbr_Lines loop
         Put_Line ("line" & Natural'Image (I) & " at offset"
                   & Source_Ptr'Image (Source_File.Lines_Table (I)));
      end loop;
   end Debug_Source_Lines;

   procedure Debug_Source_File is
   begin
      for I in Source_Files.First .. Source_Files.Last loop
         declare
            F : Source_File_Record renames Source_Files.Table(I);
         begin
            Put ("file" & Source_File_Entry'Image (I));
            Put (" name: " & Image (F.File_Name));
            Put (" dir:" & Image (F.Directory));
            Put (" length:" & Natural'Image (F.File_Length));
            New_Line;
            if F.Checksum /= No_File_Checksum_Id then
               Put (" checksum: " & Get_File_Checksum_String (F.Checksum));
            end if;
            Put (" nbr lines:" & Natural'Image (F.Nbr_Lines));
            Put (" lines_table_max:" & Natural'Image (F.Lines_Table_Max));
            New_Line;
         end;
      end loop;
   end Debug_Source_File;

   procedure Initialize
   is
      procedure free (Ptr : Lines_Table_Ptr);
      pragma Import (C, free);

      procedure Free is new Ada.Unchecked_Deallocation
        (File_Buffer, File_Buffer_Acc);
   begin
      for I in Source_Files.First .. Source_Files.Last loop
         free (Source_Files.Table (I).Lines_Table);
         Free (Source_Files.Table (I).Source);
      end loop;
      Source_Files.Free;
      Source_Files.Init;
   end Initialize;
end Files_Map;
