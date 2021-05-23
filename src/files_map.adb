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
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with GNAT.SHA1;
with GNAT.Directory_Operations;
with Logging; use Logging;
with Name_Table; use Name_Table;
with Str_Table;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;

package body Files_Map is

   --  Check validity of FILE.
   --  Raise an exception in case of error.
   procedure Check_File (File: in Source_File_Entry);
   pragma Inline (Check_File);

   --  Next location to use.
   Next_Location : Location_Type := Location_Nil + 1;

   function Get_Last_Source_File_Entry return Source_File_Entry is
   begin
      return Source_Files.Last;
   end Get_Last_Source_File_Entry;

   Home_Dir : Name_Id := Null_Identifier;

   function Get_Home_Directory return Name_Id is
   begin
      if Home_Dir = Null_Identifier then
         declare
            Dir : constant String := GNAT.Directory_Operations.Get_Current_Dir;
         begin
            Home_Dir := Get_Identifier (Dir);
         end;
      end if;
      return Home_Dir;
   end Get_Home_Directory;

   function Location_To_File (Location : Location_Type)
                             return Source_File_Entry is
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
               return I;
            end if;
         end;
      end loop;
      return No_Source_File_Entry;
   end Location_To_File;

   procedure Location_To_File_Pos (Location : Location_Type;
                                   File : out Source_File_Entry;
                                   Pos : out Source_Ptr) is
   begin
      File := Location_To_File (Location);
      if File = No_Source_File_Entry then
         --  File not found, location must be correct.
         raise Internal_Error;
      end if;
      Pos := Location_File_To_Pos (Location, File);
   end Location_To_File_Pos;

   function File_Pos_To_Location (File : Source_File_Entry; Pos : Source_Ptr)
                                 return Location_Type
   is
      pragma Assert (File <= Source_Files.Last);
   begin
      return Source_Files.Table (File).First_Location + Location_Type (Pos);
   end File_Pos_To_Location;

   function File_To_Location (File : Source_File_Entry) return Location_Type
   is
      pragma Assert (File <= Source_Files.Last);
   begin
      return Source_Files.Table (File).First_Location;
   end File_To_Location;

   --  Add a new entry in the lines_table.
   --  The new entry must be the next one after the last entry.
   procedure File_Add_Line_Number
     (File : Source_File_Entry; Line : Positive; Pos : Source_Ptr)
   is
      use Lines_Tables;

      -- Just check File is not out of bounds.
      pragma Assert (File <= Source_Files.Last);

      Source_File: Source_File_Record renames Source_Files.Table (File);
      Old_Last : Natural;
   begin
      --  Can only add line number to a real file.
      pragma Assert (Source_File.Kind = Source_File_File);

      --  Debug trace.
      if False then
         Log_Line ("file" & Source_File_Entry'Image (File)
                     & " line" & Natural'Image (Line)
                     & " at position" & Source_Ptr'Image (Pos));
      end if;

      --  The position of the first line is well-known.
      pragma Assert (Line = 1 xor Pos /= Source_Ptr_Org);

      Old_Last := Last (Source_File.Lines);
      if Line > Old_Last then
         Allocate (Source_File.Lines, Line - Old_Last);
         Source_File.Lines.Table (Old_Last + 1 .. Line) :=
           (others => Source_Ptr_Bad);
      end if;

      --  Lines are in increasing order.
      pragma Assert
        (Line = 1
           or else Source_File.Lines.Table (Line - 1) = Source_Ptr_Bad
           or else Source_File.Lines.Table (Line - 1) < Pos);
      pragma Assert
        (Line = Last (Source_File.Lines)
           or else Source_File.Lines.Table (Line + 1) = Source_Ptr_Bad
           or else Source_File.Lines.Table (Line + 1) > Pos);

      if Source_File.Lines.Table (Line) = Source_Ptr_Bad then
         Source_File.Lines.Table (Line) := Pos;
      else
         --  If the line position is already known, it must be the same.
         if Pos /= Source_File.Lines.Table (Line) then
            Log_Line ("file" & Source_File_Entry'Image (File)
                        & " for line" & Natural'Image (Line)
                        & " pos =" & Source_Ptr'Image (Pos)
                        & ", lines_table = "
                        & Source_Ptr'Image (Source_File.Lines.Table (Line)));
            raise Internal_Error;
         end if;
      end if;
   end File_Add_Line_Number;

   --  Convert a physical column to a logical column.
   --  A physical column is the offset in byte from the first byte of the line.
   --  A logical column is the position of the character when displayed.
   --  A HT (tabulation) moves the cursor to the next position multiple of the
   --  tab stop.
   --  The first character is at position 1 and at offset 0.
   function Coord_To_Col (File : Source_File_Entry;
                          Line_Pos : Source_Ptr;
                          Offset : Natural) return Natural
   is
      Source_File: Source_File_Record renames Source_Files.Table (File);
      Res : Positive := 1;
   begin
      if Offset = 0 then
         return Res;
      else
         for I in Line_Pos .. Line_Pos + Source_Ptr (Offset) - 1 loop
            if Source_File.Source (I) = ASCII.HT then
               Res := Res + Tab_Stop - Res mod Tab_Stop;
            end if;
            Res := Res + 1;
         end loop;
         return Res;
      end if;
   end Coord_To_Col;

   procedure Coord_To_Position (File : Source_File_Entry;
                                Line_Pos : Source_Ptr;
                                Offset : Natural;
                                Name : out Name_Id;
                                Col : out Natural) is
   begin
      Name := Source_Files.Table (File).File_Name;
      Col := Coord_To_Col (File, Line_Pos, Offset);
   end Coord_To_Position;

   --  Should only be called by Location_To_Coord.
   function Location_To_Line
     (Source_File : Source_File_Record; Pos : Source_Ptr) return Natural
   is
      use Lines_Tables;
      Lines_Table : constant Table_Thin_Ptr := Source_File.Lines.Table;
      Low, Hi, Mid : Natural;
      Mid1 : Natural;
   begin
      --  Look in the cache.
      if Pos >= Source_File.Cache_Pos then
         Low := Source_File.Cache_Line;
         Hi := Last (Source_File.Lines);
      else
         Low := 1;
         Hi := Source_File.Cache_Line;
      end if;

      loop
         << Again >> null;
         pragma Assert (Hi >= Low);
         pragma Assert (Low >= 1);
         pragma Assert (Hi <= Last (Source_File.Lines));

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

         --  Mid is on a known line.
         pragma Assert (Lines_Table (Mid) /= Source_Ptr_Bad);

         if Pos >= Lines_Table (Mid) then
            if Mid = Last (Source_File.Lines)
              or else (Lines_Table (Mid + 1) /= Source_Ptr_Bad
                         and then Pos < Lines_Table (Mid + 1))
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

   --  Internal procedure
   procedure Location_To_Coord (Source_File : in out Source_File_Record;
                                Pos : Source_Ptr;
                                Line_Pos : out Source_Ptr;
                                Line : out Natural;
                                Offset : out Natural)
   is
      use Lines_Tables;
      Line_P : Source_Ptr;
      Line_Threshold : constant Natural := 4;
      Low, Hi : Natural;
   begin
      --  Look in the cache.
      if Pos >= Source_File.Cache_Pos then
         Low := Source_File.Cache_Line;
         Hi := Last (Source_File.Lines);

         --  Maybe adjust the threshold.
         --  Quick look.
         if Pos - Source_File.Cache_Pos <= 120
           and then Low + Line_Threshold <= Hi
         then
            for I in 1 .. Line_Threshold loop
               Line_P := Source_File.Lines.Table (Low + I);
               if Line_P > Pos and Line_P /= Source_Ptr_Bad then
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

      Line_Pos := Source_File.Lines.Table (Line);
      Offset := Natural (Pos - Line_Pos);

      --  Update cache.
      Source_File.Cache_Pos := Line_Pos;
      Source_File.Cache_Line := Line;
   end Location_To_Coord;

   procedure Location_To_Position (Location : Location_Type;
                                   Name : out Name_Id;
                                   Line : out Positive;
                                   Col : out Natural)
   is
      File : Source_File_Entry;
      Line_Pos : Source_Ptr;
      Offset : Natural;
   begin
      Location_To_Coord (Location, File, Line_Pos, Line, Offset);
      Coord_To_Position (File, Line_Pos, Offset, Name, Col);
   end Location_To_Position;

   procedure File_Pos_To_Coord (File : Source_File_Entry;
                                Pos : Source_Ptr;
                                Line_Pos : out Source_Ptr;
                                Line : out Positive;
                                Offset : out Natural) is
   begin
      case Source_Files.Table (File).Kind is
         when Source_File_File =>
            Location_To_Coord (Source_Files.Table (File), Pos,
                               Line_Pos, Line, Offset);
         when Source_File_String =>
            Line_Pos := Source_Ptr_Org;
            Line := 1;
            Offset := Natural (Pos - Source_Ptr_Org);
         when Source_File_Instance =>
            declare
               Base : constant Source_File_Entry :=
                 Source_Files.Table (File).Base;
            begin
               Location_To_Coord (Source_Files.Table (Base), Pos,
                                  Line_Pos, Line, Offset);
            end;
      end case;
   end File_Pos_To_Coord;

   procedure Location_To_Coord (Location : Location_Type;
                                File : out Source_File_Entry;
                                Line_Pos : out Source_Ptr;
                                Line : out Positive;
                                Offset : out Natural)
   is
      Pos : Source_Ptr;
   begin
      --  Get FILE and position POS in the file.
      Location_To_File_Pos (Location, File, Pos);
      File_Pos_To_Coord (File, Pos, Line_Pos, Line, Offset);
   end Location_To_Coord;

   function Location_File_To_Pos
     (Location : Location_Type; File : Source_File_Entry) return Source_Ptr is
   begin
      return Source_Ptr (Location - Source_Files.Table (File).First_Location);
   end Location_File_To_Pos;

   function Location_File_To_Line
     (Location : Location_Type; File : Source_File_Entry) return Positive
   is
      Line_Pos : Source_Ptr;
      Line     : Positive;
      Offset   : Natural;
   begin
      Location_To_Coord
        (Source_Files.Table (File), Location_File_To_Pos (Location, File),
         Line_Pos, Line, Offset);
      return Line;
   end Location_File_To_Line;

   function Location_File_Line_To_Col
     (Loc : Location_Type; File : Source_File_Entry; Line : Positive)
     return Natural
   is
      F : Source_File_Record renames Source_Files.Table (File);
      Line_Pos : constant Source_Ptr := F.Lines.Table (Line);
      Pos : constant Source_Ptr := Location_File_To_Pos (Loc, File);
   begin
      return Coord_To_Col (File, Line_Pos, Natural (Pos - Line_Pos));
   end Location_File_Line_To_Col;

   function Location_File_Line_To_Offset
     (Loc : Location_Type; File : Source_File_Entry; Line : Positive)
     return Natural
   is
      F : Source_File_Record renames Source_Files.Table (File);
      Line_Pos : constant Source_Ptr := F.Lines.Table (Line);
      Pos : constant Source_Ptr := Location_File_To_Pos (Loc, File);
   begin
      return Natural (Pos - Line_Pos);
   end Location_File_Line_To_Offset;

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

   function Get_Pathname (Directory : Name_Id; Name : Name_Id) return String
   is
      Filename : constant String := Image (Name);
   begin
      if not GNAT.OS_Lib.Is_Absolute_Path (Filename) then
         return Image (Directory) & Filename;
      else
         return Filename;
      end if;
   end Get_Pathname;

   procedure Normalize_Pathname
     (Directory : in out Name_Id; Name : in out Name_Id)
   is
      Filename : constant String := Image (Name);
      Separator_Pos : Natural;
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
      declare
         File_Dir : constant String :=
           Filename (Filename'First .. Separator_Pos);
      begin
         if Directory /= Null_Identifier then
            Directory := Get_Identifier (Image (Directory) & File_Dir);
         else
            Directory := Get_Identifier (File_Dir);
         end if;
      end;
      Name := Get_Identifier (Filename (Separator_Pos + 1 .. Filename'Last));
   end Normalize_Pathname;

   function Find_Language (Filename : String) return Language_Type
   is
      P, E : Natural;
      Ext : String (1 .. 5);
   begin
      P := Filename'Last;
      E := Ext'Last;
      loop
         if P <= Filename'First
           or else E < Ext'First
         then
            return Language_Unknown;
         end if;
         case Filename (P) is
            when 'a' .. 'z' =>
               Ext (E) := Filename (P);
            when 'A' .. 'Z' =>
               Ext (E) := Character'Val (Character'Pos (Filename (P))
                                           - Character'Pos ('A')
                                           + Character'Pos ('a'));
            when '.' =>
               if Ext (E + 1 .. Ext'Last) = "vhd"
                 or else Ext (E + 1 .. Ext'Last) = "vhdl"
               then
                  return Language_Vhdl;
               end if;
               if Ext (E + 1 .. Ext'Last) = "v"
                 or else Ext (E + 1 .. Ext'Last) = "v"
                 or else Ext (E + 1 .. Ext'Last) = "sv"
                 or else Ext (E + 1 .. Ext'Last) = "svh"
               then
                  return Language_Verilog;
               end if;
               if Ext (E + 1 .. Ext'Last) = "psl" then
                  return Language_Psl;
               end if;
            when others =>
               return Language_Unknown;
         end case;
         P := P - 1;
         E := E - 1;
      end loop;
   end Find_Language;

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

   --  Return an entry for a filename.
   --  The file is not loaded.
   function Create_Source_File_Entry (Directory : Name_Id; Name: Name_Id)
                                     return Source_File_Entry
   is
      Res: Source_File_Entry;
   begin
      --  File must not already exist.
      pragma Assert
        (Find_Source_File (Directory, Name) = No_Source_File_Entry);

      --  Create a new entry.
      Res := Source_Files.Allocate;
      Source_Files.Table (Res) := (Kind => Source_File_File,
                                   First_Location => Next_Location,
                                   Last_Location => Next_Location,
                                   File_Name => Name,
                                   Directory => Directory,
                                   Checksum => No_File_Checksum_Id,
                                   Source => null,
                                   File_Length => 0,
                                   Lines => <>,
                                   Cache_Pos => Source_Ptr_Org,
                                   Cache_Line => 1,
                                   Gap_Start => Source_Ptr_Last,
                                   Gap_Last => Source_Ptr_Last);
      Lines_Tables.Init (Source_Files.Table (Res).Lines, Lines_Table_Init);
      File_Add_Line_Number (Res, 1, Source_Ptr_Org);
      return Res;
   end Create_Source_File_Entry;

   function Create_Source_File_From_String (Name: Name_Id; Content : String)
                                           return Source_File_Entry
   is
      Len : constant Source_Ptr := Source_Ptr (Content'Length);
      Res : Source_File_Entry;
      Buffer: File_Buffer_Acc;
   begin
      --  Fill buffer.
      Buffer := new File_Buffer
        (Source_Ptr_Org .. Source_Ptr_Org + Len + 1);

      if Len /= 0 then
         Buffer (Source_Ptr_Org .. Source_Ptr_Org + Len - 1) :=
           File_Buffer (Content);
      end if;

      --  Create entry.
      Res := Source_Files.Allocate;
      Source_Files.Table (Res) :=
        (Kind => Source_File_String,
         First_Location => Next_Location,
         Last_Location => Next_Location + Location_Type (Len) + 1,
         File_Name => Name,
         Directory => Null_Identifier,
         Checksum => No_File_Checksum_Id,
         Source => Buffer,
         File_Length => 0);

      Set_File_Length (Res, Len);

      Next_Location := Source_Files.Table (Res).Last_Location + 1;

      return Res;
   end Create_Source_File_From_String;

   function Create_Virtual_Source_File (Name: Name_Id)
                                       return Source_File_Entry is
   begin
      return Create_Source_File_From_String (Name, "");
   end Create_Virtual_Source_File;

   function Create_Instance_Source_File (Ref : Source_File_Entry;
                                         Loc : Location_Type;
                                         Inst : Vhdl.Types.Vhdl_Node)
                                        return Source_File_Entry
   is
      pragma Unreferenced (Inst);
      Base : Source_File_Entry;
      Res : Source_File_Entry;
   begin
      if Source_Files.Table (Ref).Kind = Source_File_Instance then
         Base := Source_Files.Table (Ref).Base;
      else
         Base := Ref;
      end if;

      --  Create entry.
      Res := Source_Files.Allocate;

      declare
         F : Source_File_Record renames Source_Files.Table (Base);
      begin
         Source_Files.Table (Res) :=
           (Kind => Source_File_Instance,
            First_Location => Next_Location,
            Last_Location => Next_Location + Location_Type (F.File_Length) + 1,
            File_Name => F.File_Name,
            Directory => F.Directory,
            Checksum => F.Checksum,
            Source => F.Source,
            File_Length => F.File_Length,
            Ref => Ref,
            Base => Base,
            Instance_Loc => Loc);

         Next_Location := Source_Files.Table (Res).Last_Location + 1;
      end;

      return Res;
   end Create_Instance_Source_File;

   function Instance_Relocate
     (Inst_File : Source_File_Entry; Loc : Location_Type)
     return Location_Type
   is
      pragma Assert (Inst_File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (Inst_File);
      pragma Assert (F.Kind = Source_File_Instance);
      R : Source_File_Record renames Source_Files.Table (F.Ref);
   begin
      if Loc >= R.First_Location and Loc <= R.Last_Location then
         return F.First_Location + (Loc - R.First_Location);
      else
         return Loc;
      end if;
   end Instance_Relocate;

   function Location_Instance_To_Location
     (Loc : Location_Type) return Location_Type
   is
      File : Source_File_Entry;
      Pos :  Source_Ptr;
   begin
      if Loc = No_Location then
         return No_Location;
      end if;

      Location_To_File_Pos (Loc, File, Pos);
      if Source_Files.Table (File).Kind = Source_File_Instance then
         return Source_Files.Table (File).Instance_Loc;
      else
         return No_Location;
      end if;
   end Location_Instance_To_Location;

   function Reserve_Source_File
     (Directory : Name_Id; Name: Name_Id; Length : Source_Ptr)
     return Source_File_Entry
   is
      pragma Assert (Length >= 2);
      Res : Source_File_Entry;
   begin
      Res := Create_Source_File_Entry (Directory, Name);

      declare
         F : Source_File_Record renames Source_Files.Table (Res);
      begin
         F.Source := new File_Buffer (Source_Ptr_Org
                                        .. Source_Ptr_Org + Length - 1);

         --  Read_Source_File call must follow its Create_Source_File.
         pragma Assert (F.First_Location = Next_Location);

         F.Last_Location := Next_Location + Location_Type (Length) - 1;
         Next_Location := F.Last_Location + 1;
      end;

      return Res;
   end Reserve_Source_File;

   --  Return an entry for a filename.
   --  Load the filename if necessary.
   function Read_Source_File (Directory : Name_Id; Name: Name_Id)
                             return Source_File_Entry
   is
      use GNAT.OS_Lib;
      Fd : File_Descriptor;

      Res : Source_File_Entry;

      Raw_Length : Long_Integer;
      Length : Source_Ptr;

      Buffer : File_Buffer_Acc;
   begin
      --  The file is not supposed to be already loaded, but this could happen
      --  if the same file is compiled in two libraries.
      Res := Find_Source_File (Directory, Name);
      if Res /= No_Source_File_Entry then
         return Res;
      end if;

      --  Open the file (punt on non regular files).
      declare
         Filename : constant String := Get_Pathname (Directory, Name);
         Filename0 : constant String := Filename & ASCII.NUL;
      begin
         if not Is_Regular_File (Filename) then
            return No_Source_File_Entry;
         end if;
         Fd := Open_Read (Filename0'Address, Binary);
         if Fd = Invalid_FD then
            return No_Source_File_Entry;
         end if;
      end;

      Raw_Length := File_Length (Fd);

      --  Check for too large files.  Use 'Pos (ie universal integer) to avoid
      --  errors in conversions.
      if Long_Integer'Pos (Raw_Length) > Source_Ptr'Pos (Source_Ptr'Last)
        or else Long_Integer'Pos (Raw_Length) > Integer'Pos (Integer'Last)
      then
         Close (Fd);
         return No_Source_File_Entry;
      end if;

      Length := Source_Ptr (Raw_Length);

      Res := Reserve_Source_File (Directory, Name, Length + 2);
      if Res = No_Source_File_Entry then
         Close (Fd);
         return No_Source_File_Entry;
      end if;

      Buffer := Get_File_Source (Res);

      if Read (Fd, Buffer (Source_Ptr_Org)'Address, Integer (Length))
        /= Integer (Length)
      then
         Close (Fd);
         raise Internal_Error;
      end if;
      Close (Fd);

      Set_File_Length (Res, Length);

      --  Set the gap.
      Source_Files.Table (Res).Gap_Start :=
        Source_Ptr_Org + Length + 2;
      Source_Files.Table (Res).Gap_Last :=
        Source_Files.Table (Res).Source'Last;

      --  Compute the SHA1.
      declare
         use GNAT.SHA1;
         use Str_Table;

         subtype Buffer_String is String (1 .. Buffer'Length - 2);
         Buffer_Digest : Message_Digest;
      begin
         if Length /= 0 then
            --  Avoid weird bounds for empty buffers.
            Buffer_Digest := Digest
              (Buffer_String
                 (Buffer (Source_Ptr_Org .. Source_Ptr_Org + Length - 1)));
         end if;

         Source_Files.Table (Res).Checksum :=
           File_Checksum_Id (Create_String8);
         for I in Buffer_Digest'Range loop
            Append_String8_Char (Buffer_Digest (I));
         end loop;
      end;

      return Res;
   end Read_Source_File;

   procedure Discard_Source_File (File : Source_File_Entry)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      F.File_Name := Null_Identifier;
      F.Directory := Null_Identifier;
   end Discard_Source_File;

   procedure Free_Source_File (File : Source_File_Entry)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (File_Buffer, File_Buffer_Acc);

      F : Source_File_Record renames Source_Files.Table (File);
   begin
      case F.Kind is
         when Source_File_File =>
            Lines_Tables.Free (F.Lines);
            Free (F.Source);
         when Source_File_String =>
            Free (F.Source);
         when Source_File_Instance =>
            null;
      end case;
   end Free_Source_File;

   procedure Unload_Last_Source_File (File : Source_File_Entry) is
   begin
      pragma Assert (File = Source_Files.Last);
      Free_Source_File (File);
      Source_Files.Decrement_Last;
      Next_Location :=
        Source_Files.Table (Source_Files.Last).Last_Location + 1;
   end Unload_Last_Source_File;

   procedure Skip_Gap (File : Source_File_Entry; Pos : in out Source_Ptr)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      if Pos = F.Gap_Start then
         Pos := F.Gap_Last + 1;
      end if;
   end Skip_Gap;

   --  Check validity of FILE.
   --  Raise an exception in case of error.
   procedure Check_File (File : Source_File_Entry) is
   begin
      pragma Assert (File <= Source_Files.Last);
      null;
   end Check_File;

   --  Return a buffer (access to the contents of the file) for a file entry.
   function Get_File_Source (File: Source_File_Entry)
                            return File_Buffer_Acc is
   begin
      Check_File (File);
      return Source_Files.Table (File).Source;
   end Get_File_Source;

   function Get_File_Buffer (File : Source_File_Entry)
                            return File_Buffer_Ptr is
   begin
      return To_File_Buffer_Ptr
        (Source_Files.Table (File).Source (Source_Ptr_Org)'Address);
   end Get_File_Buffer;

   procedure Set_File_Length (File : Source_File_Entry; Length : Source_Ptr) is
   begin
      Check_File (File);
      declare
         F : Source_File_Record renames Source_Files.Table (File);
         Buffer : File_Buffer_Acc renames F.Source;
      begin
         pragma Assert (Length <= Buffer'Length - 2);

         F.File_Length := Length;
         Buffer (Source_Ptr_Org + Length) := EOT;
         Buffer (Source_Ptr_Org + Length + 1) := EOT;
      end;
   end Set_File_Length;

   function Get_File_Length (File: Source_File_Entry) return Source_Ptr is
   begin
      Check_File (File);
      return Source_Files.Table (File).File_Length;
   end Get_File_Length;

   function Get_Content_Length (File : Source_File_Entry) return Source_Ptr
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      if F.Gap_Start >= F.File_Length then
         return F.File_Length;
      else
         return F.File_Length - (F.Gap_Last - F.Gap_Start + 1);
      end if;
   end Get_Content_Length;

   function Get_Buffer_Length (File : Source_File_Entry) return Source_Ptr
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      return Source_Ptr (F.Last_Location - F.First_Location + 1);
   end Get_Buffer_Length;

   --  Return the name of the file.
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

   function Get_Directory_Name (File : Source_File_Entry) return Name_Id is
   begin
      Check_File (File);
      return Source_Files.Table (File).Directory;
   end Get_Directory_Name;

   function File_Line_To_Position (File : Source_File_Entry; Line : Positive)
                                  return Source_Ptr
   is
      pragma Assert (File <= Source_Files.Last);
      Source_File: Source_File_Record renames Source_Files.Table (File);
   begin
      case Source_File.Kind is
         when Source_File_File =>
            if Line > Lines_Tables.Last (Source_File.Lines) then
               return Source_Ptr_Bad;
            else
               return Source_File.Lines.Table (Line);
            end if;
         when Source_File_String =>
            if Line /= 1 then
               return Source_Ptr_Bad;
            else
               return Source_Ptr_Org;
            end if;
         when Source_File_Instance =>
            return File_Line_To_Position (Source_File.Base, Line);
      end case;
   end File_Line_To_Position;

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
      end if;

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
   end Image;

   --  Compute the length of line that starts at START.  Tabs are expanded to
   --  compute the length.
   function Compute_Expanded_Line_Length (File : Source_File_Entry;
                                          Start : Source_Ptr) return Natural
   is
      Buf : constant File_Buffer_Acc := Get_File_Source (File);
      Pos : Source_Ptr;
      Len : Natural;
      C : Character;
   begin
      --  Compute line length.
      Pos := Start;
      Len := 0;
      loop
         C := Buf (Pos);
         Pos := Pos + 1;
         exit when C = ASCII.CR or C = ASCII.LF or C = ASCII.EOT;
         if C = ASCII.HT then
            --  Expand tab.
            Len := Len + (Tab_Stop - Len mod Tab_Stop);
         else
            Len := Len + 1;
         end if;
      end loop;
      return Len;
   end Compute_Expanded_Line_Length;

   --  Return the line that starts at START in FILE.  This is slow.
   function Extract_Expanded_Line (File : Source_File_Entry;
                                   Start : Source_Ptr) return String
   is
      Buf : constant File_Buffer_Acc := Get_File_Source (File);
      Len : constant Natural := Compute_Expanded_Line_Length (File, Start);
      Res : String (1 .. Len);
      P : Natural;
      Pos : Source_Ptr;
      C : Character;
   begin
      Pos := Start;
      P := 0;
      loop
         C := Buf (Pos);
         Pos := Pos + 1;
         exit when C = ASCII.CR or C = ASCII.LF or C = ASCII.EOT;
         if C = ASCII.HT then
            --  Expand tab.
            loop
               P := P + 1;
               Res (P) := ' ';
               exit when P mod Tab_Stop = 0;
            end loop;
         else
            P := P + 1;
            Res (P) := C;
         end if;
      end loop;
      pragma Assert (P = Res'Last);
      return Res;
   end Extract_Expanded_Line;

   function Extract_Expanded_Line (File : Source_File_Entry;
                                   Line : Positive) return String
   is
      Start : constant Source_Ptr := File_Line_To_Position (File, Line);
   begin
      return Extract_Expanded_Line (File, Start);
   end Extract_Expanded_Line;

   -- Debug procedures.
   procedure Debug_Source_Loc (Loc : Location_Type)
   is
      File : Source_File_Entry;
      Line_Pos : Source_Ptr;
      Line : Natural;
      Offset : Natural;
   begin
      Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
      Log_Line (Extract_Expanded_Line (File, Line_Pos));
   end Debug_Source_Loc;

   --  Disp sources lines of a file.
   procedure Debug_Source_Lines (File: Source_File_Entry) is
      Source_File: Source_File_Record renames Source_Files.Table (File);
   begin
      Check_File (File);
      for I in Lines_Tables.First .. Lines_Tables.Last (Source_File.Lines) loop
         Log_Line ("line" & Natural'Image (I) & " at offset"
                     & Source_Ptr'Image (Source_File.Lines.Table (I)));
      end loop;
   end Debug_Source_Lines;

   procedure Debug_Source_File (File : Source_File_Entry)
   is
      F : Source_File_Record renames Source_Files.Table(File);
   begin
      Log ("*");
      Log (Source_File_Entry'Image (File));
      Log (" name: " & Image (F.File_Name));
      Log (" dir:" & Image (F.Directory));
      Log (" file length:" & Source_Ptr'Image (F.File_Length));
      Log_Line;
      Log (" location:" & Location_Type'Image (F.First_Location)
             & " -" & Location_Type'Image (F.Last_Location));
      Log_Line;
      if F.Checksum /= No_File_Checksum_Id then
         Log (" checksum: " & Get_File_Checksum_String (F.Checksum));
         Log_Line;
      end if;
      case F.Kind is
         when Source_File_File =>
            if F.Source = null then
               Log (" no buf");
            else
               Log (" buf:" & Source_Ptr'Image (F.Source'First)
                      & " -" & Source_Ptr'Image (F.Source'Last));
            end if;
            Log_Line;
            Log (" nbr lines:"
                   & Natural'Image (Lines_Tables.Last (F.Lines)));
            Log_Line;
            Log (" Gap:" & Source_Ptr'Image (F.Gap_Start)
                   & " -" & Source_Ptr'Image (F.Gap_Last));
            Log_Line;
         when Source_File_String =>
            null;
         when Source_File_Instance =>
            Log (" instance from:" & Source_File_Entry'Image (F.Ref));
            Log (", base:" & Source_File_Entry'Image (F.Base));
            Log (", loc:" & Image (F.Instance_Loc));
            Log_Line;
      end case;
   end Debug_Source_File;

   procedure Debug_Source_Files is
   begin
      for I in Source_Files.First .. Source_Files.Last loop
         Debug_Source_File (I);
      end loop;
   end Debug_Source_Files;

   pragma Unreferenced (Debug_Source_Lines);
   pragma Unreferenced (Debug_Source_Loc);

   procedure Finalize is
   begin
      for I in Source_Files.First .. Source_Files.Last loop
         Free_Source_File (I);
      end loop;
      Source_Files.Free;
   end Finalize;

   procedure Initialize is
   begin
      Source_Files.Init;
      Next_Location := Location_Nil + 1;
   end Initialize;
end Files_Map;
