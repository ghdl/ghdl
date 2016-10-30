--  GHDL Run Time (GRT) - Wave option file package for parsing.
--  Copyright (C) 2016 Jonas Baggett
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-- Description: See package specifications

-------------------------------------------------------------------------------

-- TODO:
-- * Currently the elements of the paths parsed are converted to lowercase.
--   This is fine now, but maybe on day Verilog and/or System-C will be
--   supported by GHDL and they are case-sensitive languages. In this case, we
--   will need to find a different approach. Here are 2 possibilities :
--   1) Create 2 trees when parsing : one case sensitive and one case
--      insensitive, then latter when we have more informations, prune VHDL
--      paths from the case sensitive tree and prune verilog / system-C paths
--      from the case insensitive tree (maybe it's not really needed). Then use
--      the right tree while looking for signals to be displayed in the design.
--   2) Create only 1 case sensitive tree then latter when we have more
--      informations, look for VHDL paths in the tree and merge elements who
--      have the same name after lowering their characters.

with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Strings; use Grt.Strings;
with Grt.Astdio; use Grt.Astdio;
with Grt.Errors; use Grt.Errors;
--~ with Grt.Wave_Opt.File.Debug;

package body Grt.Wave_Opt.File is

   -- Open the wave option file
   function Open (Option_File : String) return FILEs;

   -- Initialize the root of the tree
   procedure Initialize_Tree;

   -- Tell if the tree is empty (beside the root)
   function Tree_Is_Empty return Boolean;

   -- Parse the wave option file
   procedure Parse_File (Stream : FILEs);

   -- Parse the line where the version is set
   procedure Parse_Version (Line : String; Lineno : Positive);

   -- Print the version variable given as parameter
   procedure Print_Version (Version : Version_Type);

   -- Parse a line where a signal path is set
   procedure Parse_Path (Line : in out String; Lineno : Positive);

   procedure Start (Option_File : String) is
      Stream : FILEs;
   begin
      File_Path := new String'(Option_File);
      Stream := Open (Option_File);

      if State = Display_Tree then
         Parse_File (Stream);
      -- State = Write_File
      else
         Write_Stream := Stream;
      end if;
   end Start;

   procedure Parse_File (Stream : FILEs)
   is
      First, Last : Integer;
      Line : String (1 .. Buf_Size);
      Lineno : Natural;
   begin
      Lineno := 0;

      -- Processes line after line.
      loop
         exit when fgets (Line'Address, Line'Length, Stream) = Null_Address;
         Lineno := Lineno + 1;

         -- Determine end of line.
         Last := New_Line_Pos (Line) - 1;
         if Last < 0 then
            Last := Line'Last;
         end if;

         -- Skips empty lines and comments.
         First := First_Non_Whitespace_Pos (Line (Line'First .. Last));
         if First = -1 or else Line (First) = '#' then
            goto Continue;
         end if;

         -- Create a line string without beginning and ending whitespaces
         Last := Last_Non_Whitespace_Pos (Line (First .. Last));

         if Line (First) = '$' then
            Parse_Version (Line (First .. Last), Lineno);
         else
            Parse_Path (Line (First .. Last), Lineno);
         end if;

         <<Continue>> null;
      end loop;

      if Version.Major = -1 then
         Report_C ("warning: version wasn't set at the beginning of the" &
                   " file; currently supported version is ");
         Print_Version (Current_Version);
         Report_E ("");
      end if;

      if Tree_Is_Empty then
         Report_E ("No signal path was found in the wave option file," &
                   " then every signals will be displayed.");
      end if;

      fclose (Stream);
      --~ Debug.Dump_Tree;

   end Parse_File;

   procedure Parse_Version (Line : String; Lineno : Positive)
   is
      Msg_Invalid_Format : constant String := "invalid version format";
      First, Dot_Index, Num : Integer;
   begin

      if Version /= (others => -1) then
         Error_Context ("version is set more than once", Lineno, Line'First);
      end if;

      if not Tree_Is_Empty then
         Error_Context
           ("version cannot be set after signal paths", Lineno, Line'First);
      end if;

      First := First_Non_Whitespace_Pos (Line (Line'First + 1 .. Line'Last));
      if Line (First .. First + 6) /= "version" then
         Error_Context (Msg_Invalid_Format, Lineno, Line'First);
      end if;

      -- Catch "version\n", "version1.0"
      First := First + 7;
      if not Is_Whitespace (Line (First)) then
         Error_Context (Msg_Invalid_Format, Lineno, Line'First);
      end if;

      -- Catch "version \n", "version  \n", etc
      First := First_Non_Whitespace_Pos (Line (First + 1 .. Line'Last));
      if First = -1 then
         Error_Context (Msg_Invalid_Format, Lineno, Line'First);
      end if;

      -- Catch the absence of "." or "version ."
      Dot_Index := Find (Line (First + 1 .. Line'Last), '.');
      if Dot_Index = -1 then
         Error_Context (Msg_Invalid_Format, Lineno, Line'First);
      end if;

      -- Catch version a.2
      Num := Value (Line (First .. Dot_Index - 1));
      if Num = -1 then
         Error_Context (Msg_Invalid_Format, Lineno, Line'First);
      end if;
      Version.Major := Num;

      -- Catch version 1.a
      Num := Value (Line (Dot_Index + 1 .. Line'Last));
      if Num = -1 then
         Error_Context (Msg_Invalid_Format, Lineno, Line'First);
      end if;
      Version.Minor := Num;

      if Version.Major /= Current_Version.Major
        or else Version.Minor > Current_Version.Minor
      then
         Print_Context (Line'First, Lineno, Error);
         Error_C ("unsupported format version; it must be ");
         if Current_Version.Minor /= 0 then
            Error_C ("between ");
            Print_Version (Version_Type'(Current_Version.Major, 0));
            Error_C (" and ");
         end if;
         Print_Version (Current_Version);
         Error_E;
      end if;

   end Parse_Version;

   procedure Print_Version (Version : Version_Type) is
   begin
      Report_C (Version.Major);
      Report_C (".");
      Report_C (Version.Minor);
   end Print_Version;

   procedure Initialize_Tree is
   begin
      for I in Tree_Index_Type'Range loop
         Trees (I) := new Elem_Type;
         Trees (I).Name := new String'(1 => Seps (I));
         Trees (I).Level := 0;
      end loop;
   end Initialize_Tree;

   function Tree_Is_Empty return Boolean is
   begin
      return Trees (Pkg).Next_Child = null
             and Trees (Entity).Next_Child = null;
   end Tree_Is_Empty;

   procedure Parse_Path (Line : in out String; Lineno : Positive)
   is
      -- Can equal to 0 in case of error (like '.' as a full path)
      First, Last : Natural;
      Path_Context : Path_Context_Acc;
      Tree_Index : Tree_Index_Type;
      Tree_Cursor : Elem_Acc;
      Tree_Updated : Boolean;
   begin
      Path_Context := new Path_Context_Type'(Lineno => Lineno,
                                             Max_Level => 0);
      To_Lower (Line);
      Last := Line'First;
      if Line (Line'First) = '/' then
         Tree_Index := Entity;
         Last := Last + 1;
         -- Catch '/' as a full path
         if Last > Line'Length then
            Error_Context
              ("invalid signal path", Path_Context.Lineno, Line'First);
         end if;
      else
         -- '/' not allowed for package signal paths in a.  Catch also the
         -- absence a first slash in entity signal paths, which misleads the
         -- code to believe it's inside a package
         if Find (Line, '/') > 0 then
            Error_Context
              ("invalid signal path", Path_Context.Lineno, Line'First);
         end if;
         Tree_Index := Pkg;
      end if;
      Tree_Cursor := Trees (Tree_Index);

      loop
         First := Last;

         -- Find next identifier
         loop
            if Line (Last) = Seps (Tree_Index) then
               Last := Last - 1;
               exit;
            elsif Last = Line'Last then
               exit;
            end if;
            Last := Last + 1;
         end loop;

         Path_Context.Max_Level := Path_Context.Max_Level + 1;
         Update_Tree (Cursor => Tree_Cursor,
                      Updated => Tree_Updated,
                      Elem_Name => Line (First .. Last),
                      Level => Path_Context.Max_Level,
                      Path_Context => Path_Context);

         if Last = Line'Last then
            if not Tree_Updated then
               Error_Context ("ignored already known signal path",
                              Path_Context.Lineno,
                              Line'First,
                              Warning);
            end if;
            return;
         end if;

         -- Skip the separator
         Last := Last + 2;
         -- Catch signal paths ending with / or .
         if Last > Line'Last then
            Error_Context
              ("invalid signal path", Path_Context.Lineno, Line'First);
         end if;

      end loop;

   end Parse_Path;

   procedure Update_Tree (Cursor : in out Elem_Acc;
                          Updated : out Boolean;
                          Elem_Name : String;
                          Level : Natural;
                          Path_Context : Path_Context_Acc := null)
   is
      Sibling_Cursor, Previous_Sibling_Cursor : Elem_Acc;
      Created_Elem : Elem_Acc;
   begin
      Previous_Sibling_Cursor := null;
      Sibling_Cursor := Cursor.Next_Child;
      loop
         -- Already reached the last sibling and current identifier corresponds
         -- to no existing element ? Then we will create an element
         if Sibling_Cursor = null then
            Created_Elem := new Elem_Type;
            Created_Elem.Name := new String'(Elem_Name);
            Created_Elem.Path_Context := Path_Context;
            Created_Elem.Column := Elem_Name'First;
            Created_Elem.Level := Level;
            Created_Elem.Parent := Cursor;
            -- First element of level ?
            if Previous_Sibling_Cursor = null then
               Cursor.Next_Child := Created_Elem;
            else
               Previous_Sibling_Cursor.Next_Sibling := Created_Elem;
            end if;
            Cursor := Created_Elem;
            Updated := True;
            return;
         -- Identifier was found in the tree ? Then move to its first child
         elsif Elem_Name = Sibling_Cursor.Name.all then
            Cursor := Sibling_Cursor;
            Updated := False;
            return;
         end if;
         Previous_Sibling_Cursor := Sibling_Cursor;
         Sibling_Cursor := Sibling_Cursor.Next_Sibling;
      end loop;
   end Update_Tree;

   procedure Write_Version (Stream : FILEs) is
   begin
         Put (Stream, "$ version ");
         Put_I32 (Stream, Ghdl_I32 (Current_Version.Major));
         Put (Stream, '.');
         Put_I32 (Stream, Ghdl_I32 (Current_Version.Minor));
         New_Line (Stream);
   end Write_Version;

   function Open (Option_File : String) return FILEs
   is
      Read_Mode : constant String := "rt" & ASCII.Nul;
      Write_Mode : constant String := "wt" & ASCII.Nul;
      Stream : FILEs;
      Option_File_C : String (1 .. Option_File'Length + 1);
   begin
      Option_File_C (1 .. Option_File'Length) := Option_File;
      Option_File_C (Option_File_C'Last) := ASCII.Nul;
      State := Display_Tree;

      Stream := fopen (Option_File_C'Address, Read_Mode'Address);
      if Stream = NULL_Stream then
         Report_C (Option_File);
         Report_E (" does not exist, so it will be created");

         State := Write_File;
         Stream := fopen (Option_File_C'Address, Write_Mode'Address);
         if Stream = NULL_Stream then
            Error_C ("cannot create '");
            Error_C (Option_File);
            Error_E ("'");
         end if;

         Write_Version (Stream);
      end if;

      Initialize_Tree;

      return Stream;
   end Open;

   procedure Write_Tree_Comment (Tree_Index : Tree_Index_Type) is
   begin
      New_Line (Write_Stream);
      if Tree_Index = Pkg then
         Put_Line (Write_Stream, "# Signals in packages :");
      else
         Put_Line (Write_Stream, "# Signals in entities :");
      end if;
   end Write_Tree_Comment;

   procedure Write_Signal_Path (Signal : Elem_Acc) is
      type Elem_Array is array (Positive range <>) of Elem_Acc;
      Signal_Path : Elem_Array (1 .. Signal.Level - 1);
      Cursor : Elem_Acc;
      Sep : Character;
   begin
      Cursor := Signal.Parent;
      for I in reverse Signal_Path'Range loop
         Signal_Path (I) := Cursor;
         Cursor := Cursor.Parent;
      end loop;
      if Signal_Path (1).Parent.Name.all = "/" then
         Sep := '/';
         Put (Write_Stream, Sep);
      else
         Sep := '.';
      end if;
      for I in Signal_Path'Range loop
         Put (Write_Stream, Signal_Path (I).Name.all);
         Put (Write_Stream, Sep);
      end loop;
      Put_Line (Write_Stream, Signal.Name.all);
   end Write_Signal_Path;

   procedure Finalize is
   begin
      if State = Write_File then
         fclose (Write_Stream);
         State := Display_All;
      end if;
   end Finalize;

end Grt.Wave_Opt.File;
