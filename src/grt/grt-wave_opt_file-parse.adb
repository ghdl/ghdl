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
with Grt.Stdio; use Grt.Stdio;
with Grt.Strings; use Grt.Strings;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Errors; use Grt.Errors;

--~ with Grt.Wave_Opt_File.Parse.Debug;

package body Grt.Wave_Opt_File.Parse is
   -- Open the wave option file
   function File_Open (Option_File : String) return FILEs;

   -- Update the tree with the current VHDL element parsed from the current
   -- path. Returns True if the tree was actually updated.
   function Update_Tree (Elem_Name : String; Tree_Index : Tree_Index_Type)
                        return Boolean;

   -- Parse the line where the version is set
   procedure Parse_Version (Line : String; Line_Pos : Positive);

   -- Print the version variable given as parameter
   procedure Print_Version (Version : Version_Type);

   -- Parse a line where a signal path is set
   procedure Parse_Path (Line : in out String);

   procedure Start (Option_File : String)
   is
      Stream : constant FILEs := File_Open (Option_File);
      First, Last : Integer;
      Line : String (1 .. Buf_Size);
      Line_Pos : Natural;
   begin
      File_Path := new String'(Option_File);
      Line_Pos := 0;

      -- Processes line after line.
      loop
         exit when fgets (Line'Address, Line'Length, Stream) = Null_Address;
         Line_Pos := Line_Pos + 1;

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
            Parse_Version (Line (First .. Last), Line_Pos);
         else
            Path_Context := new Path_Context_Type'(Line_Pos => Line_Pos,
                                                   Max_Level => 0);
            Parse_Path (Line (First .. Last));
         end if;

         <<Continue>> null;
      end loop;

      if Version.Major = -1 then
         Report_C ("warning: version wasn't set at the beginning of the" &
                   " file; currently supported version is ");
         Print_Version (Current_Version);
         Report_E ("");
      end if;

      if Trees = Tree_Array'(others => null) then
         Report_E ("No signal path was found in the wave option file," &
                   " then every signals will be displayed.");
      end if;

      --~ Debug.Dump_Tree;

   end Start;

-------------------------------------------------------------------------------

   procedure Parse_Version (Line : String; Line_Pos : Positive)
   is
      Msg_Invalid_Format : constant String := "invalid version format";
      First, Dot_Index, Num : Integer;
   begin

      if Version /= (others => -1) then
         Error_Context ("version is set more than once", Line_Pos, Line'First);
      end if;

      if Trees /= Tree_Array'(others => null) then
         Error_Context
           ("version cannot be set after signal paths", Line_Pos, Line'First);
      end if;

      First := First_Non_Whitespace_Pos (Line (Line'First + 1 .. Line'Last));
      if Line (First .. First + 6) /= "version" then
         Error_Context (Msg_Invalid_Format, Line_Pos, Line'First);
      end if;

      -- Catch "version\n", "version1.0"
      First := First + 7;
      if not Is_Whitespace (Line (First)) then
         Error_Context (Msg_Invalid_Format, Line_Pos, Line'First);
      end if;

      -- Catch "version \n", "version  \n", etc
      First := First_Non_Whitespace_Pos (Line (First + 1 .. Line'Last));
      if First = -1 then
         Error_Context (Msg_Invalid_Format, Line_Pos, Line'First);
      end if;

      -- Catch the absence of "." or "version ."
      Dot_Index := Find (Line (First + 1 .. Line'Last), '.');
      if Dot_Index = -1 then
         Error_Context (Msg_Invalid_Format, Line_Pos, Line'First);
      end if;

      -- Catch version a.2
      Num := Value (Line (First .. Dot_Index - 1));
      if Num = -1 then
         Error_Context (Msg_Invalid_Format, Line_Pos, Line'First);
      end if;
      Version.Major := Num;

      -- Catch version 1.a
      Num := Value (Line (Dot_Index + 1 .. Line'Last));
      if Num = -1 then
         Error_Context (Msg_Invalid_Format, Line_Pos, Line'First);
      end if;
      Version.Minor := Num;

      if Version.Major /= Current_Version.Major
        or else Version.Minor > Current_Version.Minor
      then
         Print_Context (Line'First, Line_Pos, Error);
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

   procedure Print_Version (Version : Version_Type)
   is
      Num_Str : String (1 .. Value_String_Size);
      First : Positive;
   begin
      To_String (Num_Str, First, Ghdl_I32 (Version.Major));
      Report_C (Num_Str (First .. Num_Str'Last));
      Report_C (".");
      To_String (Num_Str, First, Ghdl_I32 (Version.Minor));
      Report_C (Num_Str (First .. Num_Str'Last));
   end Print_Version;

   --------------------------------------------------------------------------

   procedure Parse_Path (Line : in out String)
   is
      -- Can equal to 0 in case of error (like '.' as a full path)
      First, Last : Natural;
      Tree_Updated : Boolean;
      Tree_Index : Tree_Index_Type;
   begin
      To_Lower (Line);
      Last := Line'First;
      if Line (Line'First) = '/' then
         Tree_Index := Entity;
         Last := Last + 1;
         -- Catch '/' as a full path
         if Last > Line'Length then
            Error_Context
              ("invalid signal path", Path_Context.Line_Pos, Line'First);
         end if;
      else
         -- '/' not allowed for package signal paths in a.  Catch also the
         -- absence a first slash in entity signal paths, which misleads the
         -- code to believe it's inside a package
         if Find (Line, '/') > 0 then
            Error_Context
              ("invalid signal path", Path_Context.Line_Pos, Line'First);
         end if;
         Tree_Index := Pkg;
      end if;
      Tree_Cursor := Trees (Tree_Index);
      Previous_Tree_Cursor := null;

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
         Tree_Updated := Update_Tree (Line (First .. Last), Tree_Index);

         if Last = Line'Last then
            if not Tree_Updated then
               Error_Context ("ignored already known signal path",
                              Path_Context.Line_Pos,
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
              ("invalid signal path", Path_Context.Line_Pos, Line'First);
         end if;

      end loop;

   end Parse_Path;

   function Update_Tree (Elem_Name : String; Tree_Index : Tree_Index_Type)
                        return Boolean
   is
      Sibling_Cursor, Previous_Sibling_Cursor : Elem_Acc;
      Elem : Elem_Acc;
   begin
      Sibling_Cursor := Tree_Cursor;
      Previous_Sibling_Cursor := null;

      loop
         -- Already reached the last sibling and current identifier corresponds
         -- to no existing element ? Then we will create an element
         if Sibling_Cursor = null then
            Elem := new Elem_Type'(Name => new String'(Elem_Name),
                                   Path_Context => Path_Context,
                                   Column_Pos => Elem_Name'First,
                                   Level => Path_Context.Max_Level,
                                   Kind => Not_Found,
                                   Next_Sibling | Next_Child => null);
            -- First element of level ?
            if Previous_Sibling_Cursor = null then
               -- Is a top level ?
               if Previous_Tree_Cursor = null then
                  Trees (Tree_Index) := Elem;
               else
                  Previous_Tree_Cursor.Next_Child := Elem;
               end if;
            else
               Previous_Sibling_Cursor.Next_Sibling := Elem;
            end if;
            Previous_Tree_Cursor := Elem;
            -- Point to Elem.Next_Child which is null
            Tree_Cursor := null;
            return True;
         -- Identifier was found in the tree ? Then move to its first child
         elsif Elem_Name = Sibling_Cursor.Name.all then
            Previous_Tree_Cursor := Sibling_Cursor;
            Tree_Cursor := Sibling_Cursor.Next_Child;
            return False;
         end if;
         Previous_Sibling_Cursor := Sibling_Cursor;
         Sibling_Cursor := Sibling_Cursor.Next_Sibling;
      end loop;
   end Update_Tree;

   --------------------------------------------------------------------------

   function File_Open (Option_File : String) return FILEs
   is
      Mode : constant String := "rt" & ASCII.Nul;
      Stream : FILEs;
      Option_File_C : String (1 .. Option_File'Length + 1);
   begin
      Option_File_C (1 .. Option_File'Length) := Option_File;
      Option_File_C (Option_File_C'Last) := ASCII.Nul;
      Stream := fopen (Option_File_C'Address, Mode'Address);
      if Stream = NULL_Stream then
         Error_C ("cannot open '");
         Error_C (Option_File);
         Error_E ("'");
      end if;
      return Stream;
   end File_Open;

end Grt.Wave_Opt_File.Parse;
