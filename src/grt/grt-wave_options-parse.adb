--  GHDL Run Time (GRT) -  command line options.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Strings; use Grt.Strings;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Errors; use Grt.Errors;

--~ with Grt.Wave_Options.Parse.Debug;

package body Grt.Wave_Options.Parse is

   procedure Start (Option_File : String) is
      Stream : constant FILEs := File_Open (Option_File);
      First, Last : Integer;
      Line : String (1 .. Buf_Size);
      Lineno : Natural;
   begin
      File_Path := new String'(Option_File);
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
         Line_Context := new Line_Context_Type'(
                                Str => new String'(Line (First .. Last)),
                                Num => Lineno,
                                Max_Level => 0);


         if Line (First) = '$' then
            Parse_Version (Line_Context.Str);
         else
            Parse_Path (Line_Context.Str);
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

-- private --------------------------------------------------------------------

   procedure Parse_Version (Line : String_Acc) is
      Msg_Invalid_Format : constant String := "invalid version format";
      First, Dot_Index, Num : Integer;
   begin

      if Version /= (others => -1) then
         Error_Context ("version is set more than once");
      end if;

      if Trees /= Tree_Array'(others => null) then
         Error_Context ("version cannot be set after signal paths");
      end if;

      First := First_Non_Whitespace_Pos (Line (Line'First + 1 .. Line'Last));
      if Line (First .. First + 6) /= "version" then
         Error_Context (Msg_Invalid_Format);
      end if;

      -- Catch "version\n", "version1.0"
      First := First + 7;
      if not Is_Whitespace (Line (First)) then
         Error_Context (Msg_Invalid_Format);
      end if;

      -- Catch "version \n", "version  \n", etc
      First := First_Non_Whitespace_Pos (Line (First + 1 .. Line'Last));
      if First = -1 then
         Error_Context (Msg_Invalid_Format);
      end if;

      -- Catch the absence of "." or "version ."
      Dot_Index := Find (Line (First + 1 .. Line'Last), '.');
      if Dot_Index = -1 then
         Error_Context (Msg_Invalid_Format);
      end if;

      -- Catch version a.2
      Num := Value (Line (First .. Dot_Index - 1));
      if Num = -1 then
         Error_Context (Msg_Invalid_Format);
      end if;
      Version.Major := Num;

      -- Catch version 1.a
      Num := Value (Line (Dot_Index + 1 .. Line'Last));
      if Num = -1 then
         Error_Context (Msg_Invalid_Format);
      end if;
      Version.Minor := Num;

      if Version.Major /= Current_Version.Major
        or else Version.Minor > Current_Version.Minor
      then
         Print_Context (Error);
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

   procedure Parse_Path (Line : String_Acc) is
      First, Last : Positive;
      Tree_Updated : Boolean;
      Tree_Index : Tree_Index_Type;
   begin
      To_Lower (Line_Context.Str.all);
      Last := Line'First;
      if Line (Line'First) = '/' then
         Tree_Index := Entity;
         Last := Last + 1;
      else
         Tree_Index := Pkg;
      end if;
      Tree_Cursor := Trees (Tree_Index);
      Previous_Tree_Cursor := null;

      loop
         First := Last;

         loop -- Find next identifier
            if Line (Last) = Sep (Tree_Index) then
               Last := Last - 1;
               exit;
            elsif Last = Line'Last then
               exit;
            end if;
            Last := Last + 1;
         end loop;

         Check_Validity (Line (First .. Last));
         Tree_Updated := Update_Tree (Line (First .. Last), Tree_Index);
         Line_Context.Max_Level := Line_Context.Max_Level + 1;

         if Last = Line'Last then
            if not Tree_Updated then
               Error_Context ("ignored already known signal path", Warning);
            end if;
            return;
         end if;

         Last := Last + 2; -- Skip the separator
         if Last > Line'Last then -- catch signal paths ending with /
            Error_Context ("invalid signal path");
         end if;

      end loop;

   end Parse_Path;

   function Update_Tree (Elem_Name : String; Tree_Index : Tree_Index_Type)
            return Boolean is
      Sibling_Cursor : Elem_Acc := Tree_Cursor;
      Previous_Sibling_Cursor : Elem_Acc := null;
      Elem : Elem_Acc;
   begin
      loop
         -- Already reached the last sibling and current identifier corresponds
         -- to no existing element ? Then we will create an element
         if Sibling_Cursor = null then
            Elem := new Elem_Type'(Name => new String'(Elem_Name),
                                   Line_Context => Line_Context,
                                   Kind => Not_Found,
                                   Next_Sibling | Next_Child => null);
            if Previous_Sibling_Cursor = null then -- First element of level ?
               if Previous_Tree_Cursor = null then -- Is a top_level ?
                  Trees (Tree_Index) := Elem;
               else
                  Previous_Tree_Cursor.Next_Child := Elem;
               end if;
            else
               Previous_Sibling_Cursor.Next_Sibling := Elem;
            end if;
            Previous_Tree_Cursor := Elem;
            Tree_Cursor := null; -- Point to Elem.Next_Child which is null
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

   procedure Check_Validity (Elem_Name : String) is
   begin
      if Elem_Name'Length = 0 then
         Error_Context ("invalid signal path");
      end if;
      for Index in Elem_Name'Range loop
         case Elem_Name (Index) is
         when 'A' .. 'Z' | 'a' .. 'z' =>
            null;
         when '0' .. '9' =>
            if Index = Elem_Name'First then
               Validity_Error (Elem_Name);
            end if;
         when '_' =>
            if Index = Elem_Name'First
              or else Index = Elem_Name'Last
              or else Elem_Name (Index - 1) = '_'
            then
               Validity_Error (Elem_Name);
            end if;
         when '.' | '/' =>
            Error_Context ("invalid signal path");
         when others =>
            Validity_Error (Elem_Name);
         end case;
      end loop;
   end Check_Validity;

   procedure Validity_Error (Elem_Name : String) is
   begin
      Print_Context (Error);
      Error_C ("object name '");
      Error_C (Elem_Name);
      Error_E ("' is not a valid VHDL name");
   end Validity_Error;

   --------------------------------------------------------------------------

   procedure Print_Context (Severity : Severity_Type) is
   begin
      Print_Context (Line_Context, Severity);
   end Print_Context;

   procedure Error_Context (Msg : String; Severity : Severity_Type := Error) is
   begin
      Error_Context (Msg, Line_Context, Severity);
   end Error_Context;

   function File_Open (Option_File : String) return FILEs is
      Mode : constant String := "rt" & ASCII.Nul;
      Stream : FILEs;
   begin
      Stream := fopen (Option_File'Address, Mode'Address);
      if Stream = NULL_Stream then
         Error_C ("cannot open '");
         Error_C (Option_File (Option_File'First .. Option_File'Last - 1));
         Error_E ("'");
      end if;
      return Stream;
   end File_Open;

end Grt.Wave_Options.Parse;
