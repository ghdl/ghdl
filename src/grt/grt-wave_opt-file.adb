--  GHDL Run Time (GRT) - Wave option file package for parsing.
--  Copyright (C) 2016 Jonas Baggett
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
--      have the same expression after lowering their characters.

with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Strings; use Grt.Strings;
with Grt.Astdio; use Grt.Astdio;
with Grt.Errors; use Grt.Errors;
--~ with Grt.Wave_Opt.File.Debug;

package body Grt.Wave_Opt.File is

   procedure To_Lower (Signal_Path : in out String);

   -- Open the wave option file
   function Open (Option_File : String; To_Be_Created : Boolean) return FILEs;

   -- Initialize the root of the tree
   procedure Initialize_Tree;

   -- Tell if the tree is empty (beside the root)
   function Tree_Is_Empty return Boolean;

   -- Parse the wave option file
   procedure Parse_File (Stream : FILEs);

   -- Parse the line where the version is set
   procedure Parse_Version (Line : String; Lineno : Positive);

   -- Print the version variable given as parameter
   procedure Diag_C_Version (Version : Version_Type);

   -- Parse a line where a signal path is set
   procedure Parse_Path (Line : in out String; Lineno : Positive);

   procedure Start (Option_File : String; To_Be_Created : Boolean) is
      Stream : FILEs;
   begin
      File_Path := new String'(Option_File);
      Stream := Open (Option_File, To_Be_Created);

      if State = Display_Tree then
         Parse_File (Stream);
      -- Otherwise, State = Write_File
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
         Warning_S ("version wasn't set at the beginning of the" &
                   " file; currently supported version is ");
         Diag_C_Version (Current_Version);
         Warning_E;
      end if;

      if Tree_Is_Empty then
         Warning_S ("No signal path was found in the wave option file," &
                      " then every signals will be displayed.");
         Warning_E;
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
         Error_S;
         Diag_C_Context (Line'First, Lineno);
         Diag_C ("unsupported format version; it must be ");
         if Current_Version.Minor /= 0 then
            Diag_C ("between ");
            Diag_C_Version (Version_Type'(Current_Version.Major, 0));
            Diag_C (" and ");
         end if;
         Diag_C_Version (Current_Version);
         Error_E;
      end if;

   end Parse_Version;

   procedure Diag_C_Version (Version : Version_Type) is
   begin
      Diag_C (Version.Major);
      Diag_C ('.');
      Diag_C (Version.Minor);
   end Diag_C_Version;

   procedure Initialize_Tree is
   begin
      for I in Tree_Index_Type'Range loop
         Trees (I) := new Elem_Type;
         Trees (I).Expr := new String'(1 => Seps (I));
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
      Level : Positive;
      Tree_Index : Tree_Index_Type;
      Tree_Cursor : Elem_Acc;
      Tree_Updated : Boolean;
      Is_Extended_Identifier : Boolean := False;
   begin
      To_Lower (Line);
      Last := Line'First;
      if Line (Line'First) = '/' then
         Tree_Index := Entity;
         Last := Last + 1;
         -- Catch '/' as a full path
         if Last > Line'Length then
            Error_Context
              ("invalid signal path", Lineno, Line'First);
         end if;
      else
         -- '/' not allowed for package signal paths in a.  Catch also the
         -- absence a first slash in entity signal paths, which misleads the
         -- code to believe it's inside a package
         if Find (Line, '/') > 0 then
            Error_Context
              ("invalid signal path", Lineno, Line'First);
         end if;
         Tree_Index := Pkg;
      end if;
      Tree_Cursor := Trees (Tree_Index);
      Tree_Updated := False;
      Level := 1;

      loop
         First := Last;

         -- Find next identifier
         loop
            if Line (Last) = '\' then
               Is_Extended_Identifier := not Is_Extended_Identifier;
            end if;
            -- What is enclosed between \ and \ is interpreted as an extended
            -- identifier, so we need to make sure that nothing in between will
            -- be interpreted as a signal path separator.
            if not Is_Extended_Identifier and Line (Last) = Seps (Tree_Index)
            then
               Last := Last - 1;
               exit;
            elsif Last = Line'Last then
               if Is_Extended_Identifier then
                  Error_Context("Extended identifier not terminated by a '\'",
                                Lineno, First);
               end if;
               exit;
            end if;
            Last := Last + 1;
         end loop;

         Update_Tree (Cursor => Tree_Cursor,
                      Last_Updated => Tree_Updated,
                      Elem_Expr => Line (First .. Last),
                      Level => Level,
                      Lineno => Lineno);

         if Last = Line'Last then
            -- If there is the following content in the wave option file :
            --    /top/a/b
            --    /top/a
            -- Then there is a conflict between those lines as according to the
            -- 2nd line, a is a signal but it isn't according to the 1st line.
            -- Then /top/a will supercede /top/a/b.
            if not Tree_Updated and Tree_Cursor.Next_Child /= null then
               Warning_S;
               Diag_C_Context (Lineno, Line'First);
               Diag_C ("supercedes line ");
               Diag_C (Tree_Cursor.Lineno);
               Diag_C (" and possibly more lines in between");
               Warning_E;
               -- TODO : destroy Tree_Cursor.Next_Child
               Tree_Cursor.Lineno := Lineno;
               Tree_Cursor.Next_Child := null;
            end if;
            return;
         end if;

         Level := Level + 1;
         -- Skip the separator
         Last := Last + 2;
         -- Catch signal paths ending with / or .
         if Last > Line'Last then
            Error_Context ("invalid signal path", Lineno, Line'First);
         end if;

      end loop;

   end Parse_Path;

   procedure Update_Tree (Cursor : in out Elem_Acc;
                          Last_Updated : in out Boolean;
                          Elem_Expr : String;
                          Level : Natural;
                          Lineno : Natural := 0)
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
            Created_Elem.Expr := new String'(Elem_Expr);
            Created_Elem.Lineno := Lineno;
            Created_Elem.Column := Elem_Expr'First;
            Created_Elem.Level := Level;
            Created_Elem.Parent := Cursor;
            -- First element of level ?
            if Previous_Sibling_Cursor = null then
               -- If there is the following content in the wave option file :
               --    /top/a
               --    /top/a/b
               -- Then there is a conflict between those lines as according to
               -- the 1st line, a is a signal but it isn't according to the 2nd
               -- line. Then /top/a will supercede /top/a/b.
               if Level > 1 and not Last_Updated then
                  Warning_S;
                  Diag_C_Context (Lineno, Elem_Expr'First);
                  Diag_C ("superceded by line ");
                  Diag_C (Cursor.Lineno);
                  Warning_E;
                  return;
                  -- TODO : destroy Created_Elem
               end if;
               Cursor.Next_Child := Created_Elem;
            else
               Previous_Sibling_Cursor.Next_Sibling := Created_Elem;
            end if;
            Cursor := Created_Elem;
            Last_Updated := True;
            return;
         -- Identifier was found in the tree ? Then move to its first child
         elsif Elem_Expr = Sibling_Cursor.Expr.all then
            Cursor := Sibling_Cursor;
            Last_Updated := False;
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

   procedure To_Lower (Signal_Path : in out String) is
      Is_Extended_Identifier : Boolean := false;
   begin
      for I in Signal_Path'Range loop
         if Signal_Path (I) = '\' then
            Is_Extended_Identifier := not Is_Extended_Identifier;
         elsif not Is_Extended_Identifier then
            Signal_Path (I) := To_Lower (Signal_Path (I));
         end if;
      end loop;
   end To_Lower;

   function Open (Option_File : String; To_Be_Created : Boolean) return FILEs
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

      if To_Be_Created then
         if Stream /= NULL_Stream then
            fclose (Stream);
            Error_S ("'");
            Diag_C (Option_File);
            Error_E ("' already exists and it won't be erased.");
         end if;
         State := Write_File;
         Stream := fopen (Option_File_C'Address, Write_Mode'Address);
         if Stream = NULL_Stream then
            Error_S ("cannot create '");
            Diag_C (Option_File);
            Error_E ("'");
         end if;
         Write_Version (Stream);
      elsif Stream = NULL_Stream then
         Error_S ("cannot read '");
         Diag_C (Option_File);
         Error_E ("'");
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
      if Signal_Path (1).Parent.Expr.all = "/" then
         Sep := '/';
         Put (Write_Stream, Sep);
      else
         Sep := '.';
      end if;
      for I in Signal_Path'Range loop
         Put (Write_Stream, Signal_Path (I).Expr.all);
         Put (Write_Stream, Sep);
      end loop;
      Put_Line (Write_Stream, Signal.Expr.all);
   end Write_Signal_Path;

   procedure Finalize is
   begin
      if State = Write_File then
         fclose (Write_Stream);
         State := Display_All;
      end if;
   end Finalize;

end Grt.Wave_Opt.File;
