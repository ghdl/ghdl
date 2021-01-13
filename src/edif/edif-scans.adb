--  EDIF scanner.
--  Copyright (C) 2019 Tristan Gingold
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Files_Map; use Files_Map;
with Name_Table; use Name_Table;
with Str_Table;
with Errorout; use Errorout;

package body Edif.Scans is
   --  Maximum length of identifiers or names.
   Max_Name_Length : constant := 512;

   --  Length of the file.  This is used to catch EOF embedded in the
   --  file.
   File_Length : Source_Ptr;

   --  Number of the current line.
   Line_Number : Natural;

   --  Position of the start of the line.
   Line_Pos : Source_Ptr;

   Source_File : Source_File_Entry;
   Pos : Source_Ptr;
   Token_Pos : Source_Ptr;

   --  Not required to be saved.
   Source : File_Buffer_Acc := null;

   function Get_Scan_Coord return Source_Coord_Type is
   begin
      return (File => Source_File,
              Line_Pos => Line_Pos,
              Line => Line_Number,
              Offset => Natural (Pos - Line_Pos));
   end Get_Scan_Coord;

   function Get_Token_Location return Location_Type is
   begin
      return File_Pos_To_Location (Source_File, Token_Pos);
   end Get_Token_Location;

   procedure Error_Msg_Scan (Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Scan, Get_Scan_Coord, Msg, Args);
   end Error_Msg_Scan;

   procedure Warning_Msg_Scan (Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Warning, Errorout.Scan, Get_Scan_Coord, Msg, Args);
   end Warning_Msg_Scan;

   procedure Set_File (File : Source_File_Entry) is
   begin
      --  Can be called only when not in use.
      pragma Assert (Source_File = No_Source_File_Entry);

      --  FILE must be a real file.
      pragma Assert (File /= No_Source_File_Entry);

      Source_File := File;
      Source := Get_File_Source (File);
      Pos := Source'First;

      File_Length := Get_File_Length (File);
      Line_Number := 1;
      Line_Pos := Source_Ptr_Org;

      Token_Pos := Pos;
   end Set_File;

   procedure Skip_Newline (C : Character) is
   begin
      if (C = LF and then Source (Pos) = CR)
        or else (C = CR and then Source (Pos) = LF)
      then
         Pos := Pos + 1;
      end if;

      --  Save the position of the next line.
      Line_Number := Line_Number + 1;
      Line_Pos := Pos;
      File_Add_Line_Number (Source_File, Line_Number, Pos);
   end Skip_Newline;

   procedure Skip_Blanks
   is
      C : Character;
   begin
      loop
         C := Source (Pos);
         case C is
            when ' ' | HT =>
               Pos := Pos + 1;
            when CR | LF =>
               Pos := Pos + 1;
               Skip_Newline (C);
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Blanks;

   procedure Current_String_Append (C : Character) is
   begin
      Str_Table.Append_String8_Char (C);
      Current_String_Len := Current_String_Len + 1;
   end Current_String_Append;

   procedure Scan_Decimal_Number
   is
      V : Int32;
      C : Character;
   begin
      V := 0;
      Pos := Pos - 1;
      loop
         C := Source (Pos);
         if C in '0' .. '9' then
            --  FIXME: handle overflow.
            V := V * 10 + Character'Pos (C) - Character'Pos ('0');
         else
            exit;
         end if;
         Pos := Pos + 1;
      end loop;
      --  Check character after the number ?
      Current_Number := V;
      Current_Token := Tok_Number;
   end Scan_Decimal_Number;

   procedure Scan_String
   is
      C : Character;
   begin
      --  FIXME: Scan_String;
      Current_String := Str_Table.Create_String8;
      Current_String_Len := 0;
      loop
         C := Source (Pos);
         if C = '"' then
            --  Skip the final quote.
            Pos := Pos + 1;
            --  Append a NUL.
            Str_Table.Append_String8_Char (NUL);
            return;
         elsif C < ' ' then
            case C is
               when Files_Map.EOT =>
                  Error_Msg_Scan ("non terminated string");
                  return;
               when LF | CR =>
                  Warning_Msg_Scan ("multi-line strings are not allowed");
                  Skip_Newline (C);
                  C := LF;
                  --  But continue.
               when others =>
                  --  FIXME: ref ?
                  Error_Msg_Scan ("control character not allowed in strings");
                  --  Continue as string ?
            end case;
         else
            --  Normal case.
            null;
         end if;
         Current_String_Append (C);
         Pos := Pos + 1;
      end loop;
   end Scan_String;

   --  A valid character for EDIF identifiers.
   function Is_Char_Id (C : Character) return Boolean is
   begin
      return (C in 'a' .. 'z'
                or C in 'A' .. 'Z'
                or C in '0' .. '9'
                or C = '_');
   end Is_Char_Id;

   procedure Scan_Identifier
   is
      Buffer : String (1 .. Max_Name_Length);
      Length : Natural;
      C : Character;
   begin
      Length := 0;
      C := Source (Pos - 1);
      loop
         Length := Length + 1;

         if C in 'A' .. 'Z' then
            --  Convert to lowercase (assuming ASCII).
            C := Character'Val (Character'Pos (C) + 32);
         end if;
         Buffer (Length) := C;

         C := Source (Pos);
         exit when not Is_Char_Id (C);
         Pos := Pos + 1;
      end loop;
      Current_Identifier := Name_Table.Get_Identifier (Buffer (1 .. Length));
   end Scan_Identifier;

   procedure Scan
   is
      C : Character;
   begin
      loop
         Token_Pos := Pos;

         C := Source (Pos);
         Pos := Pos + 1;

         case C is
            when ASCII.NUL .. ASCII.ETX
              | ASCII.ENQ .. ASCII.BS
              | ASCII.VT
              | ASCII.SO .. ASCII.US =>
               Error_Msg_Scan ("unexpected control character ^"
                                 & Character'Val (Character'Pos (C) + 64));
            when ASCII.DEL .. Character'Val (255) =>
               Error_Msg_Scan ("unexpected 8 bit character");
            when Files_Map.EOT =>
               if Pos < File_Length then
                  Error_Msg_Scan ("unexpected ^@ character in file");
               else
                  Current_Token := Tok_Eof;
                  exit;
               end if;
            when LF | CR =>
               Skip_Newline (C);
               --  Skip.
            when ' ' | HT =>
               --  Skip spaces.
               null;
            when ASCII.FF =>
               --  Also considered as a space.
               null;
            when '&' =>
               --  EDIF identifier consits of alphanumeric or underscore
               --  characters.  '&' must be used if the first character is not
               --  alphabetic.
               if not Is_Char_Id (Source (Pos)) then
                  Error_Msg_Scan ("invalid identifier char after '&'");
               else
                  Pos := Pos + 1;
                  Scan_Identifier;
                  Current_Token := Tok_Symbol;
                  exit;
               end if;
            when 'a' .. 'z'
              | 'A' .. 'Z'
              | '_' =>
               Scan_Identifier;
               Current_Token := Tok_Symbol;
               exit;
            when '0' .. '9' =>
               Scan_Decimal_Number;
               exit;
            when '"' =>
               Scan_String;
               Current_Token := Tok_String;
               exit;
            when '(' =>
               --  Be tolerante: allow blanks after '('.
               Skip_Blanks;

               C := Source (Pos);
               if C in 'a' .. 'z' or C in 'A' .. 'Z' then
                  Pos := Pos + 1;
                  Scan_Identifier;
               else
                  Error_Msg_Scan ("keyword expected after '('");
                  Current_Identifier := Null_Identifier;
               end if;
               Current_Token := Tok_Keyword;
               exit;
            when ')' =>
               Current_Token := Tok_Right_Paren;
               exit;
            when '!' | '#' | ''' | '*' | '%' | ',' | ':' | ';'
              | '<' | '=' | '>' | '?' | '@' | '$' | '\' | '[' | ']'
              | '^' | '`' | '/' | '{' | '|' | '}' | '~' | '.' =>
               --  Not allowed ?
               Error_Msg_Scan ("unexpected character '" & C & "'");
            when '+' =>
               if Source (Pos) in '0' .. '9' then
                  Pos := Pos + 1;
                  Scan_Decimal_Number;
                  exit;
               else
                  Error_Msg_Scan ("unexpected '+' character");
               end if;
            when '-' =>
               if Source (Pos) in '0' .. '9' then
                  Pos := Pos + 1;
                  Scan_Decimal_Number;
                  --  Overflow ?
                  Current_Number := -Current_Number;
                  exit;
               else
                  Error_Msg_Scan ("unexpected '-' character");
               end if;
         end case;
      end loop;
   end Scan;

end Edif.Scans;
