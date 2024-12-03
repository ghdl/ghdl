--  GHDL driver - Coverage commands
--  Copyright (C) 2024 Tristan Gingold
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

with Types; use Types;
with Tables;
with Files_Map;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Errorout;
with Name_Table;
with Options; use Options;

with Ghdlmain; use Ghdlmain;

with Grt.Types;
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio;

package body Ghdlcov is
   type Line_Result is record
      Coverage : Boolean;
      Covered : Boolean;
   end record;
   pragma Pack (Line_Result);

   type Line_Array is array (Positive range <>) of Line_Result;
   type Line_Acc is access Line_Array;

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Line_Array, Line_Acc);

   type File_Record is record
      Name : Name_Id;
      Dir : Name_Id;
      Checksum : File_Checksum_String;

      Lines : Line_Acc;
   end record;

   type File_Record_Acc is access File_Record;

   package Res_Tables is new Tables
     (Table_Component_Type => File_Record_Acc,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 8);

   function Fopen_W (Filename : String) return FILEs
   is
      Cname : constant String := Filename & ASCII.NUL;
      Mode : constant String := "w" & ASCII.NUL;
      F : FILEs;
   begin
      F := fopen (Cname'Address, Mode'Address);
      if F = NULL_Stream then
         Errorout.Error_Msg_Option ("cannot open " & Filename);
      end if;
      return F;
   end Fopen_W;

   package Mini_Json is
      type Token is
        (Tok_Eof, Tok_Error,
         Tok_Lbrac, Tok_Rbrac,
         Tok_Lcurl, Tok_Rcurl,
         Tok_Colon, Tok_Comma,
         Tok_String,
         Tok_Number
        );

      procedure Init (F : Source_File_Entry);

      --  Scan and return next token.
      function Scan return Token;

      --  Return current line number.
      function Get_Line return Natural;

      --  If TOK /= REF, raise parse_error.
      procedure Expect (Tok : Token; Ref : Token);

      --  Expect next token is REF, raise Parse_Error in case of error.
      procedure Scan_Expect (Ref : Token);

      --  Check next token is the string REF.
      procedure Scan_Expect_String (Ref : String);

      --  Get the content of the current string (the last token must have
      --  been Tok_String.
      function Get_String return String;

      --  Get the number value (the last token must have been Tok_Number).
      function Get_Number return Natural;

      Parse_Error : exception;
   end Mini_Json;

   package body Mini_Json is
      Buf : File_Buffer_Acc;
      Pos : Source_Ptr;
      Tok_Pos : Source_Ptr;
      Tok_Val : Natural;
      Line : Positive;

      procedure Init (F : Source_File_Entry) is
      begin
         Buf := Files_Map.Get_File_Source (F);
         Pos := Buf'First;
         Line := 1;
      end Init;

      function Get_Line return Natural is
      begin
         return Line;
      end Get_Line;

      function Scan return Token
      is
         C : Character;
      begin
         loop
            C := Buf (Pos);
            Tok_Pos := Pos;
            Pos := Pos + 1;

            case C is
               when ' ' | ASCII.HT =>
                  null;
               when '[' =>
                  return Tok_Lbrac;
               when ']' =>
                  return Tok_Rbrac;
               when '{' =>
                  return Tok_Lcurl;
               when '}' =>
                  return Tok_Rcurl;
               when ':' =>
                  return Tok_Colon;
               when ',' =>
                  return Tok_Comma;
               when '"' =>
                  loop
                     C := Buf (Pos);
                     Pos := Pos + 1;
                     if C = '"' then
                        return Tok_String;
                     elsif C < ' ' or C > '~' then
                        return Tok_Error;
                     end if;
                  end loop;
               when '0' .. '9' =>
                  Tok_Val := 0;
                  loop
                     Tok_Val := Tok_Val * 10
                       + Character'Pos (C) - Character'Pos ('0');
                     C := Buf (Pos);
                     if C < '0' or C > '9' then
                        return Tok_Number;
                     end if;
                     Pos := Pos + 1;
                  end loop;
               when ASCII.CR =>
                  if Buf (Pos) = ASCII.LF then
                     Pos := Pos + 1;
                  end if;
                  Line := Line + 1;
               when ASCII.LF =>
                  if Buf (Pos) = ASCII.CR then
                     Pos := Pos + 1;
                  end if;
                  Line := Line + 1;
               when ASCII.EOT =>
                  return Tok_Eof;
               when others =>
                  Errorout.Error_Msg_Option
                    ("unhandled character '" & C & "' at line"
                       & Natural'Image (Line));
                  return Tok_Error;
            end case;
         end loop;
      end Scan;

      procedure Expect (Tok : Token; Ref : Token) is
      begin
         if Tok /= Ref then
            raise Parse_Error;
         end if;
      end Expect;

      procedure Scan_Expect (Ref : Token) is
      begin
         Expect (Scan, Ref);
      end Scan_Expect;

      function Get_String return String is
      begin
         return String (Buf (Tok_Pos + 1 .. Pos - 2));
      end Get_String;

      function Get_Number return Natural is
      begin
         return Tok_Val;
      end Get_Number;

      procedure Scan_Expect_String (Ref : String) is
      begin
         Scan_Expect (Tok_String);
         if Get_String /= Ref then
            raise Parse_Error;
         end if;
      end Scan_Expect_String;
   end Mini_Json;

   procedure Parse_File_Entry
   is
      use Mini_Json;
      use Errorout;
      Tok : Token;
      Name, Dir : Name_Id;
      Checksum : File_Checksum_String;
      Maxline : Natural;
      F : File_Record_Acc;
      L : Line_Acc;
      Lineno : Natural;
   begin
      Scan_Expect (Tok_Lcurl);

      --  file: xx
      Scan_Expect_String ("file");
      Scan_Expect (Tok_Colon);
      Scan_Expect (Tok_String);
      Name := Name_Table.Get_Identifier (Get_String);
      Scan_Expect (Tok_Comma);

      --  dir: xx
      Scan_Expect_String ("dir");
      Scan_Expect (Tok_Colon);
      Scan_Expect (Tok_String);
      declare
         Dir_Str : constant String := Get_String;
      begin
         if Dir_Str = "." then
            --  Local directory
            Dir := Null_Identifier;
         else
            Dir := Name_Table.Get_Identifier (Get_String);
         end if;
      end;
      Scan_Expect (Tok_Comma);

      --  sha1: xx
      Scan_Expect_String ("sha1");
      Scan_Expect (Tok_Colon);
      Scan_Expect (Tok_String);
      Checksum := Get_String;
      Scan_Expect (Tok_Comma);

      --  mode: xx
      Scan_Expect_String ("mode");
      Scan_Expect (Tok_Colon);
      Scan_Expect (Tok_String);
      Scan_Expect (Tok_Comma);

      --  max-line: xx
      Scan_Expect_String ("max-line");
      Scan_Expect (Tok_Colon);
      Scan_Expect (Tok_Number);
      Maxline := Get_Number;
      Scan_Expect (Tok_Comma);

      --  Look for the file (or create it);
      F := null;
      for I in Res_Tables.First .. Res_Tables.Last loop
         F := Res_Tables.Table (I);
         exit when F.Name = Name and then F.Dir = Dir;
         F := null;
      end loop;

      if F /= null then
         if F.Checksum /= Checksum then
            Error_Msg_Option
              ("content of file '%i' has changed", (1 => +Name));
            F := null;
         elsif F.Lines'Last < Maxline then
            --  Reallocate
            L := new Line_Array'(1 .. Maxline => (others => False));
            L (1 .. F.Lines'Last) := F.Lines.all;
            Deallocate (F.Lines);
            F.Lines := L;
         end if;
      else
         L := new Line_Array'(1 .. Maxline => (others => False));
         F := new File_Record'(Name => Name,
                               Dir => Dir,
                               Checksum => Checksum,
                               Lines => L);
         Res_Tables.Append (F);
      end if;

      -- result:
      Scan_Expect_String ("result");
      Scan_Expect (Tok_Colon);
      Scan_Expect (Tok_Lcurl);

      loop
         Scan_Expect (Tok_String);
         Lineno := Natural'Value (Get_String);
         Scan_Expect (Tok_Colon);
         Scan_Expect (Tok_Number);
         L (Lineno).Coverage := True;
         L (Lineno).Covered := Get_Number = 1;
         Tok := Scan;
         exit when Tok = Tok_Rcurl;
         Expect (Tok, Tok_Comma);
      end loop;

      Scan_Expect (Tok_Rcurl);
   end Parse_File_Entry;

   procedure Parse_File
   is
      use Mini_Json;
      use Errorout;
      Tok : Token;
   begin
      Scan_Expect (Tok_Lcurl);

      --  Skip header.
      loop
         Scan_Expect (Tok_String);
         Scan_Expect (Tok_Colon);
         Tok := Scan;
         exit when Tok = Tok_Lbrac;
         Expect (Tok, Tok_String);
         Scan_Expect (Tok_Comma);
      end loop;

      --  For each file.
      loop
         Parse_File_Entry;

         Tok := Scan;
         exit when Tok = Tok_Rbrac;
         Expect (Tok, Tok_Comma);
      end loop;

      Scan_Expect (Tok_Rcurl);
      Scan_Expect (Tok_Eof);

   exception
      when Parse_Error =>
         Error_Msg_Option ("parse error at line %v",
                           (1 => +Int32 (Get_Line)));
   end Parse_File;

   procedure Read_Coverage_File (Filename : String)
   is
      use Errorout;
      File_Id : Name_Id;
      File : Source_File_Entry;
   begin
      File_Id := Name_Table.Get_Identifier (Filename);
      File := Files_Map.Read_Source_File (Null_Identifier, File_Id);
      if File = No_Source_File_Entry then
         Error_Msg_Option ("cannot open file %i", (1 => +File_Id));
         return;
      end if;

      Mini_Json.Init (File);
      Parse_File;
      Files_Map.Unload_Last_Source_File (File);
   end Read_Coverage_File;

   function Get_EOL_Pos (Buf : File_Buffer_Acc; Pos : Source_Ptr)
                        return Source_Ptr
   is
      Npos : Source_Ptr;
      C : Character;
   begin
      Npos := Pos;
      loop
         C := Buf (Npos);
         exit when C = ASCII.CR or C = ASCII.LF or C = Files_Map.EOT;
         Npos := Npos + 1;
      end loop;
      return Npos;
   end Get_EOL_Pos;

   function Skip_EOL (Buf : File_Buffer_Acc; Pos : Source_Ptr)
                     return Source_Ptr
   is
   begin
      case Buf (Pos) is
         when ASCII.CR =>
            if Buf (Pos + 1) = ASCII.LF then
               return Pos + 2;
            else
               return Pos + 1;
            end if;
         when ASCII.LF =>
            if Buf (Pos + 1) = ASCII.CR then
               return Pos + 2;
            else
               return Pos + 1;
            end if;
         when ASCII.EOT =>
            return Pos;
         when others =>
            raise Program_Error;
      end case;
   end Skip_EOL;

   procedure Output_Gcov_File (Rec : File_Record_Acc)
   is
      use Files_Map;
      use Grt.Astdio;
      use Name_Table;
      Lines : constant Line_Acc := Rec.Lines;
      Name, Dir : Name_Id;
      Sfe : Source_File_Entry;
      F : FILEs;
      Buf : File_Buffer_Acc;
      Pos : Source_Ptr;
      Epos : Source_Ptr;
      Line : Positive;
   begin
      Name := Rec.Name;
      Dir := Rec.Dir;
      Normalize_Pathname (Dir, Name);
      Sfe := Read_Source_File (Dir, Name);
      if Sfe = No_Source_File_Entry then
         Errorout.Error_Msg_Option
           ("cannot open source file " & Image (Name) & '"');
         return;
      end if;

      F := Fopen_W (Image (Name) & ".gcov");
      if F = NULL_Stream then
         return;
      end if;

      Put_Line (F, "     -:    0:Source:" & Image (Name));
      Put_Line (F, "     -:    0:Working directory:" & Image (Dir));

      Line := 1;
      Buf := Get_File_Source (Sfe);
      Pos := Source_Ptr_Org;

      loop
         Epos := Get_EOL_Pos (Buf, Pos);
         exit when Epos = Pos and then Buf (Pos) = EOT;

         --  Status
         if Line > Lines'Last or else not Lines (Line).Coverage then
            Put (F, "     -:");
         elsif Lines (Line).Covered then
            Put (F, "     1:");
         else
            Put (F, " #####:");
         end if;

         --  Line number
         declare
            Ln : constant String := Natural'Image (Line);
         begin
            Put (F, (1 .. 5 - Ln'Length => ' '));
            Put (F, Ln);
            Put (F, ':');
         end;

         --  Line
         declare
            subtype S is String (Natural (Pos + 1) .. Natural (Epos));
         begin
            Put (F, S (Buf (Pos .. Epos - 1)));
         end;
         New_Line (F);
         Line := Line + 1;
         Pos := Skip_EOL (Buf, Epos);
      end loop;

      fclose (F);
   end Output_Gcov_File;

   procedure Output_Gcov is
   begin
      for I in Res_Tables.First .. Res_Tables.Last loop
         Output_Gcov_File (Res_Tables.Table (I));
      end loop;
   end Output_Gcov;

   procedure Output_Gcovr (F : FILEs)
   is
      use Grt.Astdio;
      use Grt.Types;
      use Name_Table;
   begin
      Put_Line (F, "{");
      Put_Line (F, "  ""gcovr/format_version"": ""0.6"",");
      Put_Line (F, "  ""files"": [");
      for I in Res_Tables.First .. Res_Tables.Last loop
         declare
            Rec : constant File_Record_Acc := Res_Tables.Table (I);
            Lines : constant Line_Acc := Rec.Lines;
            Name, Dir : Name_Id;
            First : Boolean;
         begin
            Name := Rec.Name;
            Dir := Rec.Dir;
            Files_Map.Normalize_Pathname (Dir, Name);
            Put_Line (F, "    {");
            Put_Line (F, "      ""file"": """
                        & Image (Dir) & Image (Name) & """,");
            Put_Line (F, "      ""lines"": [");
            First := True;
            for I in Lines'Range loop
               if Lines (I).Coverage then
                  if First then
                     First := False;
                  else
                     Put_Line (F, ",");
                  end if;
                  Put (F, "        { ""branches"": []");
                  Put (F, ", ""count"": ");
                  Put_U32 (F, Boolean'Pos (Lines (I).Covered));
                  Put (F, ", ""line_number"": ");
                  Put_U32 (F, Ghdl_U32 (I));
                  Put (F, " }");
               end if;
            end loop;
            if not First then
               New_Line (F);
            end if;
            Put_Line (F, "      ],");
            Put_Line (F, "      ""functions"": []");

            Put (F, "    }");
            if I /= Res_Tables.Last then
               Put_Line (F, ",");
            else
               New_Line (F);
            end if;
         end;
      end loop;
      Put_Line (F, "  ]");
      Put_Line (F, "}");
   end Output_Gcovr;

   procedure Output_Lcov is
   begin
      --  No test name
      Put_Line ("TN:");
      for I in Res_Tables.First .. Res_Tables.Last loop
         declare
            use Name_Table;
            Rec : constant File_Record_Acc := Res_Tables.Table (I);
            Lines : constant Line_Acc := Rec.Lines;
            Name, Dir : Name_Id;
            Fn_Cov : Boolean;
         begin
            Name := Rec.Name;
            Dir := Rec.Dir;
            Files_Map.Normalize_Pathname (Dir, Name);
            Put_Line ("SF:" & Image (Dir) & Image (Name));
            --  No functions...
            Put_Line ("FN:1:file");
            Fn_Cov := False;
            for I in Lines'Range loop
               if Lines (I).Coverage and Lines (I).Covered then
                  Fn_Cov := True;
                  exit;
               end if;
            end loop;
            Put ("FNDA:");
            Put_Uns32 (Boolean'Pos (Fn_Cov));
            Put_Line (",file");
            for I in Lines'Range loop
               if Lines (I).Coverage then
                  Put ("DA:");
                  Put_Uns32 (Uns32 (I));
                  Put (',');
                  Put_Uns32 (Boolean'Pos (Lines (I).Covered));
                  New_Line;
               end if;
            end loop;
            Put_Line("end_of_record");
         end;
      end loop;
   end Output_Lcov;

   procedure Output_Ratio (Num_Lines : Uns32; Num_Covered : Uns32)
   is
      Ratio : Uns32;
   begin
      Put_Uns32 (Num_Covered);
      Put ("/");
      Put_Uns32 (Num_Lines);
      if Num_Lines = 0 then
         --  Avoid division by 0.
         Ratio := 0;
      else
         Ratio := Num_Covered * 1000 / Num_Lines;
      end if;
      Put (" ");
      Put_Uns32 (Ratio / 10);
      Put ('.');
      Put_Uns32 (Ratio mod 10);
      Put ('%');
   end Output_Ratio;

   procedure Output_Summary
   is
      Total_Lines : Uns32;
      Total_Covered : Uns32;
   begin
      Total_Lines := 0;
      Total_Covered := 0;
      for I in Res_Tables.First .. Res_Tables.Last loop
         declare
            use Name_Table;
            Rec : constant File_Record_Acc := Res_Tables.Table (I);
            Lines : constant Line_Acc := Rec.Lines;
            Num_Lines, Num_Covered : Uns32;
         begin
            Put (Image (Rec.Name));
            Put (' ');
            Num_Lines := 0;
            Num_Covered := 0;
            for I in Lines'Range loop
               if Lines (I).Coverage then
                  Num_Lines := Num_Lines + 1;
                  if Lines (I).Covered then
                     Num_Covered := Num_Covered + 1;
                  end if;
               end if;
            end loop;
            Output_Ratio (Num_Lines, Num_Covered);
            New_Line;

            Total_Lines := Total_Lines + Num_Lines;
            Total_Covered := Total_Covered + Num_Covered;
         end;
      end loop;

      Put ("Summary: ");
      Output_Ratio (Total_Lines, Total_Covered);
      New_Line;
   end Output_Summary;

   type Format_Type is
     (
      Format_Gcov,
      Format_Lcov,
      Format_Gcovr,
      Format_Summary
     );

   type Command_Coverage is new Command_Type with record
      Format : Format_Type := Format_Summary;
      Output_Filename : String_Acc := null;
   end record;

   function Decode_Command (Cmd : Command_Coverage; Name : String)
                           return Boolean;
   procedure Decode_Option (Cmd : in out Command_Coverage;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);

   function Get_Short_Help (Cmd : Command_Coverage) return String;
   procedure Disp_Long_Help (Cmd : Command_Coverage);
   procedure Perform_Action (Cmd : in out Command_Coverage;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Coverage; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "coverage";
   end Decode_Command;

   procedure Decode_Option (Cmd : in out Command_Coverage;
                            Option : String;
                            Arg : String;
                            Res : out Option_State) is
   begin
      if Option = "-o" then
         if Arg = "" then
            Res := Option_Arg_Req;
         else
            Cmd.Output_Filename := new String'(Arg);
            Res := Option_Arg;
         end if;
      elsif Option = "--format=lcov" then
         Cmd.Format := Format_Lcov;
         Res := Option_Ok;
      elsif Option = "--format=gcov" then
         Cmd.Format := Format_Gcov;
         Res := Option_Ok;
      elsif Option = "--format=gcovr" then
         Cmd.Format := Format_Gcovr;
         Res := Option_Ok;
      elsif Option = "--format=summary" then
         Cmd.Format := Format_Summary;
         Res := Option_Ok;
      else
         Decode_Option (Command_Type (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   function Get_Short_Help (Cmd : Command_Coverage) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "coverage [--format=FORMAT] [-o FILENAME] COV-FILES..."
        & ASCII.LF & "  Gather and format coverage data";
   end Get_Short_Help;

   procedure Disp_Long_Help (Cmd : Command_Coverage)
   is
      pragma Unreferenced (Cmd);
   begin
      Put_Line ("  -o FILENAME      specify result file (for gcovr format)");
      Put_Line (" --format=gcov     create .gcov files in current directory");
      Put_Line (" --format=lcov     output lcov tracefile (for genhtml)");
      Put_Line (" --format=summary  print coverage ratio per line and total");
   end Disp_Long_Help;

   procedure Perform_Action (Cmd : in out Command_Coverage;
                             Args : String_Acc_Array;
                             Success : out Boolean) is
   begin
      for I in Args'Range loop
         Read_Coverage_File (Args (I).all);
      end loop;
      case Cmd.Format is
         when Format_Gcov =>
            Output_Gcov;
         when Format_Lcov =>
            Output_Lcov;
         when Format_Gcovr =>
            declare
               F : FILEs;
            begin
               if Cmd.Output_Filename = null then
                  F := Grt.Stdio.stdout;
               else
                  F := Fopen_W (Cmd.Output_Filename.all);
                  if F = NULL_Stream then
                     Success := False;
                     return;
                  end if;
               end if;
               Output_Gcovr (F);
            end;
         when Format_Summary =>
            Output_Summary;
      end case;
      Success := True;
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Coverage);
   end Register_Commands;
end Ghdlcov;
