--  GHDL Run Time (GRT) - SDF parser.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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

with Grt.Stdio; use Grt.Stdio;
with Grt.C; use Grt.C;
with Grt.Strings; use Grt.Strings;
with Grt.Errors; use Grt.Errors;
with Ada.Unchecked_Deallocation;
with Grt.Vital_Annotate;

package body Grt.Sdf is
   use ASCII;

   type Sdf_Token_Type is
     (
      Tok_Oparen, -- (
      Tok_Cparen, -- )
      Tok_Qstring,
      Tok_Identifier,
      Tok_Rnumber,
      Tok_Dnumber,
      Tok_Div, -- /
      Tok_Dot, -- .
      Tok_Cln, -- :

      Tok_Error,
      Tok_Eof
     );

   type Sdf_Context_Acc is access Sdf_Context_Type;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => Sdf_Context_Acc, Object => Sdf_Context_Type);

   Sdf_Context : Sdf_Context_Acc;

   --  Current data read from the file.
   Buf : String_Access (1 .. Buf_Size) := null;

   --  Length of the buffer, including the EOT.
   Buf_Len : Natural;
   Pos : Natural;
   Line_Start : Integer;

   Sdf_Stream : FILEs := NULL_Stream;
   Sdf_Filename : String_Access := null;
   Sdf_Line : Natural;

   function Open_Sdf (Filename : String) return Boolean
   is
      N_Filename : String (1 .. Filename'Length + 1);
      Mode : constant String := "rt" & ASCII.NUL;
   begin
      N_Filename (1 .. Filename'Length) := Filename;
      N_Filename (N_Filename'Last) := ASCII.NUL;
      Sdf_Stream := fopen (N_Filename'Address, Mode'Address);
      if Sdf_Stream = NULL_Stream then
         Error_S ("cannot open SDF file '");
         Diag_C (Filename);
         Error_E ("'");
         return False;
      end if;
      Sdf_Context := new Sdf_Context_Type;

      Sdf_Context.Version := Sdf_Version_Unknown;

      --  Set the timescale to 1 ns.
      Sdf_Context.Timescale := 1000;

      Buf := new String (1 .. Buf_Size);
      Buf_Len := 1;
      Buf (1) := EOT;
      Sdf_Line := 1;
      Sdf_Filename := new String'(Filename);
      Pos := 1;
      Line_Start := 1;
      return True;
   end Open_Sdf;

   procedure Close_Sdf
   is
   begin
      fclose (Sdf_Stream);
      Sdf_Stream := NULL_Stream;
      Unchecked_Deallocation (Sdf_Context);
      Unchecked_Deallocation (Buf);
   end Close_Sdf;

   procedure Read_Sdf
   is
      Res : size_t;
   begin
      Res := fread (Buf (Pos)'Address, 1, size_t (Read_Size), Sdf_Stream);
      Line_Start := Line_Start - Buf_Len + Pos;
      Buf_Len := Pos + Natural (Res);
      Buf (Buf_Len) := EOT;
   end Read_Sdf;


   Ident_Start : Natural;
   Ident_End : Natural;

   procedure Read_Append
   is
      Len : Natural;
   begin
      Len := Pos - Ident_Start;
      if Ident_Start = 1 or Len >= 1024 then
         Error_S ("SDF line ");
         Diag_C (Sdf_Line);
         Error_E (" is too long");
         return;
      end if;
      Buf (1 .. Len) := Buf (Ident_Start .. Ident_Start + Len - 1);
      Pos := Len + 1;
      Ident_Start := 1;
      Read_Sdf;
   end Read_Append;

   procedure Error_S_Sdf is
   begin
      Error_S (Sdf_Filename.all);
      Diag_C (':');
      Diag_C (Sdf_Line);
      Diag_C (':');
      Diag_C (Pos - Line_Start);
      Diag_C (": ");
   end Error_S_Sdf;

   procedure Error_Sdf (Msg : String) is
   begin
      Error_S_Sdf;
      Error_E (Msg);
   end Error_Sdf;

   procedure Error_Bad_Character is
   begin
      Error_Sdf ("bad character in SDF file");
   end Error_Bad_Character;

   procedure Scan_Identifier is
   begin
      Ident_Start := Pos;
      loop
         Pos := Pos + 1;
         case Buf (Pos) is
            when 'a' .. 'z'
              | 'A' .. 'Z'
              | '0' .. '9'
              | '_' =>
               null;
            when '\' =>
               Error_Sdf ("escape character not handled");
               Ident_End := Pos - 1;
               return;
            when EOT =>
               --  Continue to read.
               Read_Append;
               Pos := Pos - 1;
            when others =>
               Ident_End := Pos - 1;
               return;
         end case;
      end loop;
   end Scan_Identifier;

   function Ident_Length return Natural is
   begin
      return Ident_End - Ident_Start + 1;
   end Ident_Length;

   function Is_Ident (Str : String) return Boolean
   is
   begin
      if Ident_Length /= Str'Length then
         return False;
      end if;
      return Buf (Ident_Start .. Ident_End) = Str;
   end Is_Ident;

   procedure Scan_Qstring
   is
   begin
      Ident_Start := Pos + 1;
      loop
         Pos := Pos + 1;
         case Buf (Pos) is
            when EOT =>
               --  Continue to read.
               Read_Append;
               Pos := Pos - 1;
            when ASCII.NUL .. Character'Val (3)
              | Character'Val (5) .. Character'Val (31)
              | Character'Val (127) .. Character'Val (255) =>
               Error_Bad_Character;
            when ' '
              | '!'
              | '#' .. '~' =>
               null;
            when '"' => -- "
               Ident_End := Pos - 1;
               Pos := Pos + 1;
               exit;
         end case;
      end loop;
   end Scan_Qstring;

   Scan_Int : Integer;
   Scan_Exp : Integer;

   function Scan_Number return Sdf_Token_Type
   is
      Has_Dot : Boolean;
      Is_Negative : Boolean;
   begin
      Has_Dot := False;
      Is_Negative := False;
      Scan_Int := 0;
      Scan_Exp := 0;
      loop
         case Buf (Pos) is
            when '0' .. '9' =>
               Scan_Int := Scan_Int * 10
                 + Character'Pos (Buf (Pos)) - Character'Pos ('0');
               if Has_Dot then
                  Scan_Exp := Scan_Exp - 1;
               end if;
               Pos := Pos + 1;
            when '.' =>
               if Has_Dot then
                  Error_Bad_Character;
                  return Tok_Error;
               else
                  Has_Dot := True;
               end if;
               Pos := Pos + 1;
            when '-' =>
               if Is_Negative then
                  Error_Bad_Character;
                  return Tok_Error;
               else
                  Is_Negative := True;
               end if;
               Pos := Pos + 1;
            when EOT =>
               if Pos /= Buf_Len then
                  Error_Bad_Character;
                  return Tok_Error;
               end if;
               Pos := 1;
               Read_Sdf;
               exit when Buf_Len = 1;
            when others =>
               exit;
         end case;
      end loop;
      if Is_Negative then
         Scan_Int := -Scan_Int;
      end if;
      if Has_Dot then
         return Tok_Rnumber;
      else
         return Tok_Dnumber;
      end if;
   end Scan_Number;

   procedure Refill_Buf is
   begin
      Buf (1 .. Buf_Len - Pos) := Buf (Pos .. Buf_Len - 1);
      Pos := Buf_Len - Pos + 1;
      Read_Sdf;
      Pos := 1;
   end Refill_Buf;

   procedure Skip_Spaces is
   begin
      --  Fast blanks skipping.
      while Buf (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      loop
         --  Be sure there is at least 1 character.
         if Pos + 1 >= Buf_Len then
            Refill_Buf;
         end if;

         case Buf (Pos) is
            when EOT =>
               if Pos /= Buf_Len then
                  return;
               end if;
               Pos := 1;
               Read_Sdf;
               if Buf_Len = 1 then
                  return;
               end if;
            when LF =>
               Pos := Pos + 1;
               if Buf (Pos) = CR then
                  Pos := Pos + 1;
               end if;
               Line_Start := Pos;
               Sdf_Line := Sdf_Line + 1;
            when CR =>
               Pos := Pos + 1;
               if Buf (Pos) = LF then
                  Pos := Pos + 1;
               end if;
               Line_Start := Pos;
               Sdf_Line := Sdf_Line + 1;
            when ' '
              | HT =>
               Pos := Pos + 1;
            when '/' =>
               if Buf (Pos + 1) = '/' then
                  Pos := Pos + 2;
                  --  Skip line comment.
                  loop
                     exit when Buf (Pos) = CR;
                     exit when Buf (Pos) = LF;
                     exit when Buf (Pos) = EOT;
                     Pos := Pos + 1;
                     if Pos >= Buf_Len then
                        Refill_Buf;
                     end if;
                  end loop;
               else
                  return;
               end if;
            when others =>
               return;
         end case;
      end loop;
   end Skip_Spaces;

   function Get_Token return Sdf_Token_Type is
   begin
      Skip_Spaces;

      --  Be sure there is at least 4 characters.
      if Pos + 4 >= Buf_Len then
         Refill_Buf;
      end if;

      case Buf (Pos) is
         when EOT =>
            if Buf_Len = 1 then
               return Tok_Eof;
            else
               Error_Bad_Character;
               return Tok_Error;
            end if;
         when '"' => -- "
            Scan_Qstring;
            return Tok_Qstring;
         when '/' =>
            --  Skip_Spaces has already handled line comments.
            Pos := Pos + 1;
            return Tok_Div;
         when '.' =>
            Pos := Pos + 1;
            return Tok_Dot;
         when ':' =>
            Pos := Pos + 1;
            return Tok_Cln;
         when '(' =>
            Pos := Pos + 1;
            return Tok_Oparen;
         when ')' =>
            Pos := Pos + 1;
            return Tok_Cparen;
         when 'a' .. 'z'
           | 'A' .. 'Z' =>
            Scan_Identifier;
            return Tok_Identifier;
         when '0' .. '9'
           | '-' =>
            return Scan_Number;
         when others =>
            Error_Bad_Character;
            return Tok_Error;
      end case;
   end Get_Token;

   function Is_White_Space (C : Character) return Boolean is
   begin
      case C is
         when ' '
           | HT
           | CR
           | LF =>
            return True;
         when others =>
            return False;
      end case;
   end Is_White_Space;

   function Get_Edge_Token return Edge_Type is
   begin
      Skip_Spaces;

      --  Be sure there is at least 4 characters.
      if Pos + 4 >= Buf_Len then
         Refill_Buf;
      end if;

      case Buf (Pos) is
         when '0' =>
            if Is_White_Space (Buf (Pos + 2)) then
               if Buf (Pos + 1) = 'z' then
                  Pos := Pos + 2;
                  return Edge_0z;
               elsif Buf (Pos + 1) = '1' then
                  Pos := Pos + 2;
                  return Edge_01;
               end if;
            end if;
         when '1' =>
            if Is_White_Space (Buf (Pos + 2)) then
               if Buf (Pos + 1) = 'z' then
                  Pos := Pos + 2;
                  return Edge_1z;
               elsif Buf (Pos + 1) = '0' then
                  Pos := Pos + 2;
                  return Edge_10;
               end if;
            end if;
         when 'z' =>
            if Is_White_Space (Buf (Pos + 2)) then
               if Buf (Pos + 1) = '0' then
                  Pos := Pos + 2;
                  return Edge_Z0;
               elsif Buf (Pos + 1) = '1' then
                  Pos := Pos + 2;
                  return Edge_Z1;
               end if;
            end if;
         when 'p' =>
            Scan_Identifier;
            if Is_Ident ("posedge") then
               return Edge_Posedge;
            end if;
         when 'n' =>
            Scan_Identifier;
            if Is_Ident ("negedge") then
               return Edge_Negedge;
            end if;
         when others =>
            null;
      end case;
      Error_Sdf ("edge_identifier expected");
      return Edge_Error;
   end Get_Edge_Token;

   procedure Error_Sdf (Tok : Sdf_Token_Type)
   is
   begin
      case Tok is
         when Tok_Qstring =>
            Error_Sdf ("qstring expected");
         when Tok_Oparen =>
            Error_Sdf ("'(' expected");
         when Tok_Identifier =>
            Error_Sdf ("identifier expected");
         when Tok_Cln =>
            Error_Sdf ("':' (colon) expected");
         when others =>
            Error_Sdf ("parse error");
      end case;
   end Error_Sdf;

   function Expect (Tok : Sdf_Token_Type) return Boolean
   is
   begin
      if Get_Token = Tok then
         return True;
      end if;
      Error_Sdf (Tok);
      return False;
   end Expect;

   function Expect_Cp_Op_Ident (Tok : Sdf_Token_Type) return Boolean
   is
   begin
      if Tok /= Tok_Cparen then
         Error_Sdf (Tok_Cparen);
         return False;
      end if;
      if not Expect (Tok_Oparen)
        or else not Expect (Tok_Identifier)
      then
         return False;
      end if;
      return True;
   end Expect_Cp_Op_Ident;

   function Expect_Qstr_Cp_Op_Ident (Str : String) return Boolean
   is
      Tok : Sdf_Token_Type;
   begin
      if not Is_Ident (Str) then
         return True;
      end if;

      Tok := Get_Token;
      if Tok = Tok_Qstring then
         Tok := Get_Token;
      end if;

      return Expect_Cp_Op_Ident (Tok);
   end Expect_Qstr_Cp_Op_Ident;

   procedure Start_Generic_Name (Kind : Timing_Generic_Kind) is
   begin
      Sdf_Context.Kind := Kind;
      Sdf_Context.Port_Num := 0;
      Sdf_Context.Ports (1).L := Invalid_Dnumber;
      Sdf_Context.Ports (2).L := Invalid_Dnumber;
      Sdf_Context.Ports (1).Edge := Edge_None;
      Sdf_Context.Ports (2).Edge := Edge_None;
   end Start_Generic_Name;

   --  Status of a parsing.
   --  ERROR: parse error (syntax is not correct)
   --  ALTERN: alternate construct parsed (ie simple RNUMBER for tc_rvalue).
   --  OPTIONAL: the construct is absent.
   --  FOUND: the construct is present.
   --  SET: the construct is present and a value was extracted from.
   type Parse_Status_Type is
     (
      Status_Error,
      Status_Altern,
      Status_Optional,
      Status_Found,
      Status_Set
     );

   function Num_To_Time return Ghdl_I64
   is
      Res : Ghdl_I64;
   begin
      Res := Ghdl_I64 (Scan_Int) * Ghdl_I64 (Sdf_Context.Timescale);
      while Scan_Exp < 0 loop
         Res := Res / 10;
         Scan_Exp := Scan_Exp + 1;
      end loop;
      return Res;
   end Num_To_Time;

   --  Parse: REXPRESSION? ')'
   procedure Parse_Rexpression
     (Status : out Parse_Status_Type; Val : out Ghdl_I64)
   is
      Tok : Sdf_Token_Type;

      procedure Pr_Rnumber (Mtm : Mtm_Type)
      is
      begin
         if Tok = Tok_Rnumber or Tok = Tok_Dnumber then
            if Mtm = Sdf_Mtm then
               Val := Num_To_Time;
               Status := Status_Set;
            elsif Status /= Status_Set then
               Status := Status_Found;
            end if;
            Tok := Get_Token;
         end if;
      end Pr_Rnumber;

      function Pr_Colon return Boolean
      is
      begin
         if Tok /= Tok_Cln then
            Error_Sdf (Tok_Cln);
            Status := Status_Error;
            return False;
         else
            Tok := Get_Token;
            return True;
         end if;
      end Pr_Colon;

   begin
      Val := 0;
      Tok := Get_Token;
      Status := Status_Error;
      if Tok = Tok_Cparen then
         Status := Status_Optional;
         return;
      end if;

      Pr_Rnumber (Minimum);

      if not Pr_Colon then
         return;
      end if;

      Pr_Rnumber (Typical);

      if not Pr_Colon then
         return;
      end if;

      Pr_Rnumber (Maximum);

      if Status = Status_Error then
         Error_Sdf ("at least one number required in an rexpression");
         return;
      end if;

      if Tok /= Tok_Cparen then
         Error_Sdf (Tok_Cparen);
         Status := Status_Error;
      end if;
   end Parse_Rexpression;

   function Expect_Rexpr_Cp_Op_Ident return Boolean
   is
      Status : Parse_Status_Type;
      Val : Ghdl_I64;
   begin
      Parse_Rexpression (Status, Val);
      if Status = Status_Error then
         return False;
      end if;
      if not Expect (Tok_Oparen)
        or else not Expect (Tok_Identifier)
      then
         Error_Sdf (Tok_Identifier);
         return False;
      end if;
      return True;
   end Expect_Rexpr_Cp_Op_Ident;

   function Parse_Port_Path1 (Tok : Sdf_Token_Type) return Boolean
   is
      Port_Spec : Port_Spec_Type
         renames Sdf_Context.Ports (Sdf_Context.Port_Num);
      Len : Natural;
   begin
      if Tok /= Tok_Identifier then
         Error_Sdf ("port path expected");
         return False;
      end if;
      Len := 0;
      for I in Ident_Start .. Ident_End loop
         Len := Len + 1;
         Port_Spec.Name (Len) := To_Lower (Buf (I));
      end loop;
      Port_Spec.Name_Len := Len;

      --  Parse   [ DNUMBER ]
      --        | [ DNUMBER : DNUMBER ]
      Skip_Spaces;
      if Buf (Pos) = '[' then
         Port_Spec.R := Invalid_Dnumber;
         Pos := Pos + 1;
         if Get_Token /= Tok_Dnumber then
            Error_Sdf (Tok);
         else
            Port_Spec.L := Ghdl_I32 (Scan_Int);
         end if;
         Skip_Spaces;
         if Buf (Pos) = ':' then
            Pos := Pos + 1;
            if Get_Token /= Tok_Dnumber then
               Error_Sdf (Tok);
            else
               Port_Spec.R := Ghdl_I32 (Scan_Int);
            end if;
            Skip_Spaces;
         end if;
         if Buf (Pos) /= ']' then
            Error_Sdf ("']' expected");
         else
            Pos := Pos + 1;
         end if;
      end if;

      return True;
   end Parse_Port_Path1;

   function Parse_Port_Path return Boolean
   is
   begin
      Sdf_Context.Port_Num := Sdf_Context.Port_Num + 1;
      return Parse_Port_Path1 (Get_Token);
   end Parse_Port_Path;

   function Parse_Port_Spec return Boolean
   is
      Tok : Sdf_Token_Type;
      Edge : Edge_Type;
   begin
      Sdf_Context.Port_Num := Sdf_Context.Port_Num + 1;
      Tok := Get_Token;
      if Tok = Tok_Identifier then
         return Parse_Port_Path1 (Tok);
      elsif Tok /= Tok_Oparen then
         Error_Sdf ("port spec expected");
         return False;
      end if;
      Edge := Get_Edge_Token;
      if Edge = Edge_Error then
         return False;
      end if;
      Sdf_Context.Ports (Sdf_Context.Port_Num).Edge := Edge;
      if not Parse_Port_Path1 (Get_Token) then
         return False;
      end if;
      if Get_Token /= Tok_Cparen then
         Error_Sdf (Tok_Cparen);
         return False;
      end if;
      return True;
   end Parse_Port_Spec;

   function Parse_Port_Tchk return Boolean renames Parse_Port_Spec;

   --  tc_rvalue ::= ( RNUMBER )
   --            ||= ( rexpression )
   --  Return status_optional for ( )
   function Parse_Tc_Rvalue return Parse_Status_Type
   is
      Tok : Sdf_Token_Type;
      Res : Parse_Status_Type;
   begin
      --  '('
      if Get_Token /= Tok_Oparen then
         Error_Sdf (Tok_Oparen);
         return Status_Error;
      end if;
      Res := Status_Found;
      Tok := Get_Token;
      if Tok = Tok_Rnumber or Tok = Tok_Dnumber then
         Sdf_Context.Timing (1) := Num_To_Time;
         Tok := Get_Token;
         if Tok = Tok_Cparen then
            --  This is a simple RNUMBER.
            return Status_Altern;
         end if;
         if Sdf_Mtm = Minimum then
            Res := Status_Set;
         end if;
      end if;
      if Tok = Tok_Cparen then
         return Status_Optional;
      end if;
      if Tok /= Tok_Cln then
         Error_Sdf (Tok_Cln);
         return Status_Error;
      end if;
      Tok := Get_Token;
      if Tok = Tok_Rnumber or Tok = Tok_Dnumber then
         if Sdf_Mtm = Typical then
            Sdf_Context.Timing (1) := Num_To_Time;
            Res := Status_Set;
         end if;
         Tok := Get_Token;
      end if;
      if Tok /= Tok_Cln then
         Error_Sdf (Tok_Cln);
         return Status_Error;
      end if;
      Tok := Get_Token;
      if Tok = Tok_Rnumber or Tok = Tok_Dnumber then
         if Sdf_Mtm = Maximum then
            Sdf_Context.Timing (1) := Num_To_Time;
            Res := Status_Set;
         end if;
         Tok := Get_Token;
      end if;
      if Tok /= Tok_Cparen then
         Error_Sdf (Tok_Cparen);
         return Status_Error;
      end if;
      return Res;
   end Parse_Tc_Rvalue;

   function Parse_Simple_Tc_Rvalue return Boolean is
   begin
      Sdf_Context.Timing_Nbr := 0;

      case Parse_Tc_Rvalue is
         when Status_Error
           | Status_Optional =>
            return False;
         when Status_Altern =>
            null;
         when Status_Found =>
            Sdf_Context.Timing_Set (1) := False;
         when Status_Set =>
            Sdf_Context.Timing_Set (1) := True;
      end case;
      return True;
   end Parse_Simple_Tc_Rvalue;

   --  rvalue ::= ( RNUMBER )
   --         ||= rexp_list
   --  Parse: rvalue )
   function Parse_Rvalue return Boolean
   is
      Tok : Sdf_Token_Type;
   begin
      Sdf_Context.Timing_Nbr := 0;
      Sdf_Context.Timing_Set := (others => False);

      case Parse_Tc_Rvalue is
         when Status_Error =>
            return False;
         when Status_Altern =>
            Sdf_Context.Timing_Nbr := 1;
            if Get_Token /= Tok_Cparen then
               Error_Sdf (Tok_Cparen);
            end if;
            return True;
         when Status_Found
           | Status_Optional =>
            null;
         when Status_Set =>
            Sdf_Context.Timing_Set (1) := True;
      end case;

      Sdf_Context.Timing_Nbr := 1;
      loop
         Tok := Get_Token;
         exit when Tok = Tok_Cparen;
         if Tok /= Tok_Oparen then
            Error_Sdf (Tok_Oparen);
            return False;
         end if;

         Sdf_Context.Timing_Nbr := Sdf_Context.Timing_Nbr + 1;
         declare
            Status : Parse_Status_Type;
            Val : Ghdl_I64;
         begin
            Parse_Rexpression (Status, Val);
            case Status is
               when Status_Error
                 | Status_Altern =>
                  return False;
               when Status_Optional
                 | Status_Found =>
                  null;
               when Status_Set =>
                  Sdf_Context.Timing_Set (Sdf_Context.Timing_Nbr) := True;
                  Sdf_Context.Timing (Sdf_Context.Timing_Nbr) := Val;
            end case;
         end;
      end loop;
      if Boolean'(False) then
         --  Do not expand here, since the most used is 01.
         case Sdf_Context.Timing_Nbr is
            when 1 =>
               for I in 2 .. 6 loop
                  Sdf_Context.Timing (I) := Sdf_Context.Timing (1);
                  Sdf_Context.Timing_Set (I) := Sdf_Context.Timing_Set (1);
               end loop;
            when 2 =>
               for I in 3 .. 4 loop
                  Sdf_Context.Timing (I) := Sdf_Context.Timing (1);
                  Sdf_Context.Timing_Set (I) := Sdf_Context.Timing_Set (1);
               end loop;
               for I in 5 .. 6 loop
                  Sdf_Context.Timing (I) := Sdf_Context.Timing (2);
                  Sdf_Context.Timing_Set (I) := Sdf_Context.Timing_Set (2);
               end loop;
            when 3 =>
               for I in 4 .. 6 loop
                  Sdf_Context.Timing (I) := Sdf_Context.Timing (I - 3);
                  Sdf_Context.Timing_Set (I) := Sdf_Context.Timing_Set (I - 3);
               end loop;
            when 6
              | 12 =>
               null;
            when others =>
               Error_Sdf ("bad number of rvalue");
               return False;
         end case;
      end if;
      return True;
   end Parse_Rvalue;

   function Handle_Generic return Boolean
   is
      Name : String (1 .. 1024);
      Len : Natural;

      procedure Start (Str : String) is
      begin
         Name (1 .. Str'Length) := Str;
         Len := Str'Length;
      end Start;

      procedure Add (Str : String)
      is
         Nlen : Natural;
      begin
         Len := Len + 1;
         Name (Len) := '_';
         Nlen := Len + Str'Length;
         Name (Len + 1 .. Nlen) := Str;
         Len := Nlen;
      end Add;

      procedure Add_Edge (Edge : Edge_Type; Force : Boolean) is
      begin
         case Edge is
            when Edge_Posedge =>
               Add ("posedge");
            when Edge_Negedge =>
               Add ("negedge");
            when Edge_01 =>
               Add ("01");
            when Edge_10 =>
               Add ("10");
            when Edge_0z =>
               Add ("0z");
            when Edge_Z1 =>
               Add ("Z1");
            when Edge_1z =>
               Add ("1z");
            when Edge_Z0 =>
               Add ("ZO");
            when Edge_None =>
               if Force then
                  Add ("noedge");
               end if;
            when Edge_Error =>
               Add ("?");
         end case;
      end Add_Edge;

      Ok : Boolean;
   begin
      case Sdf_Context.Kind is
         when Delay_Iopath =>
            Start ("tpd");
         when Delay_Port =>
            Start ("tipd");
         when Timingcheck_Setup =>
            Start ("tsetup");
         when Timingcheck_Hold =>
            Start ("thold");
         when Timingcheck_Setuphold =>
            Start ("tsetup");
         when Timingcheck_Recovery =>
            Start ("trecovery");
         when Timingcheck_Removal =>
            Start ("tremoval");
         when Timingcheck_Skew =>
            Start ("tskew");
         when Timingcheck_Width =>
            Start ("tpw");
         when Timingcheck_Period =>
            Start ("tperiod");
         when Timingcheck_Nochange =>
            Start ("tncsetup");
      end case;
      for I in 1 .. Sdf_Context.Port_Num loop
         Add (Sdf_Context.Ports (I).Name
              (1 .. Sdf_Context.Ports (I).Name_Len));
      end loop;
      if Sdf_Context.Kind in Timing_Generic_Full_Condition then
         Add_Edge (Sdf_Context.Ports (1).Edge, True);
         Add_Edge (Sdf_Context.Ports (2).Edge, False);
      elsif Sdf_Context.Kind in Timing_Generic_Simple_Condition then
         Add_Edge (Sdf_Context.Ports (1).Edge, False);
      end if;
      Vital_Annotate.Sdf_Generic (Sdf_Context.all, Name (1 .. Len), Ok);
      if not Ok then
         Error_S_Sdf;
         Diag_C ("could not annotate generic ");
         Error_E (Name (1 .. Len));
         return False;
      end if;
      return True;
   end Handle_Generic;

   function Parse_Sdf return Boolean
   is
      Tok : Sdf_Token_Type;
      Ok : Boolean;
   begin
      if Get_Token /= Tok_Oparen
        or else Get_Token /= Tok_Identifier
        or else not Is_Ident ("DELAYFILE")
        or else Get_Token /= Tok_Oparen
        or else Get_Token /= Tok_Identifier
      then
         Error_Sdf ("not an SDF file");
         return False;
      end if;

      if Is_Ident ("SDFVERSION") then
         Tok := Get_Token;
         if Tok = Tok_Qstring then
            Sdf_Context.Version := Sdf_Version_Bad;
            if Ident_Length = 3 and then Buf (Ident_Start + 1) = '.' then
               --  Version has the format '"X.Y"' (without simple quote).
               if Buf (Ident_Start) = '2'
                 and then Buf (Ident_Start + 2) = '1'
               then
                  Sdf_Context.Version := Sdf_2_1;
               end if;
            end if;
            Tok := Get_Token;
         end if;

         if not Expect_Cp_Op_Ident (Tok) then
            return False;
         end if;
      end if;

      if not Expect_Qstr_Cp_Op_Ident ("DESIGN") then
         return False;
      end if;

      if not Expect_Qstr_Cp_Op_Ident ("DATE") then
         return False;
      end if;

      if not Expect_Qstr_Cp_Op_Ident ("VENDOR") then
         return False;
      end if;

      if not Expect_Qstr_Cp_Op_Ident ("PROGRAM") then
         return False;
      end if;

      if not Expect_Qstr_Cp_Op_Ident ("VERSION") then
         return False;
      end if;

      if Is_Ident ("DIVIDER") then
         Tok := Get_Token;
         if Tok = Tok_Div or Tok = Tok_Dot then
            Tok := Get_Token;
         end if;
         if not Expect_Cp_Op_Ident (Tok) then
            return False;
         end if;
      end if;

      if Is_Ident ("VOLTAGE") then
         if not Expect_Rexpr_Cp_Op_Ident then
            return False;
         end if;
      end if;

      if not Expect_Qstr_Cp_Op_Ident ("PROCESS") then
         return False;
      end if;

      if Is_Ident ("TEMPERATURE") then
         if not Expect_Rexpr_Cp_Op_Ident then
            return False;
         end if;
      end if;

      if Is_Ident ("TIMESCALE") then
         Tok := Get_Token;
         if Tok = Tok_Rnumber or Tok = Tok_Dnumber then
            if Scan_Exp = 0 and (Scan_Int = 1
                                 or Scan_Int = 10
                                 or Scan_Int = 100)
            then
               Sdf_Context.Timescale := Scan_Int;
            else
               Error_Sdf ("bad timescale value");
               return False;
            end if;
            Tok := Get_Token;
            if Tok /= Tok_Identifier then
               Error_Sdf (Tok_Identifier);
            end if;
            if Is_Ident ("ps") then
               null;
            elsif Is_Ident ("ns") then
               Sdf_Context.Timescale := Sdf_Context.Timescale * 1000;
            elsif Is_Ident ("us") then
               Sdf_Context.Timescale := Sdf_Context.Timescale * 1000_000;
            else
               Error_Sdf ("bad timescale unit");
               return False;
            end if;
            Tok := Get_Token;
         end if;
         if not Expect_Cp_Op_Ident (Tok) then
            return False;
         end if;
      end if;

      Vital_Annotate.Sdf_Header (Sdf_Context.all);

      --  Parse cell+
      loop
         if not Is_Ident ("CELL") then
            Error_Sdf ("CELL expected");
            return False;
         end if;
         --  Parse celltype
         if Get_Token /= Tok_Oparen
           or else Get_Token /= Tok_Identifier
           or else not Is_Ident ("CELLTYPE")
           or else Get_Token /= Tok_Qstring
         then
            Error_Sdf ("CELLTYPE expected");
            return False;
         end if;
         Sdf_Context.Celltype_Len := Ident_Length;
         if Sdf_Context.Celltype_Len > Sdf_Context.Celltype'Length then
            Error_Sdf ("CELLTYPE qstring is too long");
            return False;
         end if;
         for I in Ident_Start .. Ident_End loop
            Sdf_Context.Celltype (I - Ident_Start + 1) := To_Lower (Buf (I));
         end loop;
         Vital_Annotate.Sdf_Celltype (Sdf_Context.all);
         if Get_Token /= Tok_Cparen
           or else Get_Token /= Tok_Oparen
           or else Get_Token /= Tok_Identifier
           or else not Is_Ident ("INSTANCE")
         then
            Error_Sdf ("INSTANCE expected");
            return False;
         end if;
         --  Parse instance+
         loop
            exit when not Is_Ident ("INSTANCE");
            Tok := Get_Token;
            if Tok /= Tok_Cparen then
               loop
                  if Tok /= Tok_Identifier then
                     Error_Sdf ("instance identifier expected");
                     return False;
                  end if;
                  for I in Ident_Start .. Ident_End loop
                     Buf (I) := To_Lower (Buf (I));
                  end loop;
                  Vital_Annotate.Sdf_Instance
                    (Sdf_Context.all, Buf (Ident_Start .. Ident_End), Ok);
                  if not Ok then
                     Error_Sdf ("cannot find instance");
                     return False;
                  end if;
                  Tok := Get_Token;
                  exit when Tok /= Tok_Dot;
                  Tok := Get_Token;
               end loop;
            end if;
            if Tok /= Tok_Cparen
              or else Get_Token /= Tok_Oparen
              or else Get_Token /= Tok_Identifier
            then
               Error_Sdf ("instance or timing_spec expected");
               return False;
            end if;
         end loop;
         Vital_Annotate.Sdf_Instance_End (Sdf_Context.all, Ok);
         if not Ok then
            Error_Sdf ("bad instance or celltype mistmatch");
            return False;
         end if;

         --  Parse timing_spec+
         loop
            if Is_Ident ("DELAY") then
               --  Parse deltype+
               Tok := Get_Token;
               loop
                  if Tok /= Tok_Oparen
                    or else Get_Token /= Tok_Identifier
                  then
                     Error_Sdf ("deltype expected");
                     return False;
                  end if;
                  if Is_Ident ("PATHPULSE")
                    or else Is_Ident ("GLOBALPATHPULSE")
                  then
                     Error_Sdf ("PATHPULSE and GLOBALPATHPULSE not allowed");
                     return False;
                  end if;
                  if Is_Ident ("ABSOLUTE") then
                     null;
                  elsif Is_Ident ("INCREMENT") then
                     null;
                  else
                     Error_Sdf ("ABSOLUTE or INCREMENT expected");
                     return False;
                  end if;
                  --  Parse absvals+ or incvals+
                  Tok := Get_Token;
                  loop
                     if Tok /= Tok_Oparen
                       or else Get_Token /= Tok_Identifier
                     then
                        Error_Sdf ("absvals or incvals expected");
                        return False;
                     end if;
                     if Is_Ident ("IOPATH") then
                        Start_Generic_Name (Delay_Iopath);
                        if not Parse_Port_Spec
                          or else not Parse_Port_Path
                          or else not Parse_Rvalue
                        then
                           return False;
                        end if;
                     elsif Is_Ident ("PORT") then
                        Start_Generic_Name (Delay_Port);
                        if not Parse_Port_Path
                          or else not Parse_Rvalue
                        then
                           return False;
                        end if;
                     elsif Is_Ident ("COND")
                       or else Is_Ident ("INTERCONNECT")
                       or else Is_Ident ("DEVICE")
                     then
                        Error_Sdf
                          ("COND, INTERCONNECT, or DEVICE not handled");
                        return False;
                     elsif Is_Ident ("NETDELAY") then
                        Error_Sdf ("NETDELAY not allowed in VITAL SDF");
                        return False;
                     else
                        Error_Sdf ("absvals or incvals expected");
                        return False;
                     end if;

                     if not Handle_Generic then
                        return False;
                     end if;

                     Tok := Get_Token;
                     exit when Tok = Tok_Cparen;
                  end loop;
                  Tok := Get_Token;
                  exit when Tok = Tok_Cparen;
               end loop;
            elsif Is_Ident ("TIMINGCHECK") then
               --  parse tc_def+
               Tok := Get_Token;
               loop
                  if Tok /= Tok_Oparen
                    or else Get_Token /= Tok_Identifier
                  then
                     Error_Sdf ("tc_def expected");
                     return False;
                  end if;
                  if Is_Ident ("SETUP") then
                     Start_Generic_Name (Timingcheck_Setup);
                  elsif Is_Ident ("HOLD") then
                     Start_Generic_Name (Timingcheck_Hold);
                  elsif Is_Ident ("SETUPHOLD") then
                     Start_Generic_Name (Timingcheck_Setuphold);
                  elsif Is_Ident ("RECOVERY") then
                     Start_Generic_Name (Timingcheck_Recovery);
                  elsif Is_Ident ("REMOVAL") then
                     Start_Generic_Name (Timingcheck_Removal);
                  elsif Is_Ident ("SKEW") then
                     Start_Generic_Name (Timingcheck_Skew);
                  elsif Is_Ident ("WIDTH") then
                     Start_Generic_Name (Timingcheck_Width);
                  elsif Is_Ident ("PERIOD") then
                     Start_Generic_Name (Timingcheck_Period);
                  elsif Is_Ident ("NOCHANGE") then
                     Start_Generic_Name (Timingcheck_Nochange);
                  elsif Is_Ident ("PATHCONSTRAINT")
                    or else Is_Ident ("SUM")
                    or else Is_Ident ("DIFF")
                    or else Is_Ident ("SKEWCONSTRAINT")
                  then
                     Error_Sdf ("non-VITAL tc_def");
                     return False;
                  else
                     Error_Sdf ("bad tc_def");
                     return False;
                  end if;

                  case Sdf_Context.Kind is
                     when Timingcheck_Setup
                       | Timingcheck_Hold
                       | Timingcheck_Recovery
                       | Timingcheck_Removal
                       | Timingcheck_Skew
                       | Timingcheck_Setuphold
                       | Timingcheck_Nochange =>
                        if not Parse_Port_Tchk
                          or else not Parse_Port_Tchk
                          or else not Parse_Simple_Tc_Rvalue
                        then
                           return False;
                        end if;
                     when Timingcheck_Width
                       | Timingcheck_Period =>
                        if not Parse_Port_Tchk
                          or else not Parse_Simple_Tc_Rvalue
                        then
                           return False;
                        end if;
                     when others =>
                        Internal_Error ("sdf_parse");
                  end case;

                  if not Handle_Generic then
                     return False;
                  end if;

                  case Sdf_Context.Kind is
                     when Timingcheck_Setuphold
                       | Timingcheck_Nochange =>
                        if not Parse_Simple_Tc_Rvalue then
                           return False;
                        end if;
                        Error_Sdf ("setuphold and nochange not yet handled");
                        return False;
                     when others =>
                        null;
                  end case;

                  if Get_Token /= Tok_Cparen then
                     Error_Sdf (Tok_Cparen);
                     return False;
                  end if;
                  Tok := Get_Token;
                  exit when Tok = Tok_Cparen;
               end loop;
            end if;
            Tok := Get_Token;
            exit when Tok = Tok_Cparen;
            if Tok /= Tok_Oparen then
               Error_Sdf (Tok_Oparen);
               return False;
            end if;
            if Get_Token /= Tok_Identifier then
               Error_Sdf (Tok_Identifier);
               return False;
            end if;
         end loop;
         Tok := Get_Token;
         exit when Tok = Tok_Cparen;
         if Tok /= Tok_Oparen
           or else Get_Token /= Tok_Identifier
         then
            Error_Sdf (Tok_Identifier);
         end if;
      end loop;
      if Get_Token /= Tok_Eof then
         Error_Sdf ("EOF expected");
         return False;
      end if;
      return True;
   end Parse_Sdf;

   function Parse_Sdf_File (Filename : String) return Boolean
   is
      Res : Boolean;
   begin
      if not Open_Sdf (Filename) then
         return False;
      end if;
      Res := Parse_Sdf;
      Close_Sdf;
      return Res;
   end Parse_Sdf_File;

end Grt.Sdf;
