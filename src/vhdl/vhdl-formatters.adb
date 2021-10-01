--  VHDL code formatter.
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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Types; use Types;
with Files_Map;
with Simple_IO;
with Utils_IO;
with Dyn_Tables;
with Flags;

with Vhdl.Tokens; use Vhdl.Tokens;
with Vhdl.Scanner; use Vhdl.Scanner;
with Vhdl.Prints; use Vhdl.Prints;

package body Vhdl.Formatters is
   --  Check token TOK with the one from the scanner.  Deal with irregular
   --  cases.
   procedure Check_Token (Tok : Token_Type) is
   begin
      if Tok = Current_Token then
         return;
      end if;

      --  There are a couple of exceptions.
      --  Range and Subtype are considered as identifiers when used as
      --  attributes.
      if Tok = Tok_Identifier
        and then (Current_Token = Tok_Range
                    or else Current_Token = Tok_Subtype)
      then
         return;
      end if;

      --  'default clock' are considered as identifiers.
      if (Tok = Tok_Default or else Tok = Tok_Psl_Clock)
        and then Current_Token = Tok_Identifier
      then
         return;
      end if;

      declare
         use Simple_IO;
      begin
         Put_Line_Err ("error: token mismatch. ");
         Put_Err ("  need to print: ");
         Put_Err (Image (Tok));
         Put_Err (", but read ");
         Put_Err (Image (Current_Token));
         Put_Err (" from file.");
         New_Line_Err;
      end;
      raise Internal_Error;
   end Check_Token;

   package Format_Disp_Ctxt is
      type Format_Ctxt is new Disp_Ctxt with private;

      procedure Init (Ctxt : out Format_Ctxt;
                      Sfe : Source_File_Entry;
                      First_Line : Positive := 1;
                      Last_Line : Positive := Positive'Last);
      procedure Free (Ctxt : in out Format_Ctxt);

      procedure Start_Hbox (Ctxt : in out Format_Ctxt);
      procedure Close_Hbox (Ctxt : in out Format_Ctxt);
      procedure Start_Vbox (Ctxt : in out Format_Ctxt);
      procedure Close_Vbox (Ctxt : in out Format_Ctxt);
      procedure Valign (Ctxt : in out Format_Ctxt; Point : Valign_Type);
      procedure Disp_Token (Ctxt : in out Format_Ctxt; Tok : Token_Type);
      procedure Start_Lit (Ctxt : in out Format_Ctxt; Tok : Token_Type);
      procedure Disp_Char (Ctxt : in out Format_Ctxt; C : Character);
      procedure Close_Lit (Ctxt : in out Format_Ctxt);

      package Token_Table is new Dyn_Tables
        (Table_Component_Type => Uns32,
         Table_Index_Type => Natural,
         Table_Low_Bound => 1);

      function Get_Source_File_Entry (Ctxt : Format_Ctxt)
                                     return Source_File_Entry;

      subtype Etoken_Type is Nat32 range 0 .. 2**10 - 1;
      subtype Col_Type is Natural range 0 .. 2**16 - 1;

      --  Entry in Token_Table for token TOK with column COL.
      --  Unfortunately it is not possible to pack records with discriminant
      --  with GNAT.  So it is done manually.
      type Etoken_Record is record
         Flag_Token : Boolean;
         Flag1 : Boolean;
         Flag2 : Boolean;
         Flag3 : Boolean;
         Flag4 : Boolean;
         Flag5 : Boolean;
         Tok : Etoken_Type;
         Col : Col_Type;
      end record;
      pragma Pack (Etoken_Record);
      for Etoken_Record'Size use 32;

      type Evalue_Record is record
         Flag_Token : Boolean;
         Value : Nat32;
      end record;
      pragma Pack (Evalue_Record);
      for Evalue_Record'Size use 32;

      Etok_Last : constant Etoken_Type := Token_Type'Pos (Token_Type'Last);
      Etok_Start_Vbox : constant Etoken_Type := Etok_Last + 1;
      Etok_Close_Vbox : constant Etoken_Type := Etok_Last + 2;
      Etok_Set_Vbox   : constant Etoken_Type := Etok_Last + 3;
      Etok_No_Indent  : constant Etoken_Type := Etok_Last + 4;
      Etok_Valign     : constant Etoken_Type := Etok_Last + 5;

      procedure Append_Eof (Ctxt : in out Format_Ctxt);
      procedure Read_Token (Ctxt : Format_Ctxt;
                            Idx : Natural;
                            Tok : out Etoken_Type;
                            Col : out Natural);
      procedure Write_Token (Ctxt : Format_Ctxt;
                             Idx : Natural;
                             Col : Natural);

      --  Token_Source_Type are followed in the stream by two values:
      --    the length of the token (number of characters)
      --    the position in the sources
      --  With these two values, it is possible to print the tokens.

      function Read_Value (Ctxt : Format_Ctxt; Idx : Natural) return Nat32;

      type Printer_Ctxt is abstract tagged null record;
      procedure Put (Ctxt : in out Printer_Ctxt; C : Character) is abstract;
   private
      type Format_Ctxt is new Disp_Ctxt with record
         First_Line : Natural;
         Last_Line : Natural;
         Lineno : Natural;
         Enable : Boolean;
         Flag_Lit : Boolean;
         Vnum : Natural;
         Hnum : Natural;
         Hfirst : Boolean;
         Sfe : Source_File_Entry;
         Toks : Token_Table.Instance;
      end record;
   end Format_Disp_Ctxt;

   package body Format_Disp_Ctxt is
      function To_Etoken_Record is new Ada.Unchecked_Conversion
        (Uns32, Etoken_Record);
      function To_Uns32 is new Ada.Unchecked_Conversion
        (Etoken_Record, Uns32);
      function To_Evalue_Record is new Ada.Unchecked_Conversion
        (Uns32, Evalue_Record);
      function To_Uns32 is new Ada.Unchecked_Conversion
        (Evalue_Record, Uns32);

      procedure Read_Token (Ctxt : Format_Ctxt;
                            Idx : Natural;
                            Tok : out Etoken_Type;
                            Col : out Natural)
      is
         Etok : Etoken_Record;
      begin
         Etok := To_Etoken_Record (Ctxt.Toks.Table (Idx));
         pragma Assert (Etok.Flag_Token);
         Tok := Etok.Tok;
         Col := Etok.Col;
      end Read_Token;

      procedure Write_Token (Ctxt : Format_Ctxt;
                             Idx : Natural;
                             Col : Natural)
      is
         Etok : Etoken_Record;
      begin
         Etok := To_Etoken_Record (Ctxt.Toks.Table (Idx));
         pragma Assert (Etok.Flag_Token);
         Etok.Col := Col;
         Ctxt.Toks.Table (Idx) := To_Uns32 (Etok);
      end Write_Token;

      function Read_Value (Ctxt : Format_Ctxt; Idx : Natural) return Nat32
      is
         V : Evalue_Record;
      begin
         V := To_Evalue_Record (Ctxt.Toks.Table (Idx));
         pragma Assert (not V.Flag_Token);
         return V.Value;
      end Read_Value;

      procedure Append_Token (Ctxt : in out Format_Ctxt;
                              Tok : Etoken_Type;
                              Col : Natural)
      is
         Etok : Etoken_Record;
      begin
         Etok := (Flag_Token => True,
                  Tok => Tok,
                  Col => Col,
                  others => False);
         Token_Table.Append (Ctxt.Toks, To_Uns32 (Etok));
      end Append_Token;

      procedure Append_Token (Ctxt : in out Format_Ctxt; Tok : Token_Type) is
      begin
         Append_Token (Ctxt, Token_Type'Pos (Tok), Get_Token_Offset + 1);
      end Append_Token;

      procedure Append_Value (Ctxt : in out Format_Ctxt;
                              Val : Nat32)
      is
         V : Evalue_Record;
      begin
         V := (Flag_Token => False,
               Value => Val);
         Token_Table.Append (Ctxt.Toks, To_Uns32 (V));
      end Append_Value;

      procedure Append_Source_Token (Ctxt : in out Format_Ctxt;
                                     Tok : Token_Type) is
      begin
         Append_Token (Ctxt, Token_Type'Pos (Tok), Get_Token_Offset + 1);
         Append_Value (Ctxt, Get_Token_Length);
         Append_Value (Ctxt, Nat32 (Get_Token_Position));
      end Append_Source_Token;

      procedure Append_Eof (Ctxt : in out Format_Ctxt) is
      begin
         Append_Token (Ctxt, Token_Type'Pos (Tok_Eof), 0);
      end Append_Eof;

      procedure Init (Ctxt : out Format_Ctxt;
                      Sfe : Source_File_Entry;
                      First_Line : Positive := 1;
                      Last_Line : Positive := Positive'Last) is
      begin
         Ctxt := (First_Line => First_Line,
                  Last_Line => Last_Line,
                  Lineno => 1,
                  Enable => First_Line = 1,
                  Flag_Lit => False,
                  Vnum => 0,
                  Hnum => 0,
                  Hfirst => True,
                  Sfe => Sfe,
                  Toks => <>);
         Token_Table.Init (Ctxt.Toks, 1024);
         if First_Line = 1 then
            Append_Token (Ctxt, Etok_No_Indent, 0);
         end if;
      end Init;

      procedure Free (Ctxt : in out Format_Ctxt) is
      begin
         Token_Table.Free (Ctxt.Toks);
      end Free;

      function Get_Source_File_Entry (Ctxt : Format_Ctxt)
                                     return Source_File_Entry is
      begin
         return Ctxt.Sfe;
      end Get_Source_File_Entry;

      procedure Skip_Newline (Ctxt : in out Format_Ctxt) is
      begin
         Ctxt.Lineno := Ctxt.Lineno + 1;
         if Ctxt.Enable then
            Append_Token (Ctxt, Token_Type'Pos (Tok_Newline), 0);
            if Ctxt.Last_Line < Ctxt.Lineno then
               Ctxt.Enable := False;
            end if;
         else
            if Ctxt.First_Line = Ctxt.Lineno then
               Ctxt.Enable := True;
               Append_Token (Ctxt, Etok_Set_Vbox, Ctxt.Vnum);
               if Ctxt.Hfirst then
                  Append_Token (Ctxt, Etok_No_Indent, 0);
               end if;
            end if;
         end if;
      end Skip_Newline;

      procedure Skip_Spaces (Ctxt : in out Format_Ctxt) is
      begin
         loop
            case Current_Token is
               when Tok_Eof =>
                  raise Internal_Error;
               when Tok_Newline =>
                  Skip_Newline (Ctxt);
                  Scan;
               when Tok_Line_Comment =>
                  if Ctxt.Enable then
                     Append_Source_Token (Ctxt, Current_Token);
                  end if;
                  Scan;
               when Tok_Block_Comment_Start =>
                  if Ctxt.Enable then
                     Append_Token (Ctxt, Tok_Block_Comment_Start);
                  end if;
                  loop
                     Scan_Block_Comment;
                     case Current_Token is
                        when Tok_Eof =>
                           exit;
                        when Tok_Block_Comment_Text =>
                           if Ctxt.Enable then
                              Append_Source_Token (Ctxt, Current_Token);
                           end if;
                        when Tok_Block_Comment_End =>
                           if Ctxt.Enable then
                              Append_Token (Ctxt, Tok_Block_Comment_End);
                           end if;
                           exit;
                        when Tok_Newline =>
                           Skip_Newline (Ctxt);
                        when others =>
                           raise Internal_Error;
                     end case;
                  end loop;
                  Scan;
               when others =>
                  exit;
            end case;
         end loop;
      end Skip_Spaces;

      procedure Valign (Ctxt : in out Format_Ctxt; Point : Valign_Type) is
      begin
         if Ctxt.Enable then
            Append_Token (Ctxt, Etok_Valign, Valign_Type'Pos (Point));
         end if;
      end Valign;

      procedure Start_Hbox (Ctxt : in out Format_Ctxt) is
      begin
         Ctxt.Hnum := Ctxt.Hnum + 1;
         if Ctxt.Hnum = 1 then
            Ctxt.Hfirst := True;
         end if;
      end Start_Hbox;

      procedure Close_Hbox (Ctxt : in out Format_Ctxt) is
      begin
         if Ctxt.Enable and Ctxt.Hnum = 1 then
            Append_Token (Ctxt, Etok_No_Indent, 0);
         end if;
         Ctxt.Hnum := Ctxt.Hnum - 1;
      end Close_Hbox;

      procedure Start_Vbox (Ctxt : in out Format_Ctxt) is
      begin
         pragma Assert (Ctxt.Hnum = 0);
         Ctxt.Vnum := Ctxt.Vnum + 1;
         if Ctxt.Enable then
            Append_Token (Ctxt, Etok_Start_Vbox, Ctxt.Vnum);
         end if;
      end Start_Vbox;

      procedure Close_Vbox (Ctxt : in out Format_Ctxt) is
      begin
         Skip_Spaces (Ctxt);
         Ctxt.Vnum := Ctxt.Vnum - 1;
         if Ctxt.Enable then
            Append_Token (Ctxt, Etok_Close_Vbox, Ctxt.Vnum);
         end if;
      end Close_Vbox;

      procedure Disp_Token (Ctxt : in out Format_Ctxt; Tok : Token_Type) is
      begin
         Skip_Spaces (Ctxt);
         if Ctxt.Enable then
            Append_Token (Ctxt, Tok);
         end if;
         Ctxt.Hfirst := False;
         Check_Token (Tok);
         Scan;
      end Disp_Token;

      procedure Start_Lit (Ctxt : in out Format_Ctxt; Tok : Token_Type) is
      begin
         pragma Assert (not Ctxt.Flag_Lit);
         Ctxt.Flag_Lit := True;
         Skip_Spaces (Ctxt);

         --  For bit string with length (vhdl08), first store the length.
         if Tok = Tok_Bit_String
           and then Current_Token = Tok_Integer_Letter
         then
            if Ctxt.Enable then
               Append_Source_Token (Ctxt, Tok_Integer_Letter);
            end if;
            Scan;
         end if;

         if Ctxt.Enable then
            Append_Source_Token (Ctxt, Tok);
         end if;
         Ctxt.Hfirst := False;
         Check_Token (Tok);
         Scan;
      end Start_Lit;

      procedure Disp_Char (Ctxt : in out Format_Ctxt; C : Character)
      is
         pragma Unreferenced (C);
      begin
         pragma Assert (Ctxt.Flag_Lit);
         null;
      end Disp_Char;

      procedure Close_Lit (Ctxt : in out Format_Ctxt) is
      begin
         pragma Assert (Ctxt.Flag_Lit);
         Ctxt.Flag_Lit := False;
      end Close_Lit;
   end Format_Disp_Ctxt;

   procedure Reindent (Ctxt : Format_Disp_Ctxt.Format_Ctxt;
                       Respace : Boolean := False)
   is
      use Format_Disp_Ctxt;
      --  Number of spaces for indentation.
      Indentation : constant Natural := 2;
      I : Natural;
      Etok : Etoken_Type;
      Tok : Token_Type;
      Col : Natural;

      --  Previous token.  This is used to decide whether a space must be
      --  inserted between two tokens.
      Prev_Tok : Token_Type;
      Cur_Col : Natural;
      Diff_Col : Integer;
      Indent : Natural;
      Extra_Indent : Boolean;
   begin
      I := Token_Table.First;
      Cur_Col := 1;
      Indent := 1;
      Prev_Tok := Tok_Newline;
      Extra_Indent := True;
      Diff_Col := 0;
      loop
         Read_Token (Ctxt, I, Etok, Col);

         if Etok <= Etok_Last then
            Tok := Token_Type'Val (Etok);
            case Tok is
               when Tok_Eof =>
                  exit;
               when Tok_Newline =>
                  Cur_Col := 1;
               when Token_Source_Type
                 | Tok_Block_Comment_Start
                 | Tok_First_Delimiter .. Token_Type'Last =>
                  if Cur_Col = 1 then
                     --  First token of the line, reindent it.
                     Cur_Col := Indent;
                     if Extra_Indent then
                        Cur_Col := Cur_Col + Indentation;
                     end if;
                     Diff_Col := Cur_Col - Col;
                  else
                     if Respace then
                        --  Just adjust position.
                        if Need_Space (Tok, Prev_Tok) then
                           Cur_Col := Cur_Col + 1;
                        end if;
                     else
                        Cur_Col := Col + Diff_Col;
                     end if;
                  end if;
                  Write_Token (Ctxt, I, Cur_Col);

                  if Tok /= Tok_Line_Comment
                    and then Tok /= Tok_Block_Comment_Start
                  then
                     --  If there is a new line in the current hbox, add an
                     --  extra indentation.
                     Extra_Indent := True;
                  end if;
               when Tok_Block_Comment_Text
                 | Tok_Block_Comment_End =>
                  null;
               when Tok_Invalid =>
                  raise Internal_Error;
            end case;

            case Tok is
               when Tok_Eof
                 | Tok_Invalid =>
                  raise Internal_Error;
               when Tok_Newline =>
                  I := I + 1;
               when Token_Source_Type
                 | Tok_Block_Comment_Text =>
                  if Respace then
                     --  Increment column by the length of the token
                     Cur_Col := Cur_Col + Natural (Read_Value (Ctxt, I + 1));
                  else
                     --  A token is at least one character.
                     Cur_Col := Cur_Col + 1;
                  end if;
                  I := I + 3;
               when Tok_First_Delimiter .. Token_Type'Last
                  | Tok_Block_Comment_Start
                  | Tok_Block_Comment_End =>
                  if Respace then
                     declare
                        S : constant String := Image (Tok);
                     begin
                        Cur_Col := Cur_Col + S'Length;
                     end;
                  else
                     --  A token is at least one character.
                     Cur_Col := Cur_Col + 1;
                  end if;
                  I := I + 1;
            end case;
         else
            case Etok is
               when Etok_Start_Vbox
                 | Etok_Close_Vbox =>
                  Indent := Col * Indentation + 1;
                  Extra_Indent := False;
               when Etok_Set_Vbox =>
                  Indent := Col * Indentation + 1;
               when Etok_No_Indent =>
                  Extra_Indent := False;
               when Etok_Valign =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
            I := I + 1;
         end if;

         Prev_Tok := Tok;
      end loop;
   end Reindent;

   --  Realign some token.
   --  For objects declarations of the same region, the colon (:), the subtype
   --   indication and the default value will be aligned on the same column.
   procedure Realign (Ctxt : in out Format_Disp_Ctxt.Format_Ctxt;
                      Vbox : in out Natural)
   is
      use Format_Disp_Ctxt;

      type Valign_Natural is array (Valign_Type) of Natural;
      type Valign_Boolean is array (Valign_Type) of Boolean;

      --  Maximum offset relative to previous alignment.
      Vpos : Valign_Natural;

      --  True when the realignment was done in the current line.  Used to
      --  discard same alignment marker that appears later.
      Vdone : Valign_Boolean;

      I : Natural;
      Etok : Etoken_Type;
      Tok : Token_Type;
      Col : Natural;
      Skip : Natural;

      Valign : Valign_Type;

      Diff_Col : Integer;
      Cum_Col : Integer;
      Prev_Col : Integer;
   begin
      I := Vbox;

      Vpos := (others => 0);
      Vdone := (others => False);
      Diff_Col := 0;
      Prev_Col := 0;

      --  First pass: compute the positions
      loop
         Read_Token (Ctxt, I, Etok, Col);

         if Etok <= Etok_Last then
            Tok := Token_Type'Val (Etok);
            case Tok is
               when Tok_Eof =>
                  exit;
               when Tok_Invalid =>
                  raise Internal_Error;
               when Tok_Newline =>
                  --  Restart positions.
                  Vdone := (others => False);
                  Prev_Col := 0;
                  I := I + 1;
               when Token_Source_Type
                 | Tok_Block_Comment_Text =>
                  I := I + 3;
               when Tok_First_Delimiter .. Token_Type'Last
                  | Tok_Block_Comment_Start
                  | Tok_Block_Comment_End =>
                  I := I + 1;
            end case;
         else
            case Etok is
               when Etok_Start_Vbox =>
                  --  Nested vbox
                  I := I + 1;
                  Realign (Ctxt, I);
               when Etok_Close_Vbox =>
                  exit;
               when Etok_Set_Vbox =>
                  I := I + 1;
               when Etok_No_Indent =>
                  I := I + 1;
               when Etok_Valign =>
                  --  Ok, the serious work.
                  Valign := Valign_Type'Val (Col);
                  if not Vdone (Valign) then
                     --  The first presence on this line.
                     --  Read position of the next token.
                     Read_Token (Ctxt, I + 1, Etok, Col);
                     pragma Assert (Etok <= Etok_Last);
                     Vdone (Valign) := True;
                     Diff_Col := Col - Prev_Col;
                     if Vpos (Valign) < Diff_Col then
                        Vpos (Valign) := Diff_Col;
                     end if;
                     Prev_Col := Col;
                  end if;
                  I := I + 1;
               when others =>
                  raise Internal_Error;
            end case;
         end if;
      end loop;

      --  Second pass: adjust the offsets
      I := Vbox;
      Vdone := (others => False);
      Diff_Col := 0;
      Skip := 0;
      Cum_Col := 0;

      loop
         Read_Token (Ctxt, I, Etok, Col);

         if Etok <= Etok_Last then
            Tok := Token_Type'Val (Etok);
            case Tok is
               when Tok_Eof =>
                  Vbox := I;
                  exit;
               when Tok_Invalid =>
                  raise Internal_Error;
               when Tok_Newline =>
                  Vdone := (others => False);
                  Diff_Col := 0;
                  Cum_Col := 0;
                  I := I + 1;
               when Token_Source_Type
                 | Tok_Block_Comment_Text =>
                  if Skip = 0 then
                     Write_Token (Ctxt, I, Col + Diff_Col);
                  end if;
                  I := I + 3;
               when Tok_First_Delimiter .. Token_Type'Last
                 | Tok_Block_Comment_Start
                 | Tok_Block_Comment_End =>
                  if Skip = 0 then
                     Write_Token (Ctxt, I, Col + Diff_Col);
                  end if;
                  I := I + 1;
            end case;
         else
            case Etok is
               when Etok_Start_Vbox =>
                  --  Nested vbox
                  Skip := Skip + 1;
               when Etok_Close_Vbox =>
                  if Skip = 0 then
                     Vbox := I + 1;
                     exit;
                  else
                     Skip := Skip - 1;
                  end if;
               when Etok_Set_Vbox =>
                  null;
               when Etok_No_Indent =>
                  null;
               when Etok_Valign =>
                  --  Ok, the serious work.
                  if Skip = 0 then
                     Valign := Valign_Type'Val (Col);
                     if Vpos (Valign) /= 0 and then not Vdone (Valign) then
                        Vdone (Valign) := True;
                        Cum_Col := Cum_Col + Vpos (Valign);
                        Read_Token (Ctxt, I + 1, Etok, Col);
                        Diff_Col := Cum_Col - Col;
                     end if;
                  end if;
               when others =>
                  raise Internal_Error;
            end case;
            I := I + 1;
         end if;
      end loop;
   end Realign;

   procedure Realign (Ctxt : in out Format_Disp_Ctxt.Format_Ctxt)
   is
      I : Natural;
   begin
      I := Format_Disp_Ctxt.Token_Table.First;
      Realign (Ctxt, I);
   end Realign;

   type IO_Printer_Ctxt is new Format_Disp_Ctxt.Printer_Ctxt with null record;
   procedure Put (Ctxt : in out IO_Printer_Ctxt; C : Character)
   is
      pragma Unreferenced (Ctxt);
   begin
      if C = ASCII.LF then
         Simple_IO.New_Line;
      else
         Simple_IO.Put (C);
      end if;
   end Put;

   procedure Reprint (Ctxt : Format_Disp_Ctxt.Format_Ctxt;
                      Prnt : in out Format_Disp_Ctxt.Printer_Ctxt'Class)
   is
      use Format_Disp_Ctxt;
      Sfe : constant Source_File_Entry := Get_Source_File_Entry (Ctxt);
      I : Natural;
      Etok : Etoken_Type;
      Tok : Token_Type;
      Col : Natural;
      Cur_Col : Natural;
   begin
      I := Token_Table.First;
      Cur_Col := 1;
      loop
         Read_Token (Ctxt, I, Etok, Col);
         I := I + 1;

         if Flags.Verbose then
            declare
               use Simple_IO;
               use Utils_IO;
            begin
               Put (' ');
               if Etok <= Etok_Last then
                  Put (Image (Token_Type'Val (Etok)));
               else
                  case Etok is
                     when Etok_Start_Vbox =>
                        Put ("[");
                     when Etok_Close_Vbox =>
                        Put ("]");
                     when Etok_Set_Vbox =>
                        Put ("V");
                     when Etok_No_Indent =>
                        Put ("B");
                     when Etok_Valign =>
                        Put ("A");
                     when others =>
                        raise Internal_Error;
                  end case;
               end if;
               Put (':');
               Put_Int32 (Nat32 (Col));
               Put ('@');
               Put_Int32 (Nat32 (I - 1));
            end;
         end if;

         while Cur_Col < Col loop
            Prnt.Put (' ');
            Cur_Col := Cur_Col + 1;
         end loop;

         if Etok <= Etok_Last then
            Tok := Token_Type'Val (Etok);
            case Tok is
               when Tok_Eof =>
                  exit;
               when Tok_Newline =>
                  Prnt.Put (ASCII.LF);
                  Cur_Col := 1;
               when Token_Source_Type
                 | Tok_Block_Comment_Text =>
                  declare
                     Buf : constant File_Buffer_Acc :=
                       Files_Map.Get_File_Source (Sfe);
                     Len : Nat32;
                     Pos : Source_Ptr;
                  begin
                     Len := Read_Value (Ctxt, I);
                     Pos := Source_Ptr (Read_Value (Ctxt, I + 1));
                     for K in 0 .. Len - 1 loop
                        Prnt.Put (Buf (Pos + Source_Ptr (K)));
                     end loop;
                     Cur_Col := Cur_Col + Natural (Len);
                     I := I + 2;
                  end;
               when Tok_First_Delimiter .. Token_Type'Last
                 | Tok_Block_Comment_Start
                 | Tok_Block_Comment_End =>
                  declare
                     S : constant String := Image (Tok);
                  begin
                     for I in S'Range loop
                        Prnt.Put (S (I));
                     end loop;
                     Cur_Col := Cur_Col + S'Length;
                  end;
               when Tok_Invalid =>
                  null;
            end case;
         end if;
      end loop;
   end Reprint;

   procedure Format_Init (F : Iir_Design_File;
                          First_Line : Positive := 1;
                          Last_Line : Positive := Positive'Last;
                          Ctxt : out Format_Disp_Ctxt.Format_Ctxt)
   is
      use Format_Disp_Ctxt;
      Sfe : constant Source_File_Entry := Get_Design_File_Source (F);
   begin
      Scanner.Flag_Comment := True;
      Scanner.Flag_Newline := True;

      Set_File (Sfe);
      Scan;

      Init (Ctxt, Sfe, First_Line, Last_Line);
      Prints.Disp_Vhdl (Ctxt, F);

      Close_File;
      Scanner.Flag_Comment := False;
      Scanner.Flag_Newline := False;

      Append_Eof (Ctxt);
   end Format_Init;

   procedure Format (F : Iir_Design_File;
                     Level : Format_Level;
                     Flag_Realign : Boolean;
                     First_Line : Positive := 1;
                     Last_Line : Positive := Positive'Last)
   is
      use Format_Disp_Ctxt;
      Ctxt : Format_Ctxt;
      Prnt : IO_Printer_Ctxt;
   begin
      Format_Init (F, First_Line, Last_Line, Ctxt);

      if Level > Format_None then
         Reindent (Ctxt, Level = Format_Space);
      end if;

      if Flag_Realign then
         Realign (Ctxt);
      end if;

      Reprint (Ctxt, Prnt);

      Free (Ctxt);
   end Format;

   procedure Dump_Fmt (Ctxt : Format_Disp_Ctxt.Format_Ctxt)
   is
      Prnt : IO_Printer_Ctxt;
   begin
      Reprint (Ctxt, Prnt);
   end Dump_Fmt;

   pragma Unreferenced (Dump_Fmt);

   function Allocate_Handle return Vstring_Acc is
   begin
      return new Grt.Vstrings.Vstring;
   end Allocate_Handle;

   function Get_Length (Handle : Vstring_Acc) return Natural is
   begin
      return Grt.Vstrings.Length (Handle.all);
   end Get_Length;

   function Get_C_String (Handle : Vstring_Acc)
                         return Grt.Types.Ghdl_C_String is
   begin
      return Grt.Vstrings.Get_C_String (Handle.all);
   end Get_C_String;

   procedure Free_Handle (Handle : Vstring_Acc)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Grt.Vstrings.Vstring, Vstring_Acc);
      Handle1 : Vstring_Acc;
   begin
      Grt.Vstrings.Free (Handle.all);
      Handle1 := Handle;
      Deallocate (Handle1);
   end Free_Handle;

   type Vstring_Printer_Ctxt is new Format_Disp_Ctxt.Printer_Ctxt with record
      Handle : Vstring_Acc;
   end record;

   procedure Put (Ctxt : in out Vstring_Printer_Ctxt; C : Character) is
   begin
      Grt.Vstrings.Append (Ctxt.Handle.all, C);
   end Put;

   procedure Indent_String (F : Iir_Design_File;
                            Handle : Vstring_Acc;
                            First_Line : Positive := 1;
                            Last_Line : Positive := Positive'Last)
   is
      use Format_Disp_Ctxt;
      Ctxt : Format_Ctxt;
      Prnt : Vstring_Printer_Ctxt;
   begin
      Format_Init (F, First_Line, Last_Line, Ctxt);

      Prnt := (Format_Disp_Ctxt.Printer_Ctxt with Handle);
      Reindent (Ctxt, False);
      Realign (Ctxt);
      Reprint (Ctxt, Prnt);

      Free (Ctxt);
   end Indent_String;
end Vhdl.Formatters;
