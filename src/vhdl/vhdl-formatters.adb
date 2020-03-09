--  VHDL code formatter.
--  Copyright (C) 2019 Tristan Gingold
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

with Ada.Unchecked_Deallocation;
with Types; use Types;
with Files_Map;
with Simple_IO;
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

      procedure Init (Ctxt : out Format_Ctxt; Sfe : Source_File_Entry);
      procedure Start_Hbox (Ctxt : in out Format_Ctxt);
      procedure Close_Hbox (Ctxt : in out Format_Ctxt);
      procedure Start_Vbox (Ctxt : in out Format_Ctxt);
      procedure Close_Vbox (Ctxt : in out Format_Ctxt);
      procedure Disp_Token (Ctxt : in out Format_Ctxt; Tok : Token_Type);
      procedure Start_Lit (Ctxt : in out Format_Ctxt; Tok : Token_Type);
      procedure Disp_Char (Ctxt : in out Format_Ctxt; C : Character);
      procedure Close_Lit (Ctxt : in out Format_Ctxt);
   private
      type Format_Ctxt is new Disp_Ctxt with record
         Vnum : Natural;
         Hnum : Natural;
         Prev_Tok : Token_Type;
         Sfe : Source_File_Entry;
         Source : File_Buffer_Acc;
      end record;

      procedure Disp_Newline (Ctxt : in out Format_Ctxt);
      procedure Disp_Indent (Ctxt : in out Format_Ctxt);
      procedure Put (Ctxt : in out Format_Ctxt; C : Character);
      procedure Sync (Ctxt : in out Format_Ctxt; Tok : Token_Type);
   end Format_Disp_Ctxt;

   package body Format_Disp_Ctxt is
      procedure Init (Ctxt : out Format_Ctxt; Sfe : Source_File_Entry) is
      begin
         Ctxt := (Vnum => 0,
                  Hnum => 0,
                  Prev_Tok => Tok_Newline,
                  Sfe => Sfe,
                  Source => Files_Map.Get_File_Source (Sfe));
      end Init;

      procedure Put (Ctxt : in out Format_Ctxt; C : Character)
      is
         pragma Unreferenced (Ctxt);
      begin
         Simple_IO.Put (C);
      end Put;

      procedure Start_Hbox (Ctxt : in out Format_Ctxt) is
      begin
         Ctxt.Hnum := Ctxt.Hnum + 1;
      end Start_Hbox;

      procedure Disp_Newline (Ctxt : in out Format_Ctxt) is
      begin
         Put (Ctxt, ASCII.LF);
         Ctxt.Prev_Tok := Tok_Newline;
      end Disp_Newline;

      procedure Close_Hbox (Ctxt : in out Format_Ctxt) is
      begin
         Ctxt.Hnum := Ctxt.Hnum - 1;
         if Ctxt.Hnum = 0 then
            Disp_Newline (Ctxt);
         end if;
      end Close_Hbox;

      procedure Start_Vbox (Ctxt : in out Format_Ctxt) is
      begin
         pragma Assert (Ctxt.Hnum = 0);
         Ctxt.Vnum := Ctxt.Vnum + 1;
      end Start_Vbox;

      procedure Close_Vbox (Ctxt : in out Format_Ctxt) is
      begin
         Ctxt.Vnum := Ctxt.Vnum - 1;
      end Close_Vbox;

      procedure Disp_Indent (Ctxt : in out Format_Ctxt) is
      begin
         for I in 1 .. Ctxt.Vnum loop
            Put (Ctxt, ' ');
            Put (Ctxt, ' ');
         end loop;
      end Disp_Indent;

      procedure Disp_Space (Ctxt : in out Format_Ctxt; Tok : Token_Type)
      is
         Prev_Tok : constant Token_Type := Ctxt.Prev_Tok;
      begin
         if Prev_Tok = Tok_Newline
           and then Ctxt.Hnum = 1
         then
            Disp_Indent (Ctxt);
         elsif Need_Space (Tok, Prev_Tok) then
            Put (Ctxt, ' ');
         end if;
         Ctxt.Prev_Tok := Tok;
      end Disp_Space;

      procedure Disp_Token (Ctxt : in out Format_Ctxt; Tok : Token_Type) is
      begin
         Sync (Ctxt, Tok);
         Disp_Space (Ctxt, Tok);
         Disp_Str (Ctxt, Image (Tok));
      end Disp_Token;

      procedure Start_Lit (Ctxt : in out Format_Ctxt; Tok : Token_Type) is
      begin
         Sync (Ctxt, Tok);
         Disp_Space (Ctxt, Tok);
      end Start_Lit;

      procedure Disp_Char (Ctxt : in out Format_Ctxt; C : Character) is
      begin
         Put (Ctxt, C);
      end Disp_Char;

      procedure Close_Lit (Ctxt : in out Format_Ctxt) is
      begin
         null;
      end Close_Lit;

      procedure Sync (Ctxt : in out Format_Ctxt; Tok : Token_Type) is
      begin
         --  The easy case.
         loop
            case Current_Token is
               when Tok_Eof =>
                  raise Internal_Error;
               when Tok_Newline =>
                  --  Ignored
                  Scan;
                  --  But empty lines are kept.
                  while Current_Token = Tok_Newline loop
                     Disp_Newline (Ctxt);
                     Scan;
                  end loop;
               when Tok_Line_Comment
                 | Tok_Block_Comment =>
                  --  Display the comment as it is.
                  declare
                     P : Source_Ptr;
                  begin
                     --  Re-indent the comment unless this is an end-of-line
                     --  comment or a comment at line 0.
                     if Ctxt.Prev_Tok = Tok_Newline then
                        --  Compute the offset.  Not trivial for block
                        --  comment as this is a multi-line token and
                        --  Get_Token_Offset is not valid in that case.
                        declare
                           Off : Natural;
                           Line_Pos : Source_Ptr;
                           Line : Positive;
                        begin
                           if Current_Token = Tok_Block_Comment then
                              Files_Map.File_Pos_To_Coord
                                (Ctxt.Sfe, Get_Token_Position,
                                 Line_Pos, Line, Off);
                           else
                              Off := Get_Token_Offset;
                           end if;
                           if Off /= 0 then
                              Disp_Indent (Ctxt);
                           end if;
                        end;
                     end if;

                     P := Get_Token_Position;
                     for I in 1 .. Get_Token_Length loop
                        Disp_Char (Ctxt, Ctxt.Source (P));
                        P := P + 1;
                     end loop;
                  end;
                  Scan;
                  while Current_Token = Tok_Newline loop
                     Disp_Newline (Ctxt);
                     Scan;
                  end loop;
               when others =>
                  if Current_Token = Tok_Integer_Letter
                    and then Tok = Tok_Bit_String
                  then
                     Scan;
                  end if;
                  Check_Token (Tok);
                  Scan;
                  return;
            end case;
         end loop;
      end Sync;
   end Format_Disp_Ctxt;

   procedure Format (F : Iir_Design_File)
   is
      use Format_Disp_Ctxt;
      Sfe : constant Source_File_Entry := Get_Design_File_Source (F);
      Ctxt : Format_Ctxt;
   begin
      Scanner.Flag_Comment := True;
      Scanner.Flag_Newline := True;

      Set_File (Sfe);
      Scan;

      Init (Ctxt, Sfe);
      Prints.Disp_Vhdl (Ctxt, F);
      Close_File;
      Scanner.Flag_Comment := False;
      Scanner.Flag_Newline := False;
   end Format;

   package Indent_Disp_Ctxt is
      type Indent_Ctxt is new Disp_Ctxt with record
         Vnum : Natural;
         Hnum : Natural;
         Hfirst : Boolean;  --  First token in the hbox.
         Last_Tok : Source_Ptr;
         Col : Natural;
         Line : Positive;
         First_Line : Positive;
         Last_Line : Positive;
         Discard_Output : Boolean;
         Sfe : Source_File_Entry;
         Source : File_Buffer_Acc;
      end record;

      procedure Init (Ctxt : out Indent_Ctxt;
                      Sfe : Source_File_Entry;
                      First_Line : Positive;
                      Last_Line : Positive);
      procedure Start_Hbox (Ctxt : in out Indent_Ctxt);
      procedure Close_Hbox (Ctxt : in out Indent_Ctxt);
      procedure Start_Vbox (Ctxt : in out Indent_Ctxt);
      procedure Close_Vbox (Ctxt : in out Indent_Ctxt);
      procedure Disp_Token (Ctxt : in out Indent_Ctxt; Tok : Token_Type);
      procedure Start_Lit (Ctxt : in out Indent_Ctxt; Tok : Token_Type);
      procedure Disp_Char (Ctxt : in out Indent_Ctxt; C : Character) is null;
      procedure Close_Lit (Ctxt : in out Indent_Ctxt) is null;
      procedure Put (Ctxt : in out Indent_Ctxt; C : Character);
   private
      procedure Sync (Ctxt : in out Indent_Ctxt; Tok : Token_Type);
   end Indent_Disp_Ctxt;

   package body Indent_Disp_Ctxt is
      procedure Init (Ctxt : out Indent_Ctxt;
                      Sfe : Source_File_Entry;
                      First_Line : Positive;
                      Last_Line : Positive) is
      begin
         Ctxt := (Vnum => 0,
                  Hnum => 0,
                  Hfirst => False,
                  Last_Tok => Source_Ptr_Org,
                  Col => 0,
                  Line => 1,
                  First_Line => First_Line,
                  Last_Line => Last_Line,
                  Discard_Output => First_Line > 1,
                  Sfe => Sfe,
                  Source => Files_Map.Get_File_Source (Sfe));

         Scanner.Flag_Comment := True;
         Scanner.Flag_Newline := True;

         Set_File (Sfe);
         Scan;
      end Init;

      procedure Put (Ctxt : in out Indent_Ctxt; C : Character)
      is
         pragma Unreferenced (Ctxt);
      begin
         Simple_IO.Put (C);
      end Put;

      procedure Disp_Spaces (Ctxt : in out Indent_Ctxt)
      is
         use Files_Map;
         C : Character;
         P : Source_Ptr;
         N_Col : Natural;
         Bef_Tok : Source_Ptr;
         Indent : Natural;
      begin
         if Ctxt.Discard_Output then
            return;
         end if;

         if Ctxt.Col = 0 then
            --  Reindent.
            Indent := Ctxt.Vnum;
            if Ctxt.Hnum > 0 and not Ctxt.Hfirst then
               Indent := Indent + 1;
            end if;
            for I in 1 .. 2 * Indent loop
               Put (Indent_Ctxt'Class (Ctxt), ' ');
            end loop;
            Ctxt.Col := 2 * Indent;
         else
            P := Ctxt.Last_Tok;
            Bef_Tok := Get_Token_Position;
            while P < Bef_Tok loop
               C := Ctxt.Source (P);
               if C = ASCII.HT then
                  --  Expand TABS.
                  N_Col := Ctxt.Col + Tab_Stop;
                  N_Col := N_Col - N_Col mod Tab_Stop;
                  while Ctxt.Col < N_Col loop
                     Put (Indent_Ctxt'Class (Ctxt), ' ');
                     Ctxt.Col := Ctxt.Col + 1;
                  end loop;
               else
                  Put (Indent_Ctxt'Class (Ctxt), ' ');
                  Ctxt.Col := Ctxt.Col + 1;
               end if;
               P := P + 1;
            end loop;
         end if;
      end Disp_Spaces;

      --  Disp text for sources for the current token.
      procedure Disp_Text (Ctxt : in out Indent_Ctxt)
      is
         Aft_Tok : constant Source_Ptr := Get_Position;
         P : Source_Ptr;
      begin
         if Ctxt.Discard_Output then
            return;
         end if;

         P := Get_Token_Position;
         while P < Aft_Tok loop
            Put (Indent_Ctxt'Class (Ctxt), Ctxt.Source (P));
            Ctxt.Col := Ctxt.Col + 1;
            P := P + 1;
         end loop;
      end Disp_Text;

      procedure Disp_Comments (Ctxt : in out Indent_Ctxt) is
      begin
         loop
            case Current_Token is
               when Tok_Eof =>
                  raise Internal_Error;
               when Tok_Newline =>
                  if not Ctxt.Discard_Output then
                     Put (Indent_Ctxt'Class (Ctxt), ASCII.LF);
                  end if;
                  Ctxt.Col := 0;
                  Ctxt.Line := Ctxt.Line + 1;
                  Ctxt.Discard_Output :=
                    Ctxt.Line < Ctxt.First_Line
                    or Ctxt.Line > Ctxt.Last_Line;
               when Tok_Line_Comment
                 | Tok_Block_Comment =>
                  Disp_Spaces (Ctxt);
                  Disp_Text (Ctxt);
               when others =>
                  exit;
            end case;
            Ctxt.Last_Tok := Get_Position;
            Scan;
         end loop;
      end Disp_Comments;

      procedure Start_Hbox (Ctxt : in out Indent_Ctxt) is
      begin
         Disp_Comments (Ctxt);
         Ctxt.Hnum := Ctxt.Hnum + 1;
         Ctxt.Hfirst := True;
      end Start_Hbox;

      procedure Close_Hbox (Ctxt : in out Indent_Ctxt) is
      begin
         --  An hbox cannot be empty.
         pragma Assert (Ctxt.Hfirst = False);
         Ctxt.Hnum := Ctxt.Hnum - 1;
      end Close_Hbox;

      procedure Start_Vbox (Ctxt : in out Indent_Ctxt) is
      begin
         pragma Assert (Ctxt.Hnum = 0);
         Ctxt.Vnum := Ctxt.Vnum + 1;
      end Start_Vbox;

      procedure Close_Vbox (Ctxt : in out Indent_Ctxt) is
      begin
         Ctxt.Vnum := Ctxt.Vnum - 1;
      end Close_Vbox;

      procedure Sync (Ctxt : in out Indent_Ctxt; Tok : Token_Type) is
      begin
         Disp_Comments (Ctxt);
         Disp_Spaces (Ctxt);
         Disp_Text (Ctxt);
         if Current_Token = Tok_Integer_Letter
           and then Tok = Tok_Bit_String
         then
            Scan;
            Disp_Text (Ctxt);
         end if;
         Check_Token (Tok);
         Ctxt.Last_Tok := Get_Position;
         Ctxt.Hfirst := False;
         Scan;
      end Sync;

      procedure Disp_Token (Ctxt : in out Indent_Ctxt; Tok : Token_Type) is
      begin
         Sync (Ctxt, Tok);
      end Disp_Token;

      procedure Start_Lit (Ctxt : in out Indent_Ctxt; Tok : Token_Type) is
      begin
         Sync (Ctxt, Tok);
      end Start_Lit;
   end Indent_Disp_Ctxt;

   package Indent_Vstrings_Ctxt is
      use Grt.Vstrings;

      type Vstring_Ctxt is new Indent_Disp_Ctxt.Indent_Ctxt with private;

      procedure Init (Ctxt : out Vstring_Ctxt;
                      Handle : Vstring_Acc;
                      Sfe : Source_File_Entry;
                      First_Line : Positive;
                      Last_Line : Positive);
      procedure Put (Ctxt : in out Vstring_Ctxt; C : Character);
   private
      type Vstring_Ctxt is new Indent_Disp_Ctxt.Indent_Ctxt with record
         Hand : Vstring_Acc;
      end record;
   end Indent_Vstrings_Ctxt;

   package body Indent_Vstrings_Ctxt is
      procedure Init (Ctxt : out Vstring_Ctxt;
                      Handle : Vstring_Acc;
                      Sfe : Source_File_Entry;
                      First_Line : Positive;
                      Last_Line : Positive) is
      begin
         Indent_Disp_Ctxt.Init (Indent_Disp_Ctxt.Indent_Ctxt (Ctxt), Sfe,
                                First_Line, Last_Line);
         Ctxt.Hand := Handle;
      end Init;

      procedure Put (Ctxt : in out Vstring_Ctxt; C : Character) is
      begin
         Append (Ctxt.Hand.all, C);
      end Put;
   end Indent_Vstrings_Ctxt;

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

   procedure Indent_String (F : Iir_Design_File;
                            Handle : Vstring_Acc;
                            First_Line : Positive := 1;
                            Last_Line : Positive := Positive'Last)
   is
      use Indent_Vstrings_Ctxt;
      Sfe : constant Source_File_Entry := Get_Design_File_Source (F);
      Ctxt : Vstring_Ctxt;
   begin
      Init (Ctxt, Handle, Sfe, First_Line, Last_Line);
      Prints.Disp_Vhdl (Ctxt, F);

      Close_File;
      Scanner.Flag_Comment := False;
      Scanner.Flag_Newline := False;
   end Indent_String;

   procedure Indent (F : Iir_Design_File;
                     First_Line : Positive := 1;
                     Last_Line : Positive := Positive'Last) is
   begin
      if False then
         --  Display character per character.  Slow but useful for debugging.
         declare
            use Indent_Disp_Ctxt;
            Sfe : constant Source_File_Entry := Get_Design_File_Source (F);
            Ctxt : Indent_Ctxt;
         begin
            Init (Ctxt, Sfe, First_Line, Last_Line);
            Prints.Disp_Vhdl (Ctxt, F);
         end;
      else
         declare
            use Grt.Types;
            Handle : Vstring_Acc;
            Res : Ghdl_C_String;
            Len : Natural;
         begin
            Handle := Allocate_Handle;
            Indent_String (F, Handle, First_Line, Last_Line);
            Res := Get_C_String (Handle);
            Len := Get_Length (Handle);
            Simple_IO.Put (Res (1 .. Len));
            Free_Handle (Handle);
         end;
      end if;
   end Indent;
end Vhdl.Formatters;
