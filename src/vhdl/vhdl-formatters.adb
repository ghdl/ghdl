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

with Types; use Types;
with Files_Map;
with Simple_IO;
with Vhdl.Tokens; use Vhdl.Tokens;
with Vhdl.Scanner; use Vhdl.Scanner;
with Vhdl.Prints; use Vhdl.Prints;

package body Vhdl.Formatters is
   package Format_Disp_Ctxt is
      type Format_Ctxt is new Disp_Ctxt with record
         Vnum : Natural;
         Hnum : Natural;
         Prev_Tok : Token_Type;
         Sfe : Source_File_Entry;
         Source : File_Buffer_Acc;
      end record;

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

                  --  There are a couple of exceptions due to attributes or
                  --  PSL.
                  if Tok = Tok_Identifier
                    and then (Current_Token = Tok_Range
                                or else Current_Token = Tok_Subtype)
                  then
                     null;
                  elsif (Tok = Tok_Psl_Default
                           or else Tok = Tok_Psl_Clock)
                    and then Current_Token = Tok_Identifier
                  then
                     null;
                  elsif Tok /= Current_Token then
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
                  end if;
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
   end Format;
end Vhdl.Formatters;
