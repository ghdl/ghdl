--  Verilog preprocessor output
--  Copyright (C) 2023 Tristan Gingold
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

with Types; use Types;
with Name_Table;
with Str_Table;
with Files_Map;
with Errorout; use Errorout;
with Simple_IO; use Simple_IO;

with Verilog.Errors; use Verilog.Errors;
with Verilog.Tokens; use Verilog.Tokens;
with Verilog.Scans; use Verilog.Scans;
with Verilog.Macros; use Verilog.Macros;
with Verilog.Bignums; use Verilog.Bignums;

package body Verilog.Disp_Preproc is
   procedure Put (Id : Name_Id) is
   begin
      Put (Name_Table.Image (Id));
   end Put;

   procedure Disp_Pp_Line
     (Line : Natural; File : Source_File_Entry; Level : Natural) is
   begin
      Put ("`line");
      Put (Natural'Image (Line));
      Put (" """);
      Put (Files_Map.Get_File_Name (File));
      Put ('"');
      Put (Natural'Image (Level));
      New_Line;
   end Disp_Pp_Line;

   procedure Disp_Pp_Arg (Args : Macro_Args_Arr_Acc; Idx : Natural) is
   begin
      if Args /= null and then Idx in Args'Range then
         Put (Args (Idx).Id);
      else
         Put ("`ARG");
      end if;
   end Disp_Pp_Arg;

   procedure Put_Char_String (C : Character) is
   begin
      case C is
         when ASCII.LF =>
            Put ("\n");
         when '\' =>
            Put ("\\");
         when '"' =>
            Put ("\""");
         when others =>
            Put (C);
      end case;
   end Put_Char_String;

   procedure Put_String (Id : String8_Id; Len : Natural) is
   begin
      Put ('"');
      for I in 1 .. Len loop
         Put_Char_String (Str_Table.Char_String8 (Id, Pos32 (I)));
      end loop;
      Put ('"');
   end Put_String;

   procedure Disp_Pp_String (Tok : Token_Element)
   is
      use Files_Map;
      Sfe : Source_File_Entry;
      Pos : Source_Ptr;
      Source : File_Buffer_Acc;
   begin
      Sfe := Location_To_File (Tok.Loc);
      Pos := Location_File_To_Pos (Tok.Loc, Sfe);
      if Tok.Data.Pp_End > Pos then
         Source := Get_File_Source (Sfe);
         for I in Pos .. Tok.Data.Pp_End loop
            Put_Char_String (Source (I));
         end loop;
      end if;
   end Disp_Pp_String;

   procedure Disp_Token_Array (Toks : Token_Array_Acc;
                               Args : Macro_Args_Arr_Acc) is
   begin
      for I in Toks'Range loop
         declare
            Tok : Token_Element renames Toks (I);
         begin
            if True or Tok.Spaces then
               Put (' ');
            end if;
            case Tok.Token is
               when Tok_Identifier
                 | Tok_System =>
                  Put (Tok.Data.Id);
               when Tok_Pp_Macro =>
                  Put ('`');
                  Put (Tok.Data.Macro_Id);
               when Tok_Eol =>
                  Put ('\');
                  New_Line;
                  Put ("  ");
               when Tok_Pp_Arg =>
                  Disp_Pp_Arg (Args, Tok.Data.Arg_Idx);
               when Tok_String_Literal =>
                  Put_String (Tok.Data.Str_Id, Tok.Data.Str_Len);
               when Tok_Pp_String_Arg =>
                  Disp_Pp_String (Tok);
                  Disp_Pp_Arg (Args, Tok.Data.Pp_Arg);
               when Tok_Pp_String_End =>
                  Disp_Pp_String (Tok);
                  Put ("`""");
               when Tok_Pp_String_Start =>
                  Put ("`""");
               when others =>
                  Put (Image (Tok.Token));
            end case;
         end;
      end loop;
   end Disp_Token_Array;

   procedure Disp_Pp_Define (Mac : Macro_Acc)
   is
   begin
      Put (Mac.Id);
      if Mac.Args /= null then
         Put ('(');
         for I in Mac.Args'Range loop
            if I /= Mac.Args'First then
               Put (", ");
            end if;
            Put (Mac.Args (I).Id);
            if Mac.Args (I).Default /= null then
               Put (" = ");
               Disp_Token_Array (Mac.Args (I).Default, null);
            end if;
         end loop;
         Put (')');
      end if;
      if Mac.Toks /= null then
         Put (' ');
         Disp_Token_Array (Mac.Toks, Mac.Args);
      end if;
   end Disp_Pp_Define;

   procedure Disp_Preprocessor
   is
      Has_Nl : Boolean;
      Has_Cr : Boolean;
      Coord : Source_Coord_Type;
      Prev_Token : Token_Type;
      Token : Token_Type;
   begin
      Flag_Scan_All := True;
      Has_Nl := True;
      Has_Cr := True;
      Token := Tok_None;
      loop
         Prev_Token := Token;
         Scan;
         Token := Current_Token;
         case Token is
            when Tok_Eol =>
               --  Avoid consecutive empty lines.
               if not Has_Cr then
                  New_Line;
                  Has_Cr := True;
               elsif not Has_Nl then
                  New_Line;
                  Has_Nl := True;
               end if;
            when Tok_Pp_Include =>
               Coord := Get_Scan_Coord;
               Disp_Pp_Line (1, Coord.File, 1);
            when Tok_Pp_Endinclude =>
               Coord := Get_Scan_Coord;
               Disp_Pp_Line (Coord.Line, Coord.File, 2);
            when Tok_Pp_Define =>
               Put ("`define ");
               Disp_Pp_Define (Get_Current_Macro);
               New_Line;
            when Tok_String_Literal =>
               Put_String (Current_String, Current_String_Len);
            when Tok_Number_32 =>
               case Prev_Token is
                  when Tok_Base_Bin =>
                     for I in reverse 0 .. Current_Number_Len - 1 loop
                        Put (Get_Bin_Digit (Current_Number_Lo, I));
                     end loop;
                  when Tok_Base_Hex =>
                     declare
                        P : Natural;
                     begin
                        P := 0;
                        while P < Current_Number_Len loop
                           Put (Get_Hex_Digit (Current_Number_Lo, P));
                           P := P + 4;
                        end loop;
                     end;
                  when others =>
                     Report_Msg (Msgid_Fatal, Errorout.Scan,
                                 +Get_Token_Location,
                                 "unhandled token %t before number_32",
                                 (1 => +Prev_Token));
               end case;
            when Tok_Eof =>
               exit;
            when Tok_Line_Comment
              | Tok_Block_Comment =>
               --  Print comment ?
               null;
            when others =>
               if Has_Cr then
                  Has_Cr := False;
                  Has_Nl := False;
                  Coord := Get_Scan_Coord;
                  Put ((1 .. Coord.Offset - Get_Token_Width => ' '));
               else
                  Put (' ');
               end if;
               Put (Image_Current_Token);
         end case;
      end loop;
   end Disp_Preprocessor;

   procedure Disp_Tokens is
   begin
      loop
         Scan;
         case Current_Token is
            when Tok_Pp_Include
               | Tok_Pp_Endinclude
               | Tok_Pp_Define
               | Tok_Line_Comment
               | Tok_Block_Comment
               | Tok_Eol =>
               raise Internal_Error;
            when Tok_Eof =>
               exit;
            when others =>
               Put (Image_Current_Token);
               New_Line;
         end case;
      end loop;
   end Disp_Tokens;
end Verilog.Disp_Preproc;
