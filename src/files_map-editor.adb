--  Edition of a files_map buffer.
--  Copyright (C) 2018 Tristan Gingold
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

with Ada.Text_IO;

package body Files_Map.Editor is
   --  Compute the number of character in FILE between [START_POS; END_POS)
   function Get_Range_Length (File : Source_File_Entry;
                              Start_Pos : Source_Ptr;
                              End_Pos : Source_Ptr) return Source_Ptr
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
      Res : Source_Ptr;
   begin
      pragma Assert (End_Pos >= Start_Pos);
      pragma Assert (End_Pos <= F.File_Length);
      Res := End_Pos - Start_Pos;

      --  Returns now if the gap is outside the range.
      if F.Gap_Last < Start_Pos or else F.Gap_Start >= End_Pos then
         return Res;
      end if;

      --  Check the gap is completly within the range.
      if F.Gap_Last > End_Pos or else F.Gap_Start <= Start_Pos then
         raise Internal_Error;
      end if;

      return Res - (F.Gap_Last - F.Gap_Start + 1);
   end Get_Range_Length;

   --  Move the gap to the end of LINE.
   procedure Move_Gap (File : Source_File_Entry;
                       Line : Positive)
   is
      use Lines_Tables;

      F : Source_File_Record renames Source_Files.Table (File);
      New_Start : Source_Ptr;
      Gap_Len : Source_Ptr;
      Diff : Source_Ptr;
   begin
      if Line = Lines_Tables.Last (F.Lines) then
         New_Start := F.File_Length;
         if New_Start = F.Gap_Start then
            --  No move.
            return;
         end if;
      else
         New_Start := Line_To_Position (File, Line + 1);
         if New_Start = F.Gap_Last + 1 then
            --  No move (the gap is already at end of LINE).
            return;
         end if;
      end if;

      Gap_Len := F.Gap_Last - F.Gap_Start + 1;

      if New_Start < F.Gap_Start then
         --  The gap is moved toward the start of the file by DIFF bytes:
         --     |   [A][XXXX]    |
         --  => |   [XXXX][B]    |
         Diff := F.Gap_Start - New_Start;
         --  Move [A] to [B].
         F.Source (F.Gap_Last - Diff + 1 .. F.Gap_Last) :=
           F.Source (New_Start .. New_Start + Diff - 1);
         --  Renumber
         --  Lines starting from line + 1 until location < Gap_Start should
         --  have their location added by gap_len.
         for L in Line + 1 .. Last (F.Lines) loop
            exit when F.Lines.Table (L) >= F.Gap_Start;
            F.Lines.Table (L) := F.Lines.Table (L) + Gap_Len;
         end loop;
      else
         --  The gap is moved toward the end of the file by DIFF bytes.
         --     |   [XXXX][A]    |
         --  => |   [B][XXXX]    |
         Diff := New_Start - F.Gap_Start;
         --  Move [A] to [B].
         F.Source (F.Gap_Start .. F.Gap_Start + Diff - 1) :=
           F.Source (F.Gap_Last + 1 .. F.Gap_Last + 1 + Diff - 1);
         --  Renumber
         --  Lines starting from LINE downto location > Gap_Start should have
         --  their location substracted by gap_len.
         for L in reverse 1 .. Line loop
            exit when F.Lines.Table (L) <= F.Gap_Start;
            F.Lines.Table (L) := F.Lines.Table (L) - Gap_Len;
         end loop;
      end if;

      --  Adjust gap.
      F.Gap_Start := New_Start;
      F.Gap_Last := New_Start + Gap_Len - 1;
   end Move_Gap;

   --  Count the length of newlines at position P in TEXT.
   --  Result can be:
   --    1 for (LF, CR)
   --    2 for (LF+CR or CR+LF),
   --    0 for non-newlines.
   function Is_Newline (Text : String; P : Positive) return Natural is
   begin
      if Text (P) = ASCII.CR then
         if P < Text'Last and then Text (P + 1) = ASCII.LF then
            return 2;
         else
            return 1;
         end if;
      elsif Text (P) = ASCII.LF then
         if P < Text'Last and then Text (P + 1) = ASCII.CR then
            return 2;
         else
            return 1;
         end if;
      else
         return 0;
      end if;
   end Is_Newline;

   --  Count the number of newlines (LF, CR, LF+CR or CR+LF) in TEXT.
   function Count_Newlines (Text : String) return Natural
   is
      P : Positive;
      Res : Natural;
      R : Natural;
   begin
      P := Text'First;
      Res := 0;
      while P <= Text'Last loop
         R := Is_Newline (Text, P);
         if R > 0 then
            P := P + R;
            Res := Res + 1;
         else
            P := P + 1;
         end if;
      end loop;

      return Res;
   end Count_Newlines;

   procedure Replace_Text (File : Source_File_Entry;
                           Start_Line : Positive;
                           Start_Off  : Natural;
                           End_Line   : Positive;
                           End_Off    : Natural;
                           Text : String)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
      Start_Pos : constant Source_Ptr :=
        Line_To_Position (File, Start_Line) + Source_Ptr (Start_Off);
      End_Pos : constant Source_Ptr :=
        Line_To_Position (File, End_Line) + Source_Ptr (End_Off);
      Text_Size : constant Source_Ptr := Text'Length;
      Gap_Size : Source_Ptr;
      Range_Size : Source_Ptr;
   begin
      Gap_Size := F.Gap_Last - F.Gap_Start + 1;
      Range_Size := Get_Range_Length (File, Start_Pos, End_Pos);

      --  Check there is enough space.
      if Text_Size > Gap_Size + Range_Size then
         raise Constraint_Error;
      end if;

      --  Move gap
      Move_Gap (File, End_Line);

      --  Replace text, handle new lines.
      --  Deletion.
      --     End_Pos --|    |---- Gap_Start
      --    |   [ABCDEFGHn][XXX]  |
      -- => |   [ABRGHn][XXXXXX]  |
      --           |-- Start_Pos
      --
      --  Insertion
      --   End_Pos --|   |---- Gap_Start
      --      |  [ABCDn][XXXXX] |
      --  =>  |  [ABRRRDn][XXX] |
      --            |-- Start_Pos
      declare
         Move_Len : constant Source_Ptr := F.Gap_Start - End_Pos;
         New_Pos  : constant Source_Ptr := Start_Pos + Text_Size;
      begin
         F.Source (New_Pos .. New_Pos + Move_Len - 1) :=
           F.Source (End_Pos .. End_Pos + Move_Len - 1);
         --  FIXME: clear gap when extended.
         F.Gap_Start := New_Pos + Move_Len;
         --  Copy.
         F.Source (Start_Pos .. Start_Pos + Text_Size - 1) :=
           File_Buffer (Text);
      end;

      --  Renumber.
      declare
         use Lines_Tables;
         Text_Lines : constant Natural := Count_Newlines (Text);
         Orig_Lines : constant Natural := End_Line - Start_Line;
         Diff : constant Integer := Text_Lines - Orig_Lines;
         Orig_Last : constant Natural := Last (F.Lines);
         P : Positive;
         Nl_Len : Natural;
         L : Natural;
      begin
         --  No change in newlines.
         if Text_Lines = 0 and then Orig_Lines = 0 then
            return;
         end if;

         --  Make room for lines table.
         if Diff /= 0 then
            if Diff > 0 then
               Set_Last (F.Lines, Orig_Last + Diff);
            end if;
            F.Lines.Table (End_Line + Diff .. Orig_Last + Diff) :=
              F.Lines.Table (End_Line .. Orig_Last);
            if Diff < 0 then
               Set_Last (F.Lines, Orig_Last + Diff);
            end if;
         end if;

         --  Renumber.
         P := Text'First;
         L := Start_Line + 1;
         while P <= Text'Last loop
            Nl_Len := Is_Newline (Text, P);
            if Nl_Len = 0 then
               P := P + 1;
            else
               P := P + Nl_Len;
               F.Lines.Table (L) := Start_Pos + Source_Ptr (P - Text'First);
            end if;
         end loop;
      end;
   end Replace_Text;

   procedure Replace_Text_Ptr (File : Source_File_Entry;
                               Start_Line : Positive;
                               Start_Off  : Natural;
                               End_Line   : Positive;
                               End_Off    : Natural;
                               Str : Thin_String_Ptr;
                               Str_Len : Natural) is
   begin
      Replace_Text (File, Start_Line, Start_Off, End_Line, End_Off,
                    Str (1 .. Str_Len));
   end Replace_Text_Ptr;

   procedure Set_Gap (File : Source_File_Entry;
                      First : Source_Ptr;
                      Last : Source_Ptr)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      F.Gap_Start := First;
      F.Gap_Last := Last;
   end Set_Gap;

   procedure Check_Buffer_Content (File : Source_File_Entry;
                                   Str : Thin_String_Ptr;
                                   Str_Len : Natural)
   is
      use Ada.Text_IO;
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      --  Check length.
      declare
         Buf_Len : constant Source_Ptr :=
           F.File_Length - (F.Gap_Last - F.Gap_Start);
      begin
         if Str_Len /= Natural (Buf_Len) then
            Put_Line (Standard_Error,
                      "length mismatch (text:" & Natural'Image (Str_Len)
                        & ", buffer:" & Source_Ptr'Image (Buf_Len));
         end if;
      end;

      --  Check content.
      declare
         T_Pos : Natural;
         S_Pos : Source_Ptr;
      begin
         T_Pos := Str'First;
         S_Pos := Source_Ptr_Org;
         while T_Pos <= Str_Len loop
            if F.Source (S_Pos) /= Str (T_Pos) then
               Put_Line (Standard_Error,
                         "difference at offset" & Natural'Image (T_Pos));
               exit;
            end if;
            T_Pos := T_Pos + 1;
            S_Pos := S_Pos + 1;
            if S_Pos = F.Gap_Start then
               S_Pos := F.Gap_Last + 1;
            end if;
         end loop;
      end;

      --  Check lines.
      declare
         L : Positive;
         P : Source_Ptr;
         Nl : Natural;
         subtype Buf_Subtype is String (1 .. Natural (F.File_Length));
      begin
         L := 1;
         P := Source_Ptr_Org;
         loop
            if F.Lines.Table (L) /= P then
               Put_Line (Standard_Error,
                         "offset mismatch for line" & Natural'Image (L));
            end if;
            L := L + 1;

            loop
               Nl := Is_Newline
                 (Buf_Subtype (F.Source (Source_Ptr_Org
                                           .. F.File_Length - 1)),
                 Positive (1 + P));
               if Nl = 0 then
                  P := P + 1;
               else
                  P := P + Source_Ptr (Nl);
                  exit;
               end if;
            end loop;

            if P = F.Gap_Start then
               P := F.Gap_Last + 1;
            end if;
            exit when P = F.File_Length;
         end loop;
      end;
   end Check_Buffer_Content;

end Files_Map.Editor;
