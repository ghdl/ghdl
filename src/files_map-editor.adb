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
      if Start_Line /= End_Line then
         --  Not handled.
         --  FIXME: also handle newlines in TEXT.
         raise Internal_Error;
      end if;
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
end Files_Map.Editor;
