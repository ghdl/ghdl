--  Edition of a files_map buffer.
--  Copyright (C) 2018 Tristan Gingold
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

with Logging; use Logging;

package body Files_Map.Editor is
   --  Count the length of newlines at position P in TEXT.
   --  Result can be:
   --    1 for (LF, CR)
   --    2 for (LF+CR or CR+LF),
   --    0 for non-newlines.
   function Is_Newline (Text : File_Buffer; P : Source_Ptr) return Natural is
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

   procedure Compute_Lines (File : Source_File_Entry)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
      L : Positive;
      P : Source_Ptr;
      Nl : Natural;
   begin
      Lines_Tables.Init (F.Lines, Lines_Table_Init);

      L := 1;
      P := Source_Ptr_Org;
      Main_Loop: loop
         File_Add_Line_Number (File, L, P);
         exit Main_Loop when P = F.File_Length;

         loop
            Nl := Is_Newline (F.Source.all, P);
            if Nl = 0 then
               P := P + 1;
            else
               P := P + Source_Ptr (Nl);
               exit;
            end if;
            exit Main_Loop when P = F.File_Length;
         end loop;

         Skip_Gap (File, P);

         L := L + 1;
      end loop Main_Loop;
   end Compute_Lines;

   procedure Check_Buffer_Lines (File : Source_File_Entry)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
      L : Positive;
      P : Source_Ptr;
      Nl : Natural;
   begin
      --  Check File_Length.
      P := F.File_Length;
      if P >= Get_Buffer_Length (File) then
         Log_Line ("invalid file length");
      end if;
      if F.Source (P) /= EOT or else F.Source (P + 1) /= EOT then
         Log_Line ("missing EOT at end of buffer");
      end if;

      L := 1;
      P := Source_Ptr_Org;
      Main_Loop: loop
         if F.Lines.Table (L) /= P then
            Log_Line ("offset mismatch for line" & Natural'Image (L));
         end if;

         exit Main_Loop when P = F.File_Length;

         --  Skip until eol.
         loop
            Nl := Is_Newline (F.Source.all, P);
            if Nl = 0 then
               P := P + 1;
            else
               P := P + Source_Ptr (Nl);
               exit;
            end if;
            exit Main_Loop when P = F.File_Length;
         end loop;

         Skip_Gap (File, P);

         L := L + 1;
      end loop Main_Loop;

      if Lines_Tables.Last (F.Lines) /= L then
         Log_Line ("incorrect number of lines");
      end if;

      if F.Lines.Table (F.Cache_Line) /= F.Cache_Pos then
         Log_Line ("incorrect position of cache line");
      end if;
   end Check_Buffer_Lines;

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
         --  Moved to the end of the buffer.
         if F.Gap_Start >= F.File_Length then
            pragma Assert (F.Gap_Start = F.File_Length + 2);
            --  Already there.
            return;
         end if;
         New_Start := F.File_Length + 2;
      else
         New_Start := File_Line_To_Position (File, Line + 1);
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

         if F.Gap_Start >= F.File_Length then
            --  The gap was after the EOT.  As it is moved before, we need
            --  to increase the file length.
            F.File_Length := F.File_Length + Gap_Len;
         end if;

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
         New_Start := New_Start - Gap_Len;
         Diff := New_Start - F.Gap_Start;
         --  Move [A] to [B].
         F.Source (F.Gap_Start .. F.Gap_Start + Diff - 1) :=
           F.Source (F.Gap_Last + 1 .. F.Gap_Last + 1 + Diff - 1);

         if New_Start + Gap_Len > F.File_Length then
            --  Moved past the end of file.  Decrease the file length.
            F.File_Length := F.File_Length - Gap_Len;
         end if;

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

      --  Clear cache.
      F.Cache_Line := 1;
      F.Cache_Pos := Source_Ptr_Org;
   end Move_Gap;

   --  Count the number of newlines (LF, CR, LF+CR or CR+LF) in TEXT.
   function Count_Newlines (Text : File_Buffer) return Natural
   is
      P : Source_Ptr;
      Res : Natural;
      R : Natural;
   begin
      P := Text'First;
      Res := 0;
      while P <= Text'Last loop
         R := Is_Newline (Text, P);
         if R > 0 then
            P := P + Source_Ptr (R);
            Res := Res + 1;
         else
            P := P + 1;
         end if;
      end loop;

      return Res;
   end Count_Newlines;

   function Replace_Text (File : Source_File_Entry;
                          Start_Line : Positive;
                          Start_Off  : Natural;
                          End_Line   : Positive;
                          End_Off    : Natural;
                          Text       : File_Buffer) return Boolean
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      --  Move gap to the end of end_line.  At least a part of End_Line
      --  remains as the end position is exclusive but valid.
      Move_Gap (File, End_Line);

      --  The gap has moved, so every offset may have changed...
      declare
         Start_Pos : constant Source_Ptr :=
           File_Line_To_Position (File, Start_Line) + Source_Ptr (Start_Off);
         End_Pos : constant Source_Ptr :=
           File_Line_To_Position (File, End_Line) + Source_Ptr (End_Off);
         Text_Len : constant Source_Ptr := Text'Length;
         Gap_Size : constant Source_Ptr := F.Gap_Last - F.Gap_Start + 1;
         Range_Size : Source_Ptr;
      begin
         Range_Size := Get_Range_Length (File, Start_Pos, End_Pos);

         --  Check there is enough space.
         if Text_Len > Gap_Size + Range_Size then
            return False;
         end if;

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
            New_Pos  : constant Source_Ptr := Start_Pos + Text_Len;
         begin
            F.Source (New_Pos .. New_Pos + Move_Len - 1) :=
              F.Source (End_Pos .. End_Pos + Move_Len - 1);
            --  Copy.
            F.Source (Start_Pos .. Start_Pos + Text_Len - 1) := Text;

            --  If the gap was after the end of the file, then adjust end of
            --  file.
            if F.Gap_Start > F.File_Length then
               F.File_Length := F.File_Length + Text_Len - Range_Size;
            end if;

            --  FIXME: clear gap when extended.
            F.Gap_Start := New_Pos + Move_Len;
         end;

         --  Renumber.
         declare
            use Lines_Tables;
            Text_Lines : constant Natural := Count_Newlines (Text);
            Orig_Lines : constant Natural := End_Line - Start_Line;
            Diff : constant Integer := Text_Lines - Orig_Lines;
            Orig_Last : constant Natural := Last (F.Lines);
            P : Source_Ptr;
            Nl_Len : Natural;
            L : Natural;
         begin
            --  No change in newlines.
            if Text_Lines = 0 and then Orig_Lines = 0 then
               return True;
            end if;

            --  Make room for lines table.
            if Diff /= 0 then
               if Diff > 0 then
                  Set_Last (F.Lines, Orig_Last + Diff);
               end if;
               F.Lines.Table (End_Line + 1 + Diff .. Orig_Last + Diff) :=
                 F.Lines.Table (End_Line + 1 .. Orig_Last);
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
                  P := P + Source_Ptr (Nl_Len);
                  F.Lines.Table (L) := Start_Pos + (P - Text'First);
                  L := L + 1;
               end if;
            end loop;

            --  Clear cache.
            F.Cache_Line := 1;
            F.Cache_Pos := Source_Ptr_Org;

            Check_Buffer_Lines (File);
         end;
      end;

      return True;
   end Replace_Text;

   function Replace_Text_Ptr (File : Source_File_Entry;
                              Start_Line : Positive;
                              Start_Off  : Natural;
                              End_Line   : Positive;
                              End_Off    : Natural;
                              Text_Ptr   : File_Buffer_Ptr;
                              Text_Len   : Source_Ptr) return Boolean is
   begin
      return Replace_Text (File, Start_Line, Start_Off, End_Line, End_Off,
                           Text_Ptr (0 .. Text_Len - 1));
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

   procedure Fill_Text_Ptr (File : Source_File_Entry;
                            Text_Ptr   : File_Buffer_Ptr;
                            Text_Len   : Source_Ptr)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
      Buf_Len : constant Source_Ptr := Get_Buffer_Length (File);
   begin
      if Text_Len + 2 > Buf_Len then
         --  Buffer is too small!
         raise Constraint_Error;
      end if;

      if Text_Len > 0 then
         F.Source (Source_Ptr_Org .. Source_Ptr_Org + Text_Len - 1) :=
           Text_Ptr (Source_Ptr_Org .. Source_Ptr_Org + Text_Len - 1);
      end if;
      Set_File_Length (File, Text_Len);

      --  Move the gap after the two terminal EOT.
      Set_Gap (File, Text_Len + 2, Buf_Len - 1);

      --  Clear cache.
      F.Cache_Line := 1;
      F.Cache_Pos := Source_Ptr_Org;

      --  Reset line table.
      Lines_Tables.Free (F.Lines);
      Lines_Tables.Init (F.Lines, Lines_Table_Init);
      File_Add_Line_Number (File, 1, Source_Ptr_Org);
   end Fill_Text_Ptr;

   procedure Copy_Source_File (Dest : Source_File_Entry;
                               Src : Source_File_Entry)
   is
      pragma Assert (Src <= Source_Files.Last);
      pragma Assert (Dest <= Source_Files.Last);
      S : Source_File_Record renames Source_Files.Table (Src);
      D : Source_File_Record renames Source_Files.Table (Dest);
      S_Cont_Len : constant Source_Ptr := Get_Content_Length (Src);
      D_Buf_Len : constant Source_Ptr := Get_Buffer_Length (Dest);
   begin
      if S_Cont_Len + 2 > D_Buf_Len then
         --  Buffer is too small!
         raise Constraint_Error;
      end if;

      if S.Gap_Start < S.File_Length then
         pragma Assert (Source_Ptr_Org = 0);
         D.Source (0 .. S.Gap_Start - 1) :=
           S.Source (0 .. S.Gap_Start - 1);
         D.Source (S.Gap_Start .. S_Cont_Len - 1) :=
           S.Source (S.Gap_Last + 1 .. S.File_Length - 1);
      else
         pragma Assert (S.Gap_Start = S_Cont_Len + 2);
         D.Source (Source_Ptr_Org .. Source_Ptr_Org + S_Cont_Len - 1) :=
           S.Source (Source_Ptr_Org .. Source_Ptr_Org + S_Cont_Len - 1);
      end if;

      Set_File_Length (Dest, S_Cont_Len);

      --  Move the gap after the two terminal EOT.
      Set_Gap (Dest, S_Cont_Len + 2, D_Buf_Len - 1);

      --  Clear cache.
      D.Cache_Line := 1;
      D.Cache_Pos := Source_Ptr_Org;

      Compute_Lines (Dest);
   end Copy_Source_File;

   procedure Check_Buffer_Content (File : Source_File_Entry;
                                   Str : File_Buffer_Ptr;
                                   Str_Len : Source_Ptr)
   is
      pragma Assert (File <= Source_Files.Last);
      F : Source_File_Record renames Source_Files.Table (File);
   begin
      --  Check length.
      declare
         Buf_Len : Source_Ptr;
      begin
         Buf_Len := F.File_Length;
         if F.Gap_Start < F.File_Length then
            Buf_Len := F.File_Length - (F.Gap_Last + 1 - F.Gap_Start);
            if F.File_Length + 1 /= F.Source'Last then
               Log_Line ("bad file length");
            end if;
         else
            if F.Gap_Start /= F.File_Length + 2 then
               Log_Line ("bad position of gap at end of file");
            end if;
         end if;
         if Str_Len /= Buf_Len then
            Log_Line ("length mismatch - text:" & Source_Ptr'Image (Str_Len)
                        & ", buffer:" & Source_Ptr'Image (Buf_Len));
         end if;
      end;

      --  Check presence of EOT.
      if F.Source (F.File_Length) /= EOT then
         Log_Line ("missing first EOT");
      end if;
      if F.Source (F.File_Length + 1) /= EOT then
         Log_Line ("missing second EOT");
      end if;

      --  Check content.
      declare
         T_Pos : Source_Ptr;
         S_Pos : Source_Ptr;
      begin
         T_Pos := Source_Ptr_Org;
         S_Pos := Source_Ptr_Org;
         while T_Pos < Str_Len loop
            if F.Source (S_Pos) /= Str (T_Pos) then
               Log_Line ("difference at offset" & Source_Ptr'Image (T_Pos));
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
      Check_Buffer_Lines (File);
   end Check_Buffer_Content;

end Files_Map.Editor;
