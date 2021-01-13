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

package Files_Map.Editor is
   --  Replace [START; END) by TEXT.  Return True in case of success, False
   --  in case of failure (the gap is too small).
   function Replace_Text (File : Source_File_Entry;
                          Start_Line : Positive;
                          Start_Off  : Natural;
                          End_Line   : Positive;
                          End_Off    : Natural;
                          Text       : File_Buffer) return Boolean;

   --  Likewise, but with pointer + length string.
   function Replace_Text_Ptr (File : Source_File_Entry;
                              Start_Line : Positive;
                              Start_Off  : Natural;
                              End_Line   : Positive;
                              End_Off    : Natural;
                              Text_Ptr   : File_Buffer_Ptr;
                              Text_Len   : Source_Ptr) return Boolean;

   --  Replace the content of FILE with TEXT.
   procedure Fill_Text_Ptr (File : Source_File_Entry;
                            Text_Ptr   : File_Buffer_Ptr;
                            Text_Len   : Source_Ptr);

   --  Copy content of SRC to DEST.  The size of DEST must be large enough.
   --  Clear lines table of DEST.
   procedure Copy_Source_File (Dest : Source_File_Entry;
                               Src : Source_File_Entry);

   --  Recompute lines number.
   procedure Compute_Lines (File : Source_File_Entry);

   --  Check lines of FILE are correct.
   procedure Check_Buffer_Lines (File : Source_File_Entry);

   --  Check that content of FILE is STR[1 .. STR_LEN].
   procedure Check_Buffer_Content (File : Source_File_Entry;
                                   Str : File_Buffer_Ptr;
                                   Str_Len : Source_Ptr);
end Files_Map.Editor;
