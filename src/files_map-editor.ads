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

package Files_Map.Editor is
   procedure Replace_Text (File : Source_File_Entry;
                           Start_Line : Positive;
                           Start_Off  : Natural;
                           End_Line   : Positive;
                           End_Off    : Natural;
                           Text       : File_Buffer);

   procedure Replace_Text_Ptr (File : Source_File_Entry;
                               Start_Line : Positive;
                               Start_Off  : Natural;
                               End_Line   : Positive;
                               End_Off    : Natural;
                               Text_Ptr   : File_Buffer_Ptr;
                               Text_Len   : Source_Ptr);

   --  Set the position of the GAP in FILE.
   procedure Set_Gap (File : Source_File_Entry;
                      First : Source_Ptr;
                      Last : Source_Ptr);

   --  Recompute lines number.
   procedure Compute_Lines (File : Source_File_Entry);

   --  Check lines of FILE are correct.
   procedure Check_Buffer_Lines (File : Source_File_Entry);

   --  Check that content of FILE is STR[1 .. STR_LEN].
   procedure Check_Buffer_Content (File : Source_File_Entry;
                                   Str : File_Buffer_Ptr;
                                   Str_Len : Source_Ptr);
end Files_Map.Editor;
