--  GHDL Run Time (GRT) - Wave option file package for parsing.
--  Copyright (C) 2016 Jonas Baggett
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-- Description: Wave option file child package for parsing.
--              Parse a wave option file provided in the command line and
--              create a tree with the path of the signals to be displayed on
--              the waveform

with Grt.Stdio; use Grt.Stdio;

package Grt.Wave_Opt_File.Parse is
   pragma Preelaborate;

   procedure Start (Option_File : String);

private

   Buf_Size : constant := 1024;

   Line_Context : Line_Context_Acc;

   Tree_Cursor, Previous_Tree_Cursor : Elem_Acc;

   type Version_Type is record
      Major : Integer;
      Minor : Integer;
   end record;
   Version : Version_Type := (others => -1);
   Current_Version : constant Version_Type := (Major => 1, Minor => 0);

   type Sep_Array is array (Tree_Index_Type) of Character;
   Sep : constant Sep_Array := (Pkg => '.', Entity => '/');

   procedure Parse_Version (Line : String_Access);
   procedure Print_Version (Version : Version_Type);

   procedure Parse_Path (Line : String_Access);
   function Update_Tree (Elem_Name : String; Tree_Index : Tree_Index_Type)
                        return Boolean;

   procedure Print_Context (Severity : Severity_Type);
   procedure Error_Context (Msg : String; Severity : Severity_Type := Error);
   function File_Open (Option_File : String) return FILEs;

end Grt.Wave_Opt_File.Parse;
