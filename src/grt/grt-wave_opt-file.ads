--  GHDL Run Time (GRT) - Wave option file package for parsing.
--  Copyright (C) 2016 Jonas Baggett
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-- Description: Wave option file child package for file manipulation and
--              tree creation.
--              When State = Display_Tree, it parse the wave option file
--              provided in the command line and create a tree with the path of
--              the signals to be displayed on the waveform.

with Grt.Stdio; use Grt.Stdio;

package Grt.Wave_Opt.File is
   pragma Preelaborate;

   -- Open the wave option file given as parameter and parses it if it exists,
   -- otherwise creates it and it will be written when reading the design
   -- hierarchy
   procedure Start (Option_File : String; To_Be_Created : Boolean);

   -- Write the path of a signal to the option file
   procedure Write_Signal_Path (Signal : Elem_Acc);

   -- Write a starting comment before the first signal path of the packages
   -- tree or of the entities tree is ever written
   procedure Write_Tree_Comment (Tree_Index : Tree_Index_Type);

   -- Update_Tree : Update the tree with the current VHDL element read from
   -- the current path.
   -- Called when the option file is read or when the option file is created
   -- while reading the design hierarchy.
   procedure Update_Tree (Cursor : in out Elem_Acc;
                          Last_Updated : in out Boolean;
                          Elem_Expr : String;
                          Level : Natural;
                          Lineno : Natural := 0);

   -- Destructor
   procedure Finalize;

private

   Write_Stream : FILEs;

   Buf_Size : constant := 1024;

   type Version_Type is record
      Major : Integer;
      Minor : Integer;
   end record;
   Version : Version_Type := (others => -1);
   Current_Version : constant Version_Type := (Major => 1, Minor => 1);

   type Sep_Array is array (Tree_Index_Type) of Character;

end Grt.Wave_Opt.File;
