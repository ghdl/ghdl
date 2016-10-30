--  GHDL Run Time (GRT) - Wave option file top package.
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

-- Description: Wave option file top package.
--              Allows to select signals to be displayed on the waveform (with
--              the help of it's child units)
--              Contains common stuff for it's child units

package Grt.Wave_Opt is
   pragma Preelaborate;

   -- State :
   -- Display_All  : No signal filtering, display all
   -- Write_File   : Write in a new wave option file the signals found in the
   --                design. No signal filtering too.
   -- Display_Tree : Parse the given option file and create the tree. Display
   --                only the signals that are in the tree
   type State_Type is (Display_All, Write_File, Display_Tree);
   State : State_Type := Display_All;

   type String_Cst is access constant String;
   Value_String_Size : constant := 10;

   File_Path : String_Cst;

   type Path_Context_Type is record
      Lineno : Natural;
      Max_Level : Natural;
   end record;
   type Path_Context_Acc is access Path_Context_Type;

   type Elem_Kind_Type is (Not_Found, Pkg_Entity, Signal);
   type Elem_Type;
   type Elem_Acc is access Elem_Type;
   type Elem_Type is record
      Name : String_Cst;
      Path_Context : Path_Context_Acc := null;
      Column : Natural := 0;
      Level : Natural;
      Kind : Elem_Kind_Type := Not_Found;
      Parent : Elem_Acc := null;
      Next_Sibling, Next_Child : Elem_Acc := null;
   end record;

   type Tree_Index_Type is (Pkg, Entity);
   type Tree_Array is array (Tree_Index_Type) of Elem_Acc;
   Trees : Tree_Array := (others => null);
   type Sep_Array is array (Tree_Index_Type) of Character;
   Seps : constant Sep_Array := (Pkg => '.', Entity => '/');

   type Severity_Type is (Error, Warning);

private

   -- An error/warning message start with the context or the error/warning.
   -- This procedure print this context
   procedure Print_Context
     (Lineno, Column : Positive; Severity : Severity_Type);
   procedure Print_Context (Element : Elem_Acc; Severity : Severity_Type);

   -- Print an error/warning with it's context
   procedure Error_Context (Msg : String;
                            Lineno, Column : Positive;
                            Severity : Severity_Type := Error);
   procedure Error_Context
     (Msg : String; Element : Elem_Acc; Severity : Severity_Type := Error);

end Grt.Wave_Opt;
