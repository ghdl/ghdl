--  GHDL Run Time (GRT) - Wave option file package for reading the tree.
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

-- Description: Wave option file child package doing the link between the
--              design and the other wave option packages.
--              Provides functions to find in the tree which signals are to be
--              displayed or not.
--              When State = Display_Tree, it reads the tree created after
--              parsing the wave option file and filters signals accordingly.
--              When State = Write_File, it calls File.Update_Tree to create
--              the tree from the design tree and write the signal paths of all
--              the design to a new wave option file.

with Grt.Types; use Grt.Types;

package Grt.Wave_Opt.Design is
   pragma Preelaborate;

   type Match_Elem_Type;
   type Match_List is access Match_Elem_Type;
   type Match_Elem_Type is record
      Tree_Elem : Elem_Acc;
      Next : Match_List;
   end record;

   -- Returns the top element of the tree corresponding to the index given, but
   -- only if the name given matches with it.  Otherwise returns null
   function Get_Top_Cursor (Tree_Index : Tree_Index_Type; Name : Ghdl_C_String)
                           return Match_List;

   -- If there is an element in the parent element given that matches the name
   -- given, returns it, otherwise returns null
   function Get_Cursor
     (Parent : Match_List; Name : Ghdl_C_String; Is_Signal : Boolean := False)
     return Match_List;

   -- Returns true if the element given is not null, which means it exists in
   -- the tree of the VHDL elements to be displayed
   function Is_Displayed (Cursor : Match_List) return Boolean;

   -- If relevent, read the whole tree and check if every element was found in
   -- design
   procedure Last_Checks;

end Grt.Wave_Opt.Design;
