--  GHDL Run Time (GRT) - Wave option file package for debugging.
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

-- Description: See package specifications

with Grt.Astdio; use Grt.Astdio;

package body Grt.Wave_Opt.File.Debug is

   -- Dump recursively an element of the tree.
   procedure Dump_Sub_Tree (Cursor : Elem_Acc);

   procedure Dump_Tree is
   begin
      New_Line;
      for Index in Tree_Index_Type'Range loop
         Put_Line ("----------------------------");
         if Index = Pkg then
            Put_Line ("Packages : ");
         else
            Put_Line ("Instances : ");
         end if;
         Dump_Sub_Tree (Trees (Index).Next_Child);
      end loop;
      Put_Line ("----------- END -----------------");
      New_Line;
   end Dump_Tree;

   procedure Dump_Sub_Tree (Cursor : Elem_Acc)
   is
      Sibling_Cursor : Elem_Acc;
   begin
      Sibling_Cursor := Cursor;
      while Sibling_Cursor /= null loop
         Put ((3 .. 2 * Sibling_Cursor.Level => ' '));
         Put ('/');
         Put_Line (Sibling_Cursor.Expr.all);
         Dump_Sub_Tree (Sibling_Cursor.Next_Child);
         Sibling_Cursor := Sibling_Cursor.Next_Sibling;
      end loop;
   end Dump_Sub_Tree;

end Grt.Wave_Opt.File.Debug;
