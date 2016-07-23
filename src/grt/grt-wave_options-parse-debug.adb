--  GHDL Run Time (GRT) - mono-thread version.
--  Copyright (C) 2005 - 2014 Tristan Gingold
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

with Grt.Astdio; use Grt.Astdio;

package body Grt.Wave_Options.Parse.Debug is

   procedure Dump_Tree is
   begin
      for Index in Tree_Index_Type'Range loop
         New_Line;
         New_Line;
         Put ("----------------------------");
         New_Line;
         if Index = Pkg then
            Put ("Packages : ");
         else
            Put ("Instances : ");
         end if;
         New_Line;
         Dump_Sub_Tree (Trees (Index), 1);
      end loop;
      Put ("----------- END -----------------");
      New_Line;
   end Dump_Tree;

   procedure Dump_Sub_Tree (Previous_Cursor : Elem_Acc; Level : Positive)
   is
      Current_Cursor : Elem_Acc := Previous_Cursor;
   begin
      while Current_Cursor /= null loop
         Put ((3 .. 2 * Level => ' '));
         Put ('/'); Put (Current_Cursor.Name.all); New_Line;
         Dump_Sub_Tree (Current_Cursor.Next_Child, Level + 1);
         Current_Cursor := Current_Cursor.Next_Sibling;
      end loop;
   end Dump_Sub_Tree;

end Grt.Wave_Options.Parse.Debug;
