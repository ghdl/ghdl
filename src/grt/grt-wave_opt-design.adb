--  GHDL Run Time (GRT) - Wave option file package for reading the tree.
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

-- Description: See package specifications

with Grt.Errors; use Grt.Errors;
with Grt.Wave_Opt.File; use Grt.Wave_Opt.File;

package body Grt.Wave_Opt.Design is

   -- Find the element that matches the name given. Starts with the element
   -- given, then go thru all its siblings
   function Find_Cursor (Name : String;
                         First_Sibling : Elem_Acc;
                         Is_Signal : Boolean := False)
                        return Elem_Acc;

   function Get_Top_Cursor (Tree_Index : Tree_Index_Type; Name : Ghdl_C_String)
                           return Elem_Acc
   is
      Root : Elem_Acc;
   begin
      Root := Trees (Tree_Index);
      if State = Write_File and then Root.Next_Child = null then
         Write_Tree_Comment (Tree_Index);
      end if;
      return Get_Cursor (Root, Name);
   end Get_Top_Cursor;

   function Get_Cursor (Parent : Elem_Acc;
                        Name : Ghdl_C_String;
                        Is_Signal : Boolean := False) return Elem_Acc
   is
      Cursor : Elem_Acc;
      Dummy_Bool : Boolean;
      Str_Name : constant String := Name (1 .. strlen (Name));
   begin
      case State is
         when Write_File =>
            Cursor := Parent;
            Update_Tree (Cursor => Cursor,
                         Updated => Dummy_Bool,
                         Elem_Name => Str_Name,
                         Level => Parent.Level + 1);
            if Is_Signal then
               Write_Signal_Path (Cursor);
            end if;
            return Cursor;
         when Display_Tree =>
            return Find_Cursor (Str_Name, Parent.Next_Child, Is_Signal);
         when Display_All =>
            return null;
      end case;
   end Get_Cursor;

   function Find_Cursor (Name : String;
                         First_Sibling : Elem_Acc;
                         Is_Signal : Boolean := False)
                        return Elem_Acc
   is
      Cursor : Elem_Acc;
   begin
      Cursor := First_Sibling;
      loop
         if Cursor = null then
            return null;
         elsif Cursor.Name.all = Name then
            if Is_Signal then
               Cursor.Kind := Signal;
            else
               Cursor.Kind := Pkg_Entity;
            end if;
            return Cursor;
         end if;
         Cursor := Cursor.Next_Sibling;
      end loop;
   end Find_Cursor;

   function Is_Displayed (Cursor : Elem_Acc) return Boolean is
   begin
      if State /= Display_Tree or else Cursor /= null then
         return True;
      end if;
      return False;
   end Is_Displayed;

   -- Read the whole sub tree given and check if every element was found in
   -- design.  Called by Last_Checks
   procedure Check_Sub_Tree_If_All_Found (Previous_Cursor : Elem_Acc);

   procedure Last_Checks is
   begin
      if Wave_Opt.State = Display_Tree then
         for Index in Tree_Index_Type'Range loop
            Check_Sub_Tree_If_All_Found (Trees (Index).Next_Child);
         end loop;
      end if;
   end Last_Checks;

   procedure Check_Sub_Tree_If_All_Found (Previous_Cursor : Elem_Acc)
   is
      Cursor : Elem_Acc;
   begin
      Cursor := Previous_Cursor;
      while Cursor /= null loop
         if Cursor.Kind = Not_Found then
            Print_Context (Cursor, Warning);
            Report_C (Cursor.Name.all);
            Report_E (" : first element of the path not found in design");
         elsif Cursor.Level = Cursor.Path_Context.Max_Level
           and then Cursor.Kind = Pkg_Entity
         then
            Print_Context (Cursor, Warning);
            Report_C (Cursor.Name.all);
            Report_E (" is not a signal");
         else
            Check_Sub_Tree_If_All_Found (Cursor.Next_Child);
         end if;
         Cursor := Cursor.Next_Sibling;
      end loop;

   end Check_Sub_Tree_If_All_Found;

end Grt.Wave_Opt.Design;
