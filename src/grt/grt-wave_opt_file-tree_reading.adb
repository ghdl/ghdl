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

with Grt.Strings; use Grt.Strings;
with Grt.Errors; use Grt.Errors;

package body Grt.Wave_Opt_File.Tree_Reading is
   -- Returns true is all signals are displayed.  This is the case when no
   -- wave option file was provided or the one provided contains no paths
   function All_Signals_Displayed return Boolean;

   -- Find the element that matches the name given. Starts with the element
   -- given, then go thru all its siblings
   function Find_Cursor (Name : Ghdl_C_String;
                         First_Sibling : Elem_Acc;
                         Is_Signal : Boolean := False)
                        return Elem_Acc;

   function Get_Top_Cursor (Name : Ghdl_C_String; Index : Tree_Index_Type)
                           return Elem_Acc is
   begin
      return Find_Cursor (Name, Trees (Index));
   end Get_Top_Cursor;

   function Get_Cursor
     (Name : Ghdl_C_String; Parent : Elem_Acc; Is_Signal : Boolean := False)
     return Elem_Acc is
   begin
      if All_Signals_Displayed then
         return null;
      end if;
      return Find_Cursor (Name, Parent.Next_Child, Is_Signal);
   end Get_Cursor;

   function Is_Displayed (Cursor : Elem_Acc) return Boolean is
   begin
      if All_Signals_Displayed or else Cursor /= null then
         return True;
      end if;
      return False;
   end Is_Displayed;

   -- Read the whole sub tree given and check if every element was found in
   -- design.  Called by Check_If_All_Found
   procedure Check_Sub_Tree_If_All_Found
     (Previous_Cursor : Elem_Acc; Sep : Character);

   procedure Check_If_All_Found is
   begin
      for Index in Tree_Index_Type'Range loop
         Check_Sub_Tree_If_All_Found (Trees (Index), Seps (Index));
      end loop;
   end Check_If_All_Found;

-------------------------------------------------------------------------------

   function Find_Cursor (Name : Ghdl_C_String;
                         First_Sibling : Elem_Acc;
                         Is_Signal : Boolean := False)
                        return Elem_Acc
   is
      Len : constant Natural := strlen (Name);
      Cursor : Elem_Acc;
   begin
      Cursor := First_Sibling;
      loop
         if Cursor = null then
            return null;
         elsif Cursor.Name.all = Name (1 .. Len) then
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

   procedure Check_Sub_Tree_If_All_Found
     (Previous_Cursor : Elem_Acc; Sep : Character)
   is
      Cursor : Elem_Acc;
   begin
      Cursor := Previous_Cursor;
      while Cursor /= null loop
         if Cursor.Kind = Not_Found then
            Print_Context (Cursor, Warning);
            Report_C ("no VHDL object in design matches ");
            Report_E (Cursor.Name.all);
         elsif Cursor.Level = Cursor.Path_Context.Max_Level
           and then Cursor.Kind = Pkg_Entity
         then
            Error_Context ("not a signal", Cursor, Warning);
         else
            Check_Sub_Tree_If_All_Found (Cursor.Next_Child, Sep);
         end if;
         Cursor := Cursor.Next_Sibling;
      end loop;

   end Check_Sub_Tree_If_All_Found;

   function All_Signals_Displayed return Boolean is
   begin
      return Trees = Tree_Array'(others => null);
   end All_Signals_Displayed;


end Grt.Wave_Opt_File.Tree_Reading;
