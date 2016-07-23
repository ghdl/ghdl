--  GHDL Run Time (GRT) -  command line options.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

with Grt.Strings; use Grt.Strings;
with Grt.Errors; use Grt.Errors;

package body Grt.Wave_Options.Read is

   function Get_Top_Cursor (Name : Ghdl_C_String; Index : Tree_Index_Type)
            return Elem_Acc is
   begin
      return Find_Cursor (Name, Trees (Index));
   end Get_Top_Cursor;

   function Get_Cursor (Name : Ghdl_C_String; Parent : Elem_Acc;
            Is_Signal : Boolean := False) return Elem_Acc is
   begin
      if Display_All_Signals then
         return null;
      end if;
      return Find_Cursor (Name, Parent.Next_Child, Is_Signal);
   end Get_Cursor;

   function Is_Displayed (Cursor : Elem_Acc) return Boolean is
   begin
      if Display_All_Signals or else Cursor /= null then
         return True;
      end if;
      return False;
   end Is_Displayed;

   procedure Check_If_All_Found is
   begin
      for Index in Tree_Index_Type'Range loop
         Check_If_Found (Trees (Index), Seps (Index), 1);
      end loop;
   end Check_If_All_Found;

-- private --------------------------------------------------------------------

   function Find_Cursor (Name : Ghdl_C_String; First : Elem_Acc;
            Is_Signal : Boolean := False) return Elem_Acc is
      Len : constant Natural := strlen (Name);
      Cursor : Elem_Acc := First;
   begin
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

   procedure Check_If_Found (Previous_Cursor : Elem_Acc;
                             Sep : Character; Level : Positive) is
      Cursor : Elem_Acc := Previous_Cursor;
      Index : Positive;
   begin
      while Cursor /= null loop
         if Cursor.Kind = Not_Found then
            Print_Context (Cursor.Line_Context, Warning);
            Report_C ("no VHDL object in design matches ");
            -- Display the path of the first unfound vhdl object in signal path
            if Level > 1 then
               Index := 1;
               for I in 2 .. Level loop
                  Index := Find (Cursor.Line_Context.Str.all, Sep, Index + 1);
               end loop;
               Report_C (Cursor.Line_Context.Str (Cursor.Line_Context.Str'First
                                                  .. Index));
            elsif Sep = '/' then
               Report_C ("/");
            end if;
            Report_E (Cursor.Name.all);
         elsif Level = Cursor.Line_Context.Max_Level
           and then Cursor.Kind = Pkg_Entity
         then
            Error_Context ("not a signal", Cursor.Line_Context, Warning);
         else
            Check_If_Found (Cursor.Next_Child, Sep, Level + 1);
         end if;
         Cursor := Cursor.Next_Sibling;
      end loop;

   end Check_If_Found;

   function Display_All_Signals return Boolean is
   begin
      if Trees = Tree_Array'(others => null) then
         return True;
      end if;
      return False;
   end Display_All_Signals;


end Grt.Wave_Options.Read;
