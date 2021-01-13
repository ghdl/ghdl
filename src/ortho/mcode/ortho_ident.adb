--  Mcode back-end for ortho.
--  Copyright (C) 2006 Tristan Gingold
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
with Ada.Text_IO;
with Tables;

package body Ortho_Ident is
   package Ids is new Tables
     (Table_Component_Type => Natural,
      Table_Index_Type => O_Ident,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   package Strs is new Tables
     (Table_Component_Type => Character,
      Table_Index_Type => Natural,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   function Get_Identifier (Str : String) return O_Ident
   is
      Start : Natural;
   begin
      Start := Strs.Allocate (Str'Length + 1);
      for I in Str'Range loop
         --  Identifiers are NULL terminated, so they cannot have any
         --  embedded NULL.
         pragma Assert (Str (I) /= ASCII.NUL);
         Strs.Table (Start + I - Str'First) := Str (I);
      end loop;
      Strs.Table (Start + Str'Length) := ASCII.NUL;
      Ids.Append (Start);
      return Ids.Last;
   end Get_Identifier;

   function Is_Equal (L, R : O_Ident) return Boolean
   is
   begin
      return L = R;
   end Is_Equal;

   function Get_String_Length (Id : O_Ident) return Natural
   is
      Start : Natural;
   begin
      Start := Ids.Table (Id);
      if Id = Ids.Last then
         return Strs.Last - Start + 1 - 1;
      else
         return Ids.Table (Id + 1) - 1 - Start;
      end if;
   end Get_String_Length;

   function Get_String (Id : O_Ident) return String
   is
      Res : String (1 .. Get_String_Length (Id));
      Start : constant Natural := Ids.Table (Id);
   begin
      for I in Res'Range loop
         Res (I) := Strs.Table (Start + I - Res'First);
      end loop;
      return Res;
   end Get_String;

   function Get_Cstring (Id : O_Ident) return System.Address is
   begin
      return Strs.Table (Ids.Table (Id))'Address;
   end Get_Cstring;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean
   is
      Start : constant Natural := Ids.Table (Id);
      Len : constant Natural := Get_String_Length (Id);
   begin
      if Len /= Str'Length then
         return False;
      end if;
      for I in Str'Range loop
         if Str (I) /= Strs.Table (Start + I - Str'First) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Equal;

   function Is_Nul (Id : O_Ident) return Boolean is
   begin
      return Id = O_Ident_Nul;
   end Is_Nul;

   procedure Disp_Stats
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Number of Ident: " & O_Ident'Image (Ids.Last));
      Put_Line ("Number of Ident-Strs: " & Natural'Image (Strs.Last));
   end Disp_Stats;

   procedure Finish is
   begin
      Ids.Free;
      Strs.Free;
   end Finish;
end Ortho_Ident;
