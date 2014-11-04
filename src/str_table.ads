--  String table.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Types; use Types;

package Str_Table is
   --  Create a new entry in the string table and returns a number to it.
   function Start return String_Id;
   pragma Inline (Start);

   --  Add a new character in the current entry.
   procedure Append (C : Character);
   pragma Inline (Append);

   --  Finish the current entry.
   procedure Finish;
   pragma Inline (Finish);

   --  Get a fat access to the string ID.
   function Get_String_Fat_Acc (Id : String_Id) return String_Fat_Acc;
   pragma Inline (Get_String_Fat_Acc);

   --  Get ID as a string.
   --  This function is slow, to be used only for debugging.
   function Image (Id : String_Id) return String;

   --  Free all the memory and reinitialize the package.
   procedure Initialize;
end Str_Table;

