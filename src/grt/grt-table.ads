--  GHDL Run Time (GRT) - Resizable array
--  Copyright (C) 2008 - 2014 Tristan Gingold
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

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;
   Table_Initial   : Positive;

package Grt.Table is
   pragma Elaborate_Body;

   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;
   subtype Fat_Table_Type is
     Table_Type (Table_Low_Bound .. Table_Index_Type'Last);

   --  Thin pointer.
   type Table_Ptr is access all Fat_Table_Type;

   --  The table itself.
   Table : aliased Table_Ptr := null;

   --  Get the high bound.
   function Last return Table_Index_Type;
   pragma Inline (Last);

   --  Get the low bound.
   First : constant Table_Index_Type := Table_Low_Bound;

   --  Increase the length by 1.
   procedure Increment_Last;
   pragma Inline (Increment_Last);

   --  Decrease the length by 1.
   procedure Decrement_Last;
   pragma Inline (Decrement_Last);

   --  Set the last bound.
   procedure Set_Last (New_Val : Table_Index_Type);

   --  Release extra memory.
   procedure Release;

   --  Free all the memory used by the table.
   --  The table won't be useable anymore.
   procedure Free;

   --  Append a new element.
   procedure Append (New_Val : Table_Component_Type);
   pragma Inline (Append);
end Grt.Table;
