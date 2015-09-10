--  Efficient expandable one dimensional array.
--  Copyright (C) 2015 Tristan Gingold
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

--  This package mimics GNAT.Table, but:
--  - the index type can be any discrete type (in particular a modular type)
--  - the increment is not used
--  - the interface is simplified.
generic
   --  This package creates:
   --    array (Table_Index_Type range Table_Low_Bound .. <>)
   --      of Table_Component_Type;
   type Table_Component_Type is private;
   type Table_Index_Type is (<>);

   --  The lowest bound of the array.  Note that Table_Low_Bound shouldn't be
   --  Table_Index_Type'First, as otherwise Last may raise constraint error
   --  when the table is empty.
   Table_Low_Bound : Table_Index_Type;

   --  Initial number of elements.
   Table_Initial   : Positive;
package Tables is
   --  Ada type for the array.
   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;
   --  Fat subtype (so that the access is thin).
   subtype Big_Table_Type is
     Table_Type (Table_Low_Bound .. Table_Index_Type'Last);

   --  Access type for the vector.  This is a thin pointer so that it is
   --  compatible with C pointer, as this package uses malloc/realloc/free for
   --  memory management.
   type Table_Thin_Ptr is access all Big_Table_Type;
   pragma Convention (C, Table_Thin_Ptr);
   for Table_Thin_Ptr'Storage_Size use 0;

   --  Pointer to the table.  Note that the use of a thin pointer to the
   --  largest array, this implementation bypasses Ada index checks.
   Table : Table_Thin_Ptr := null;

   --  Initialize the table.  This is done automatically at elaboration.
   procedure Init;

   --  Logical bounds of the array.
   First : constant Table_Index_Type := Table_Low_Bound;
   function Last return Table_Index_Type;
   pragma Inline (Last);

   --  Deallocate all the memory.  Makes the array unusable until the next
   --  call to Init.
   procedure Free;

   --  Increase by 1 the length of the array.  This may allocate memory.
   procedure Increment_Last;
   pragma Inline (Increment_Last);

   --  Decrease by 1 the length of the array.
   procedure Decrement_Last;
   pragma Inline (Decrement_Last);

   --  Increase or decrease the length of the array by specifying the upper
   --  bound.
   procedure Set_Last (Index : Table_Index_Type);

   --  Append VAL to the array.  This always increase the length of the array.
   procedure Append (Val : Table_Component_Type);
   pragma Inline (Append);

   --  Increase by NUM the length of the array, and returns the old value
   --  of Last + 1.
   function Allocate (Num : Natural := 1) return Table_Index_Type;
end Tables;
