--  Efficient expandable one dimensional array type.
--  Copyright (C) 2015 - 2016 Tristan Gingold
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
package Dyn_Tables is
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

   --  Non user visible data.
   type Instance_Private is private;

   --  Type for the dynamic table.
   type Instance is record
      --  Pointer to the table.  Note that the use of a thin pointer to the
      --  largest array, this implementation bypasses Ada index checks.
      Table : Table_Thin_Ptr := null;

      --  Private data.
      Priv : Instance_Private;
   end record;

   --  Initialize the table.  This must be done by users.
   procedure Init (T : in out Instance; Table_Initial : Positive);

   --  Logical bounds of the array.
   First : constant Table_Index_Type := Table_Low_Bound;
   function Last (T : Instance) return Table_Index_Type;
   pragma Inline (Last);

   --  Return the index of the next bound after last.
   function Next (T : Instance) return Table_Index_Type;

   --  Deallocate all the memory.  Makes the array unusable until the next
   --  call to Init.
   procedure Free (T : in out Instance);

   --  Increase by 1 the length of the array.  This may allocate memory.
   procedure Increment_Last (T : in out Instance);
   pragma Inline (Increment_Last);

   --  Decrease by 1 the length of the array.
   procedure Decrement_Last (T : in out Instance);
   pragma Inline (Decrement_Last);

   --  Increase or decrease the length of the array by specifying the upper
   --  bound.
   procedure Set_Last (T : in out Instance; Index : Table_Index_Type);

   --  Append VAL to the array.  This always increase the length of the array.
   procedure Append (T : in out Instance; Val : Table_Component_Type);
   pragma Inline (Append);

   --  Increase by NUM the length of the array.
   procedure Allocate (T : in out Instance; Num : Natural := 1);

   --  Reserve memory for NUM extra entries.
   procedure Reserve (T : in out Instance; Num : Natural);
private
   type Unsigned is mod 2**32;

   type Instance_Private is record
      --  Number of allocated elements in the table.
      Length : Unsigned := 0;

      --  Number of used elements in the table.
      Last_Pos : Unsigned := 0;
   end record;
end Dyn_Tables;
