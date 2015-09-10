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

with Interfaces.C; use Interfaces.C;
with System;

package body Tables is
   --  Number of allocated elements in the table.
   Length : Natural := 0;

   --  Number of used elements in the table.
   Last_Pos : Natural := 0;

   --  Size of an element in storage units (bytes).
   El_Size : constant size_t :=
     size_t (Table_Type'Component_Size / System.Storage_Unit);

   --  Expand the table by doubling its size.  The table must have been
   --  initialized.
   procedure Expand (Num : Natural)
   is
      --  For efficiency, directly call realloc.
      function Crealloc (Ptr : Table_Thin_Ptr; Size : size_t)
                        return Table_Thin_Ptr;
      pragma Import (C, Crealloc, "realloc");
   begin
      pragma Assert (Length /= 0);
      pragma Assert (Table /= null);

      --  Expand the bound.
      Last_Pos := Last_Pos + Num;

      --  Check if need to reallocate.
      if Last_Pos < Length then
         return;
      else
         --  Double the length.
         loop
            Length := Length * 2;
            exit when Length > Last_Pos;
         end loop;
      end if;

      --  Realloc and check result.
      Table := Crealloc (Table, size_t (Length) * El_Size);
      if Table = null then
         raise Storage_Error;
      end if;
   end Expand;

   function Allocate (Num : Natural := 1) return Table_Index_Type
   is
      Res : constant Table_Index_Type := Table_Index_Type'Val
        (Table_Index_Type'Pos (Table_Low_Bound) + Last_Pos);
   begin
      Expand (Num);

      return Res;
   end Allocate;

   procedure Increment_Last is
   begin
      --  Increase by 1.
      Expand (1);
   end Increment_Last;

   procedure Decrement_Last is
   begin
      Last_Pos := Last_Pos - 1;
   end Decrement_Last;

   procedure Set_Last (Index : Table_Index_Type)
   is
      New_Last : constant Natural :=
        (Table_Index_Type'Pos (Index)
           - Table_Index_Type'Pos (Table_Low_Bound) + 1);
   begin
      if New_Last < Last_Pos then
         --  Decrease length.
         Last_Pos := New_Last;
      else
         --  Increase length.
         Expand (New_Last - Last_Pos);
      end if;
   end Set_Last;

   procedure Init
   is
      --  Direct interface to malloc.
      function Cmalloc (Size : size_t) return Table_Thin_Ptr;
      pragma Import (C, Cmalloc, "malloc");
   begin
      if Table = null then
         --  Allocate memory if not already allocated.
         Length := Table_Initial;
         Table := Cmalloc (size_t (Length) * El_Size);
      end if;

      --  Table is initially empty.
      Last_Pos := 0;
   end Init;

   function Last return Table_Index_Type is
   begin
      return Table_Index_Type'Val
        (Table_Index_Type'Pos (Table_Low_Bound) + Last_Pos - 1);
   end Last;

   procedure Free is
      --  Direct interface to free.
      procedure Cfree (Ptr : Table_Thin_Ptr);
      pragma Import (C, Cfree, "free");
   begin
      Cfree (Table);
      Table := null;
      Length := 0;
      Last_Pos := 0;
   end Free;

   procedure Append (Val : Table_Component_Type) is
   begin
      Increment_Last;
      Table (Last) := Val;
   end Append;

begin
   Init;
end Tables;
