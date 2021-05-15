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

with System; use System;
with Grt.C; use Grt.C;

package body Grt.Table is

   --  Maximum index of table before resizing.
   Max : Table_Index_Type := Table_Index_Type'Pred (Table_Low_Bound);

   --  Current value of Last
   Last_Val : Table_Index_Type;

   function Malloc (Size : size_t) return Table_Ptr;
   pragma Import (C, Malloc);

   procedure Free (T : Table_Ptr);
   pragma Import (C, Free);

   --  Resize and reallocate the table according to LAST_VAL.
   procedure Resize
   is
      function Realloc (T : Table_Ptr; Size : size_t) return Table_Ptr;
      pragma Import (C, Realloc);

      New_Size : size_t;
   begin
      while Max < Last_Val loop
         Max := Max + (Max - Table_Low_Bound + 1);
      end loop;

      --  Do the multiplication using size_t to avoid overflow if the bounds
      --  are a 32bit type on a 64bit machine.
      New_Size := (size_t (Max - Table_Low_Bound + 1)
                     * size_t (Table_Type'Component_Size / Storage_Unit));

      Table := Realloc (Table, New_Size);

      if Table = null then
         raise Storage_Error;
      end if;
   end Resize;

   procedure Append (New_Val : Table_Component_Type) is
   begin
      Increment_Last;
      Table (Last_Val) := New_Val;
   end Append;

   procedure Decrement_Last is
   begin
      Last_Val := Table_Index_Type'Pred (Last_Val);
   end Decrement_Last;

   procedure Free is
   begin
      Free (Table);
      Table := null;
   end Free;

   procedure Increment_Last is
   begin
      Last_Val := Table_Index_Type'Succ (Last_Val);

      if Last_Val > Max then
         Resize;
      end if;
   end Increment_Last;

   function Last return Table_Index_Type is
   begin
      return Last_Val;
   end Last;

   procedure Release is
   begin
      Max := Last_Val;
      Resize;
   end Release;

   procedure Set_Last (New_Val : Table_Index_Type) is
   begin
      if New_Val < Last_Val then
         Last_Val := New_Val;
      else
         Last_Val := New_Val;

         if Last_Val > Max then
            Resize;
         end if;
      end if;
   end Set_Last;

begin
   Last_Val := Table_Index_Type'Pred (Table_Low_Bound);
   Max := Table_Low_Bound + Table_Index_Type (Table_Initial) - 1;

   Table := Malloc (size_t (Table_Initial)
                      * size_t (Table_Type'Component_Size / Storage_Unit));
end Grt.Table;
