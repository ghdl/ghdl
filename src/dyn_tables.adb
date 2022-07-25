--  Efficient expandable one dimensional array.
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

with Interfaces.C; use Interfaces.C;
with System;

package body Dyn_Tables is
   --  Size of an element in storage units (bytes).
   El_Size : constant size_t :=
     size_t (Table_Type'Component_Size / System.Storage_Unit);

   --  Expand the table by doubling its size.  The table must have been
   --  initialized.  Memory space for the extra elements are allocated but the
   --  length of the table is not increased.
   procedure Reserve (T : in out Instance; Num : Unsigned)
   is
      --  For efficiency, directly call realloc.
      function Crealloc (Ptr : Table_Thin_Ptr; Size : size_t)
                        return Table_Thin_Ptr;
      pragma Import (C, Crealloc, "realloc");

      New_Len : Unsigned;
      New_Last : Unsigned;
   begin
      pragma Assert (T.Priv.Length /= 0);
      pragma Assert (T.Table /= null);

      --  Expand the bound.
      New_Last := T.Priv.Last_Pos + Num;
      if New_Last < T.Priv.Last_Pos then
         raise Constraint_Error;
      end if;

      --  Check if need to reallocate.
      if New_Last < T.Priv.Length then
         return;
      end if;

      --  Double the length.
      loop
         New_Len := T.Priv.Length * 2;

         --  Check overflow.
         if New_Len < T.Priv.Length then
            raise Constraint_Error;
         end if;

         T.Priv.Length := New_Len;
         exit when New_Len > New_Last;
      end loop;

      --  Realloc and check result.
      if size_t (T.Priv.Length) > size_t'Last / El_Size then
         raise Constraint_Error;
      end if;
      T.Table := Crealloc (T.Table, size_t (T.Priv.Length) * El_Size);
      if T.Table = null then
         raise Storage_Error;
      end if;
   end Reserve;

   --  Expand the table (allocate and increase the length).
   procedure Expand (T : in out Instance; Num : Unsigned) is
   begin
      Reserve (T, Num);
      T.Priv.Last_Pos := T.Priv.Last_Pos + Num;
   end Expand;

   procedure Reserve (T : in out Instance; Num : Natural) is
   begin
      Reserve (T, Unsigned (Num));
   end Reserve;

   procedure Allocate (T : in out Instance; Num : Natural := 1) is
   begin
      Expand (T, Unsigned (Num));
   end Allocate;

   procedure Increment_Last (T : in out Instance) is
   begin
      --  Increase by 1.
      Expand (T, 1);
   end Increment_Last;

   procedure Decrement_Last (T : in out Instance) is
   begin
      T.Priv.Last_Pos := T.Priv.Last_Pos - 1;
   end Decrement_Last;

   procedure Set_Last (T : in out Instance; Index : Table_Index_Type)
   is
      New_Last : constant Unsigned :=
        (Table_Index_Type'Pos (Index)
           - Table_Index_Type'Pos (Table_Low_Bound) + 1);
   begin
      if New_Last < T.Priv.Last_Pos then
         --  Decrease length.
         T.Priv.Last_Pos := New_Last;
      else
         --  Increase length.
         Expand (T, New_Last - T.Priv.Last_Pos);
      end if;
   end Set_Last;

   procedure Init (T : in out Instance; Table_Initial : Positive)
   is
      --  Direct interface to malloc.
      function Cmalloc (Size : size_t) return Table_Thin_Ptr;
      pragma Import (C, Cmalloc, "malloc");
   begin
      if T.Table = null then
         --  Allocate memory if not already allocated.
         T.Priv.Length := Unsigned (Table_Initial);
         T.Table := Cmalloc (size_t (T.Priv.Length) * El_Size);
      end if;

      --  Table is initially empty.
      T.Priv.Last_Pos := 0;
   end Init;

   function Last (T : Instance) return Table_Index_Type is
   begin
      return Table_Index_Type'Val
        (Table_Index_Type'Pos (Table_Low_Bound)
           + Unsigned'Pos (T.Priv.Last_Pos) - 1);
   end Last;

   function Next (T : Instance) return Table_Index_Type is
   begin
      return Table_Index_Type'Val
        (Table_Index_Type'Pos (Table_Low_Bound) + T.Priv.Last_Pos);
   end Next;

   procedure Free (T : in out Instance) is
      --  Direct interface to free.
      procedure Cfree (Ptr : Table_Thin_Ptr);
      pragma Import (C, Cfree, "free");
   begin
      Cfree (T.Table);
      T := (Table => null,
            Priv => (Length => 0,
                     Last_Pos => 0));
   end Free;

   procedure Append (T : in out Instance; Val : Table_Component_Type) is
   begin
      Increment_Last (T);
      T.Table (Last (T)) := Val;
   end Append;
end Dyn_Tables;
