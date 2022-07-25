--  Efficient expandable one dimensional array.
--  Copyright (C) 2015 Tristan Gingold
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

package body Tables is
   function Allocate (Num : Natural := 1) return Table_Index_Type
   is
      Res : constant Table_Index_Type := Dyn_Table.Next (T);
   begin
      Dyn_Table.Allocate (T, Num);

      return Res;
   end Allocate;

   procedure Reserve (Num : Natural := 1) is
   begin
      Dyn_Table.Reserve (T, Num);
   end Reserve;

   procedure Increment_Last is
   begin
      --  Increase by 1.
      Dyn_Table.Increment_Last (T);
   end Increment_Last;

   procedure Decrement_Last is
   begin
      Dyn_Table.Decrement_Last (T);
   end Decrement_Last;

   procedure Set_Last (Index : Table_Index_Type) is
   begin
      Dyn_Table.Set_Last (T, Index);
   end Set_Last;

   procedure Init is
   begin
      Dyn_Table.Init (T, Table_Initial);
   end Init;

   function Last return Table_Index_Type is
   begin
      return Dyn_Table.Last (T);
   end Last;

   procedure Free is
   begin
      Dyn_Table.Free (T);
   end Free;

   procedure Append (Val : Table_Component_Type) is
   begin
      Dyn_Table.Append (T, Val);
   end Append;
begin
   Init;
end Tables;
