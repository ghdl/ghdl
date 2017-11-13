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

package body Tables is
   function Allocate (Num : Natural := 1) return Table_Index_Type
   is
      Res : constant Table_Index_Type := Dyn_Table.Next (T);
   begin
      Dyn_Table.Allocate (T, Num);

      return Res;
   end Allocate;

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
      Dyn_Table.Init (T);
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
