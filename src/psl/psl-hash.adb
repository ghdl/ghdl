--  PSL - HDL interface.
--  Copyright (C) 2002-2016 Tristan Gingold
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

with Tables;
with PSL.Types; use PSL.Types;

package body PSL.Hash is

   type Index_Type is new Natural;
   No_Index : constant Index_Type := 0;

   type Cell_Record is record
      Res : Node;
      Next : Index_Type;
   end record;

   Hash_Size : constant Index_Type := 127;

   package Cells is new Tables
     (Table_Component_Type => Cell_Record,
      Table_Index_Type => Index_Type,
      Table_Low_Bound => 0,
      Table_Initial => 256);

   procedure Init is
   begin
      Cells.Set_Last (Hash_Size - 1);
      for I in 0 .. Hash_Size - 1 loop
         Cells.Table (I) := (Res => Null_Node, Next => No_Index);
      end loop;
   end Init;

   function Get_PSL_Node (Hdl : Int32; Loc : Location_Type) return Node
   is
      Idx : Index_Type := Index_Type (Hdl mod Int32 (Hash_Size));
      N_Idx : Index_Type;
      Res : Node;
   begin
      --  In the primary table.
      Res := Cells.Table (Idx).Res;
      if Res = Null_Node then
         Res := Create_Node (N_HDL_Bool);
         Set_HDL_Node (Res, Hdl);
         Set_Location (Res, Loc);
         Cells.Table (Idx).Res := Res;
         return Res;
      end if;

      loop
         if Get_HDL_Node (Res) = Hdl then
            return Res;
         end if;
         --  Look in collisions chain
         N_Idx := Cells.Table (Idx).Next;
         exit when N_Idx = No_Index;
         Idx := N_Idx;
         Res := Cells.Table (Idx).Res;
      end loop;
      Res := Create_Node (N_HDL_Bool);
      Set_HDL_Node (Res, Hdl);
      Set_Location (Res, Loc);
      Cells.Append ((Res => Res, Next => No_Index));
      Cells.Table (Idx).Next := Cells.Last;
      return Res;
   end Get_PSL_Node;
end PSL.Hash;
