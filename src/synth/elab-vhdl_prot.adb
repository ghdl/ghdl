--  Protected objects table
--  Copyright (C) 2022 Tristan Gingold
--
--  This file is part of GHDL.
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

package body Elab.Vhdl_Prot is

   package Prot_Table is new Tables
     (Table_Component_Type => Synth_Instance_Acc,
      Table_Index_Type => Protected_Index,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Create (Inst : Synth_Instance_Acc) return Protected_Index is
   begin
      Prot_Table.Append (Inst);
      return Prot_Table.Last;
   end Create;

   function Get (Idx : Protected_Index) return Synth_Instance_Acc
   is
      Res : Synth_Instance_Acc;
   begin
      pragma Assert (Idx > No_Protected_Index);
      pragma Assert (Idx <= Prot_Table.Last);
      Res := Prot_Table.Table (Idx);
      pragma Assert (Res /= null);
      return Res;
   end Get;

   procedure Destroy (Idx : Protected_Index) is
   begin
      pragma Assert (Idx > No_Protected_Index);
      pragma Assert (Idx <= Prot_Table.Last);
      pragma Assert (Prot_Table.Table (Idx) /= null);
      Prot_Table.Table (Idx) := null;

      --  TODO: Decrease if last ?
   end Destroy;

end Elab.Vhdl_Prot;
