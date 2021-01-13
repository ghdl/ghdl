--  Type interning - set of unique objects.
--  Copyright (C) 2019 Tristan Gingold
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


package body Dyn_Interning is
   function Build_No_Value (Obj : Object_Type) return No_Value_Type
   is
      pragma Unreferenced (Obj);
   begin
      return (null record);
   end Build_No_Value;

   procedure Get
     (Inst : in out Instance; Params : Params_Type; Res : out Object_Type)
   is
      Idx : Index_Type;
   begin
      Get_Index (Inst, Params, Idx);
      Res := Get_By_Index (Inst, Idx);
   end Get;
end Dyn_Interning;
