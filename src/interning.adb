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

package body Interning is
   Inst : Implementation.Instance;

   procedure Init is
   begin
      Implementation.Init (Inst);
   end Init;

   function Get (Params : Params_Type) return Object_Type
   is
      Res : Object_Type;
   begin
      Implementation.Get (Inst, Params, Res);
      return Res;
   end Get;

   function Last_Index return Index_Type is
   begin
      return Implementation.Last_Index (Inst);
   end Last_Index;

   function Get_By_Index (Index : Index_Type) return Object_Type is
   begin
      return Implementation.Get_By_Index (Inst, Index);
   end Get_By_Index;
end Interning;
