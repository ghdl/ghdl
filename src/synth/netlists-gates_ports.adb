--  Easy access to ports (of some gates).
--  Copyright (C) 2017 Tristan Gingold
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

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;

package body Netlists.Gates_Ports is
   function Get_Mux2_Sel (Inst : Instance) return Input
   is
      pragma Assert (Get_Id (Inst) = Id_Mux2);
   begin
      return Get_Input (Inst, 0);
   end Get_Mux2_Sel;

   function Get_Mux2_I0 (Inst : Instance) return Input
   is
      pragma Assert (Get_Id (Inst) = Id_Mux2);
   begin
      return Get_Input (Inst, 1);
   end Get_Mux2_I0;

   function Get_Mux2_I1 (Inst : Instance) return Input
   is
      pragma Assert (Get_Id (Inst) = Id_Mux2);
   begin
      return Get_Input (Inst, 2);
   end Get_Mux2_I1;
end Netlists.Gates_Ports;
