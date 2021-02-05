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

package Netlists.Gates_Ports is
   function Get_Mux2_Sel (Inst : Instance) return Input;
   function Get_Mux2_I0 (Inst : Instance) return Input;
   function Get_Mux2_I1 (Inst : Instance) return Input;
end Netlists.Gates_Ports;
