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

with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Elab.Vhdl_Prot is
   function Create (Inst : Synth_Instance_Acc) return Protected_Index;
   function Get (Idx : Protected_Index) return Synth_Instance_Acc;
   procedure Destroy (Idx : Protected_Index);
end Elab.Vhdl_Prot;
