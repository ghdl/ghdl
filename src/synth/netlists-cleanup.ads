--  Netlist cleanup.
--  Copyright (C) 2019 Tristan Gingold
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

with Netlists.Builders; use Netlists.Builders;

package Netlists.Cleanup is
   --  Remove instances of module M whose outputs are not connected.
   --  Their inputs will be deconnected, which can result in new instances
   --  that are also removed.
   procedure Remove_Unconnected_Instances (M : Module);

   --  Stronger version of Remove_Unconnected_Instances: use a mark and
   --  sweep algorithm.
   procedure Mark_And_Sweep (M : Module);

   --  Reconnection inputs of width 0 (the null inputs) to an Const_X gate.
   --  This will make all the null logic unconnected and ready to be cleaned.
   procedure Replace_Null_Inputs (Ctxt : Context_Acc; M : Module);

   --  Remove Id_Output gates.
   procedure Remove_Output_Gates (M : Module);
end Netlists.Cleanup;
