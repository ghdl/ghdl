--  Simulation of VHDL
--  Copyright (C) 2023 Tristan Gingold
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

with Grt.Vhdl_Types;

package Simul.Main is
   Break_Time : Grt.Vhdl_Types.Std_Time;
   Break_Step : Boolean;

   Trace_Simulation : Boolean := False;

   Flag_Interractive : Boolean := False;
   Flag_Debug_Elab : Boolean := False;

   type Elaborate_Acc is access procedure;

   --  Procedure called for elaboration.
   Elaborate_Proc : Elaborate_Acc;

   --  Start and run simulation.
   procedure Simulation;

   Simulation_Finished : exception;
end Simul.Main;
