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

with Types; use Types;

with Grt.Vhdl_Types;
with Grt.Stdio;

package Simul.Main is
   --  For debugger: stop at time or next delta.
   Break_Time : Grt.Vhdl_Types.Std_Time;
   Break_Step : Boolean;

   Trace_Simulation : Boolean := False;

   Flag_Interractive : Boolean := False;
   Flag_Debug_Elab : Boolean := False;

   --  Start and run simulation.
   procedure Simulation;

   --  Filename for the CSV output.
   Csv_Filename : String_Acc;
   Csv_File : Grt.Stdio.FILEs;

   Simulation_Finished : exception;
end Simul.Main;
