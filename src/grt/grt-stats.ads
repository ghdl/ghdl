--  GHDL Run Time (GRT) - statistics.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

package Grt.Stats is
   --  Entry points to gather statistics.
   procedure Start_Elaboration;
   procedure Start_Order;

   --  Time in user processes.
   procedure Start_Processes;


   --  Time in next time computation.
   procedure Start_Next_Time;


   --  Time in signals update.
   procedure Start_Update;


   --  Time in process resume
   procedure Start_Resume;


   procedure End_Simulation;

   --  Disp all statistics.
   procedure Disp_Stats;
end Grt.Stats;



