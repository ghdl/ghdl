--  GHDL Run Time (GRT) -  analog solver.
--  Copyright (C) 2022 Tristan Gingold
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

with Grt.Types; use Grt.Types;

package Grt.Analog_Solver is
   --  Finish initialization.
   procedure Initialize;
   pragma Import (Ada, Initialize, "grt__analog_solver__initialize");

   --  Run the analog solver.
   --  Return status:
   --  0: Ok, Tn reached
   --  1: Stopped due to zero crossing
   --  2: failure
   procedure Solve (T : Ghdl_F64; Tn : in out Ghdl_F64; Res : out Integer);
   pragma Import (Ada, Solve, "grt__analog_solver__solve");

   Solve_Ok : constant Integer := 0;
   Solve_Cross : constant Integer := 1;
   Solve_Failure : constant Integer := 2;
end Grt.Analog_Solver;
