--  Native dataflow (.flow) JSON exporter for simulation.
--  Copyright (C) 2026 Tristan Gingold
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

package Simul.Flow is
   --  Phase-0 spike: write a minimal JSON snapshot of the elaborated dataflow
   --  tables (process/signal/driver/sensitivity/connection counts plus signal
   --  names) to Filename, to prove the data is reachable from this hook point.
   procedure Dump (Filename : String);
end Simul.Flow;
