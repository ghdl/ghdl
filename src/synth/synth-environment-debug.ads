--  Debug utilities for synthesis environment.
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

generic
package Synth.Environment.Debug is
   procedure Put_Wire_Id (Wid : Wire_Id);
   procedure Debug_Wire (Wid : Wire_Id);
   procedure Debug_Assign (Asgn : Seq_Assign);
   procedure Debug_Conc_Assigns (First : Conc_Assign);
private
   procedure Debug_Phi (Id : Phi_Id);
end Synth.Environment.Debug;
