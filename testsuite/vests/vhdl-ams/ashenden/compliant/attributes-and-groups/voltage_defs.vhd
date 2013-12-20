
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

package voltage_defs is

  type voltage is range -2e9 to +2e9
    units
      nV;
      uV = 1000 nV;
      mV = 1000 uV;
      V = 1000 mV;
    end units voltage;

  attribute resolution : real;

  attribute resolution of nV : units is 1.0;
  attribute resolution of uV : units is 0.01;
  attribute resolution of mV : units is 0.01;
  attribute resolution of V : units is 0.001;

end package voltage_defs;
