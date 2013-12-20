
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

library IEEE_proposed;  use IEEE_proposed.electrical_systems.all;

entity v_Sum is
  generic ( k1 : real := 1.0;
            k2 : real := -1.0 );
  port ( terminal in1, in2 : electrical;
         terminal output : electrical );
end entity v_Sum;

----------------------------------------------------------------

architecture behavioral of v_Sum is

  quantity vin1 across in1 to electrical_ref;
  quantity vin2 across in2 to electrical_ref;
  quantity vout across iout through output to electrical_ref;

begin

  vout == k1 * vin1 + k2 * vin2;

end architecture behavioral;
