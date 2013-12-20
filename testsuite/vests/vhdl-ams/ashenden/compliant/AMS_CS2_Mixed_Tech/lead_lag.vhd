
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

library ieee;  use ieee.math_real.all;

entity lead_lag is
  generic ( k : real := 400.0;     -- gain multiplier
            f1 : real := 5.0;	   -- break frequency (zero)
            f2 : real := 2000.0);  -- break frequency (pole)
  port ( quantity input : in real;
         quantity output : out real);
end entity lead_lag;

----------------------------------------------------------------

architecture simple of lead_lag is

  constant num : real_vector := (f1 * math_2_pi, 1.0);
  constant den : real_vector := (f2 * math_2_pi, 1.0);

begin

  output == k * input'ltf(num, den);

end architecture simple;
