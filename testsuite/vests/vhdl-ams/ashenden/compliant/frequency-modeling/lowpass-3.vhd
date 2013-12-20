
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;
library ieee;  use ieee.math_real.all;

entity lowpass is
  port ( terminal input : electrical;
         terminal output : electrical );
end entity lowpass;

----------------------------------------------------------------

architecture ltf of lowpass is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  constant wp : real := 10.0 * math_2_pi;   -- pole in rad/s
  constant num : real_vector := (0 => wp);  -- numerator in s
  constant den : real_vector := (wp, 1.0);  -- denominator in s
  
begin

  vout == vin'ltf(num, den);

end architecture ltf;
