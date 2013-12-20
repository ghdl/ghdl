
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
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity opamp_2pole_res is
  generic ( A : real := 1.0e6;	    -- open loop gain
            rin : real := 1.0e6;    -- input resistance
            rout : real := 100.0;   -- output resistance
            fp1 : real := 5.0;	    -- first pole
            fp2 : real := 9.0e5 );  -- second pole
  port ( terminal in_pos, in_neg, output : electrical );
end entity opamp_2pole_res;

----------------------------------------------------------------

architecture ltf of opamp_2pole_res is

  constant wp1 : real := fp1 * math_2_pi;
  constant wp2 : real := fp2 * math_2_pi;
  constant num : real_vector := (0 => wp1 * wp2 * A);
  constant den : real_vector := (wp1 * wp2, wp1 + wp2, 1.0);
  quantity v_in across i_in through in_pos to in_neg;
  quantity v_out across i_out through output;

begin

  i_in  == v_in / rin;  -- input current

  v_out == v_in'ltf(num, den) + i_out * rout;

end architecture ltf;
