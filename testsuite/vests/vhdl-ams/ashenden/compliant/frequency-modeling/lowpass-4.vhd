
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

entity lowpass is
  generic ( fp : real := 10.0;	      -- pole in Hz for 'zoh, 'delayed
            Fsmp : real := 10.0e3 );  -- sample frequency for 'zoh, 'delayed
  port ( terminal input : electrical;
         terminal output: electrical );
end entity lowpass;

----------------------------------------------------------------

architecture z_minus_1 of lowpass is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  quantity vin_sampled : real;		  -- discrete sample of input quantity
  quantity vin_zm1, vout_zm1 : real; 	  -- z**-1 
  constant Tsmp : real := 1.0 / Fsmp;	  -- sample period
  constant wp : real := fp * math_2_pi;   -- pole in rad/s
  constant n0 : real := Tsmp * wp;        -- z0 numerator coefficient
  constant n1 : real := Tsmp * wp;        -- z-1 numerator coefficient
  constant d0 : real := Tsmp * wp + 2.0;  -- z0 denominator coefficient
  constant d1 : real := Tsmp * wp - 2.0;  -- z-1 denominator coefficient

begin

  vin_sampled  == vin'zoh(Tsmp);

  vin_zm1  == vin_sampled'delayed(Tsmp);

  vout_zm1 == vout'delayed(Tsmp);

  vout == vin_sampled * n0 / d0 + n1 * vin_zm1 / d0 - d1 * vout_zm1 / d0;

end z_minus_1;
