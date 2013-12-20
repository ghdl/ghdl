
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
  generic ( fp : real := 10.0;	     -- pole in Hz for 'ztf
            Fsmp : real := 10.0e3);  -- sample frequency for 'ztf 
  port ( terminal input: electrical;
         terminal output: electrical );
end entity lowpass;

----------------------------------------------------------------

architecture ztf of lowpass is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  constant Tsmp : real := 1.0 / Fsmp;	  -- sample period
  constant wp : real := fp * math_2_pi;	  -- pole in rad/s
  constant n0 : real := Tsmp * wp; 	  -- z0 numerator coefficient (a)
  constant n1 : real := Tsmp * wp; 	  -- z-1 numerator coefficient (b)
  constant d0 : real := Tsmp * wp + 2.0;  -- z0 denominator coefficient (c)
  constant d1 : real := Tsmp * wp - 2.0;  -- z-1 denominator coefficient (d)
  constant num : real_vector := (n0, n1); 
  constant den : real_vector := (d0, d1);

begin

  vout == vin'ztf(num, den, Tsmp);

end ztf;
