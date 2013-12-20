
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
                        
entity v_BPF is

  generic ( k : real := 1.0;	   -- filter gain
            fo : real := 100.0e3;  -- center frequency [Hz]
            q : real := 0.707 );   -- quality factor

  port ( terminal input	: electrical;   
         terminal output : electrical );

end entity v_BPF;

----------------------------------------------------------------

architecture behavioral of v_BPF is

  quantity vin across input;                        
  quantity vout across iout through output;            
  constant wo : real := math_2_pi * fo;		         -- frequency in radians
  constant num : real_vector := (0.0, wo);	         -- numerator array
  constant den : real_vector := (wo * wo, wo / q, 1.0);  -- denominator array

begin

  vout == k * vin'ltf(num, den);     -- Laplace transform of output

end architecture behavioral;
