
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

library ieee, ieee_proposed;
use ieee_proposed.electrical_systems.all;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity bfsk is

  generic ( fc : real := 1.0e6;		-- mean carrier frequency
            delta_f : real := 5.0e3;    -- difference between low and high
                                        --   carrier frequencies
            amp : voltage := 1.0;	-- amplitude of modulated signal
            offset : voltage := 0.0 );  -- output offset voltage

  port ( signal d_in : in std_logic;	 -- digital input
         terminal a_out : electrical );  -- output terminal

end entity bfsk; 

----------------------------------------------------------------

architecture behavioral of bfsk is

  quantity vout across iout through a_out;         -- output branch
  quantity phi : real; 		                   -- free quantity angle in radians
  constant wc : real := math_2_pi * fc;		   -- convert fc to rad/s
  constant delta_w : real := math_2_pi * delta_f;  -- convert delta_f to rad/s

begin

  if To_X01(d_in) = '0' use
    phi'dot == wc; 		-- set to carrier frequency
  elsif To_X01(d_in) = '1' use
    phi'dot == wc + delta_w;	-- set to carrier frequency + delta
  else
    phi'dot == 0.0;
  end use;

  break on d_in;

  vout == offset + amp * sin(phi);  -- create sinusoidal output using phi

end architecture behavioral;
