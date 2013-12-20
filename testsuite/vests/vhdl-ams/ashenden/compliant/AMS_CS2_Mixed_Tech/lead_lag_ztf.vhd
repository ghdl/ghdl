
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

entity lead_lag_ztf is
  
  generic ( a1 : real := 2.003140;
            a2 : real := -1.996860;
            b1 : real := 3.250000;
            b2 : real := -0.750000;
            k : real := 400.0;		 -- normalizing gain
            tsampl : real := 0.1e-3;	 -- sample period
            init_delay : real := 0.0 );  -- optional delay
        
  port ( quantity input : in real;
         quantity output : out real );
        
end entity lead_lag_ztf;

----------------------------------------------------------------

architecture simple of lead_lag_ztf is

  constant num: real_vector := (a1, a2);
  constant den: real_vector := (b1, b2);

begin

  output == k * input'ztf(num, den, tsampl, init_delay);  -- implement transfer function

end architecture simple;
