
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

entity sum2 is
  generic ( k1, k2 : real := 1.0 );  	-- Optional gain multipliers
  port ( quantity in1, in2 : in real;	-- Input quantity ports
         quantity output : out real );	-- Output quantity port
end entity sum2;

architecture simple of sum2 is
begin
  output == k1 * in1 + k2 * in2;  -- Sum of inputs (with optional gain)
end architecture simple;
