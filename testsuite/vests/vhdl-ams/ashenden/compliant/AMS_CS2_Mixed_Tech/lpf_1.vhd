
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

entity lpf_1 is
  generic ( fp : real; 		   -- pole freq in hertz
            gain : real := 1.0 );  -- filter gain
  port ( quantity input : in real;
         quantity output : out real);
end entity lpf_1;

----------------------------------------------------------------

library ieee;  use ieee.math_real.all;

architecture simple of lpf_1 is

  constant wp : real := math_2_pi*fp;
  constant num : real_vector := (0 => wp * gain);  -- "0 =>" is needed to give
                                                   -- vector index when only
                                                   -- a single element is used.
  constant den : real_vector := (wp, 1.0);

begin

  output == input'ltf(num, den);

end architecture simple;
