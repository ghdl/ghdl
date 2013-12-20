
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

package physical_attributes is

  -- code from book (in text)

  attribute layout_ignore : boolean;
  attribute pin_number : positive;

  -- end code from book

end package physical_attributes;


-- code from book

library ieee;  use ieee.std_logic_1164.all;
use work.physical_attributes.all;

entity \74x138\ is
  generic ( Tpd : time );
  port ( en1, en2a_n, en2b_n : in std_logic;
         s0, s1, s2 : in std_logic;
         y0, y1, y2, y3, y4, y5, y6, y7 : out std_logic );

  attribute layout_ignore of Tpd : constant is true;

  attribute pin_number of s0 : signal is 1;
  attribute pin_number of s1 : signal is 2;
  attribute pin_number of s2 : signal is 3;
  attribute pin_number of en2a_n : signal is 4;
  -- . . .

end entity \74x138\;

-- code from book
