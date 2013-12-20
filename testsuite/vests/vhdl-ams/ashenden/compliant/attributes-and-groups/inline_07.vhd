
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

entity inline_07 is

end entity inline_07;


----------------------------------------------------------------


architecture test of inline_07 is

  component multiplier is
  end component multiplier;

  type length is range 0 to integer'high
    units nm;
      um = 1000 nm;
      mm = 1000 um;
      mil = 25400 nm;
    end units length;

  type coordinate is record
      x, y : length;
    end record coordinate;

  type orientation_type is (up, down, left, right);

  attribute cell_allocation : string;
  attribute cell_position : coordinate;
  attribute cell_orientation : orientation_type;

  -- code from book:

  attribute cell_allocation of mult : label is "wallace_tree_multiplier";
  attribute cell_position of mult : label is ( 1200 um, 4500 um );
  attribute cell_orientation of mult : label is down;

  -- end of code from book

begin

  mult : component multiplier;

end architecture test;
