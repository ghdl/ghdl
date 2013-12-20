
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

library ieee;

entity inline_09 is

end entity inline_09;


----------------------------------------------------------------


architecture test of inline_09 is
begin

  process_5_c : process is

    use ieee.math_real.all;

    -- code from book

    type complex is record
        re : real;    -- Real part
        im : real;    -- Imaginary part
      end record;

    subtype positive_real is real range 0.0 to real'high;
    subtype principal_value is real range -math_pi to math_pi;

    type complex_polar is record
        mag : positive_real;      -- Magnitude
        arg : principal_value;    -- Angle in radians; -math_pi is illegal
      end record;

    -- end of code from book

  begin
    wait;
  end process process_5_c;


end architecture test;
