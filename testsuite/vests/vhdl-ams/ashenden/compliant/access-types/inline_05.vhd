
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

entity inline_05 is

end entity inline_05;


----------------------------------------------------------------


architecture test of inline_05 is
begin


  process is

    -- code from book:

    type coordinate is array (1 to 3) of real;
    type coordinate_ptr is access coordinate;

    variable origin : coordinate_ptr := new coordinate'(0.0, 0.0, 0.0);

    type time_array is array (positive range <>) of time;
    variable activation_times : time_array(1 to 100);

    -- end of code from book

  begin

    report real'image( origin(1) );
    report real'image( origin(2) );
    report real'image( origin(3) );
    report real'image( origin.all(1) );

    wait;
  end process;


  process is

    type time_array is array (positive range <>) of time;

    -- code from book:

    type time_array_ptr is access time_array;

    variable activation_times : time_array_ptr;

    -- end of code from book

  begin

    -- code from book:

    activation_times := new time_array'(10 us, 15 us, 40 us);

    activation_times := new time_array'( activation_times.all
                                         & time_array'(70 us, 100 us) );

    activation_times := new time_array(1 to 10);

    -- end of code from book

    wait;
  end process;


end architecture test;
