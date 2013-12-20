
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

entity inline_07a is

end entity inline_07a;


----------------------------------------------------------------


architecture test of inline_07a is
begin


  process is

    -- code from book:

    type value_cell;

    type value_ptr is access value_cell;

    type value_cell is record
        value : real_vector(0 to 3);
        next_cell : value_ptr;
      end record value_cell;

    variable value_list : value_ptr;

    -- end of code from book

  begin

    -- code from book:

    if value_list /= null then
      -- . . .       -- do something with the list
      -- not in book
      report "value_list /= null";
      -- end not in book
    end if;

    value_list := new value_cell'( real_vector'(0.0, 5.0, 0.0, 42.0), value_list );

    value_list := new value_cell'( real_vector'(3.3, 2.2, 0.27, 1.9), value_list );

    value_list := new value_cell'( real_vector'(2.9, 0.1, 21.12, 8.3), value_list );

    -- end of code from book

    wait;
  end process;


end architecture test;
