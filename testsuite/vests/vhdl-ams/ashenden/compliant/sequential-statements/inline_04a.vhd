
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

entity inline_04a is

end entity inline_04a;


----------------------------------------------------------------


architecture test of inline_04a is

  type gear_type is (gear_1, gear_2, neutral);
  signal gear : gear_type := gear_1;

  signal gear_engaged : boolean := false;

begin

  process_1_d : process (gear) is

    variable max_acceleration : real := 0.0;
    variable reverse_indicator : boolean := true;

  begin

    -- code from book:

    if gear = neutral then
      max_acceleration := 0.0;
      reverse_indicator := false;
      gear_engaged <= false;
    end if;

    -- end of code from book

  end process process_1_d;

  stimulus : process is
  begin
    gear <= gear_2 after 100 ns, neutral after 200 ns;
    wait;
  end process stimulus;

end architecture test;
