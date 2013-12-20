
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

entity inline_01 is

end entity inline_01;


----------------------------------------------------------------


architecture test of inline_01 is
begin


  block_1_a : block is

    -- code from book:

    type word is array (0 to 31) of bit;

    --

    type controller_state is (initial, idle, active, error);

    type state_counts is array (idle to error) of natural;

    -- end of code from book

  begin
  end block block_1_a;


  process_1_a : process is

    -- code from book:

    type word is array (31 downto 0) of bit;

    --

    type controller_state is (initial, idle, active, error);

    --

    type state_counts is
      array (controller_state range idle to error) of natural;

    --

    subtype coeff_ram_address is integer range 0 to 63;
    type coeff_array is array (coeff_ram_address) of real;

    --

    variable buffer_register, data_register : word;
    variable counters : state_counts;
    variable coeff : coeff_array;

    -- end of code from book

  begin

    -- code from book:

    coeff(0) := 0.0;

    counters(active) := counters(active) + 1;

    data_register := buffer_register;

    -- end of code from book

    wait;
  end process process_1_a;


end architecture test;
