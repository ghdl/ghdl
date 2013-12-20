
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

entity inline_08 is

end entity inline_08;


----------------------------------------------------------------


library util;  use util.stimulus_generators.all;

architecture test of inline_08 is

  constant T_pd : delay_length := 5 ns;

  signal a, b : bit := '0';
  signal test_inputs : bit_vector(1 to 2);

begin


  block_3_f : block is

    signal sum, carry : bit;

  begin

    -- code from book:

    half_add : process is
    begin
      sum <= a xor b after T_pd;
      carry <= a and b after T_pd;
      wait on a, b;
    end process half_add;

    -- end of code from book

  end block block_3_f;


  ----------------


  block_3_g : block is

    signal sum, carry : bit;

  begin

    -- code from book:

    half_add : process (a, b) is
    begin
      sum <= a xor b after T_pd;
      carry <= a and b after T_pd;
    end process half_add;

    -- end of code from book

  end block block_3_g;


  ----------------


  stimulus_3_f_g :
    all_possible_values(test_inputs, 20 ns);

  (a, b) <= test_inputs;


end architecture test;
