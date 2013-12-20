
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

-- code from book:

entity and3 is
  port ( a, b, c : in bit := '1';
         z, not_z : out bit);
end entity and3;

-- end of code from book


----------------------------------------------------------------


architecture functional of and3 is
begin

  non_inverting:
    z <= a and b and c;

  inverting:
    not_z <= not (a and b and c);

end architecture functional;


----------------------------------------------------------------


entity inline_24 is

end entity inline_24;


----------------------------------------------------------------


library util;  use util.stimulus_generators.all;

architecture test of inline_24 is

  signal s1, s2, ctrl1_a, ctrl1_b : bit;
  signal test_input : bit_vector(1 to 2);

begin


  block_4_a : block is
    port ( ctrl1 : out bit );
    port map ( ctrl1 => ctrl1_a );
  begin

    -- code from book:

    g1 : entity work.and3 port map ( a => s1, b => s2, not_z => ctrl1 );

    -- end of code from book

  end block block_4_a;


  ----------------


  block_4_b : block is
    port ( ctrl1 : out bit );
    port map ( ctrl1 => ctrl1_b );
  begin

    -- code from book:

    g1 : entity work.and3 port map ( a => s1, b => s2, not_z => ctrl1,
                                     c => open, z => open );

    -- end of code from book

  end block block_4_b;


  ----------------


  stimulus : all_possible_values( bv => test_input,
				  delay_between_values => 10 ns );

  (s1, s2) <= test_input;

  verifier :
    assert ctrl1_a = ctrl1_b
      report "versions differ";


end architecture test;
