
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

entity tb_full is
end entity tb_full;


library util;  use util.stimulus_generators.all;

architecture test of tb_full is

  signal in1, in2, in3, out1, out2 : bit;
  signal test_vector : bit_vector(1 to 3);

begin

  dut : configuration work.full
    generic map ( inpad_delay => 2 ns, outpad_delay => 3 ns )
    port map ( in1 => in1, in2 => in2, in3 => in3, out1 => out1, out2 => out2 );

  stimulus : all_possible_values ( test_vector, 50 ns );

  (in1, in2, in3) <= test_vector;

end architecture test;
