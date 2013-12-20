
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

entity inline_03 is

end entity inline_03;


----------------------------------------------------------------


library ieee;  use ieee.numeric_bit.all;

architecture test of inline_03 is

  constant T_delay_adder : delay_length := 10 ns;

  -- code from book:

  function bv_add ( bv1, bv2 : in bit_vector ) return bit_vector is
  begin
    -- . . .
    -- not in book
    return bit_vector(unsigned(bv1) + unsigned(bv2));
    -- end not in book
  end function bv_add;

  signal source1, source2, sum : bit_vector(0 to 31);

  -- end of code from book


begin


  -- code from book:

  adder : sum <= bv_add(source1, source2) after T_delay_adder;

  -- end of code from book


  ----------------


  stimulus : process is
  begin
    wait for 50 ns;
    source1 <= X"00000002";  source2 <= X"00000003";  wait for 50 ns;
                             source2 <= X"FFFFFFF0";  wait for 50 ns;
    source1 <= X"00000010";                           wait for 50 ns;

    wait;
  end process stimulus;


end architecture test;
