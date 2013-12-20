
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

entity tb_reg4 is
end entity tb_reg4;


architecture test of tb_reg4 is

  signal clk, clr, d0, d1, d2, d3 : bit := '0';
  signal q0, q1, q2, q3 : bit;

begin

  dut : entity work.reg4(struct)
    port map ( clk => clk, clr => clr,
	       d0 => d0, d1 => d1, d2 => d2, d3 => d3,
	       q0 => q0, q1 => q1, q2 => q2, q3 => q3 );

  stimulus : process is
  begin
    (d3, d2, d1, d0) <= bit_vector'(b"1010");	wait for 10 ns;
    clk <= '1';					wait for 10 ns;
    (d3, d2, d1, d0) <= bit_vector'(b"0101");	wait for 10 ns;
    clk <= '0';					wait for 10 ns;
    (d3, d2, d1, d0) <= bit_vector'(b"1111");	wait for 10 ns;
    clr <= '1';					wait for 10 ns;
    clk <= '1';					wait for 10 ns;
    clr <= '0';					wait for 10 ns;
    clk <= '0';					wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;
