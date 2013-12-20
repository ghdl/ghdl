
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_01_tb_01_02.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity test_bench_01_02 is

end entity test_bench_01_02;

architecture test_reg4_struct of test_bench_01_02 is

  signal d0, d1, d2, d3, en, clk, q0, q1, q2, q3 : bit;

begin

  dut : entity work.reg4(struct)
    port map ( d0 => d0, d1 => d1, d2 => d2, d3 => d3, en => en, clk => clk,
               q0 => q0, q1 => q1, q2 => q2, q3 => q3 );

  stimulus : process is
  begin
    wait for 20 ns;
    (d0, d1, d2, d3) <= bit_vector'("1010");	wait for 20 ns;
    en <= '1';					wait for 20 ns;
    clk <= '1';					wait for 20 ns;
    (d0, d1, d2, d3) <= bit_vector'("0101");	wait for 20 ns;
    clk <= '0';					wait for 20 ns;
    (d0, d1, d2, d3) <= bit_vector'("0000");	wait for 20 ns;
    en <= '1';					wait for 20 ns;
    (d0, d1, d2, d3) <= bit_vector'("1111");	wait for 20 ns;

    wait;
  end process stimulus;

end architecture test_reg4_struct;
