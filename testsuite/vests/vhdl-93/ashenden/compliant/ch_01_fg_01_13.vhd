
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
-- $Id: ch_01_fg_01_13.vhd,v 1.1.1.1 2001-08-22 18:20:47 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity test_bench is
end entity test_bench;

architecture test_reg4 of test_bench is

  signal d0, d1, d2, d3, en, clk, q0, q1, q2, q3 : bit;

begin

  dut : entity work.reg4(behav)
    port map ( d0, d1, d2, d3, en, clk, q0, q1, q2, q3 );

  stimulus : process is
  begin
    d0 <= '1';  d1 <= '1';  d2 <= '1';  d3 <= '1';
    en <= '0';  clk <= '0';
    wait for 20 ns;
    en <= '1';  wait for 20 ns;
    clk <= '1';  wait for 20 ns;
    d0 <= '0';  d1 <= '0';  d2 <= '0';  d3 <= '0';  wait for 20 ns;
    en <= '0';  wait for 20 ns;
    -- . . .
    wait;
  end process stimulus;

end architecture test_reg4;
