
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
-- $Id: ap_a_fg_a_04.vhd,v 1.1.1.1 2001-08-22 18:20:47 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity fg_a_04 is

end entity fg_a_04;


library ieee;  use ieee.std_logic_1164.all;

architecture test of fg_a_04 is

  signal clk, reset, d, q, q_n : std_ulogic;

begin

  -- code from book

  ff1 : process (reset, clk) is
  begin
    if reset = '1' then
      q <= '0';
    elsif rising_edge(clk) then
      q <= d;
    end if;
  end process ff1;

  q_n <= not q;

  -- end code from book

  stimulus : process is
  begin
    reset <= '0';  clk <= '0';  d <= '1';  wait for 10 ns;
    reset <= '1', '0' after 30 ns;
    clk <= '1' after 10 ns, '0' after 20 ns;
    wait for 40 ns;
    clk <= '1', '0' after 20 ns;
    d <= '0' after 10 ns;

    wait;
  end process stimulus;

end architecture test;

