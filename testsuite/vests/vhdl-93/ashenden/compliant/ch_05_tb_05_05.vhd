
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
-- $Id: ch_05_tb_05_05.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity tb_05_05 is
end entity tb_05_05;


library ieee;  use ieee.std_logic_1164.all;

architecture test of tb_05_05 is

  signal a, b : std_ulogic := '0';
  signal y : std_ulogic;

begin

  dut : entity work.and2(detailed_delay)
    port map ( a => a, b => b, y => y );

  stimulus : process is
  begin
    wait for 10 ns;
    a <= '1';	wait for 10 ns;
    b <= '1';	wait for 10 ns;
    b <= '0';	wait for 10 ns;

    b <= '1', '0' after  250 ps;   wait for 10 ns;
    b <= '1', '0' after  350 ps;   wait for 10 ns;
    b <= '1', '0' after  450 ps;   wait for 10 ns;
    b <= '1', '0' after  550 ps;   wait for 10 ns;
    b <= '1', '0' after  650 ps;   wait for 10 ns;
    b <= '1', '0' after  750 ps;   wait for 10 ns;
    b <= '1', '0' after  850 ps;   wait for 10 ns;

    b <= '1';	wait for 10 ns;
    b <= '0', '1' after  250 ps;   wait for 10 ns;
    b <= '0', '1' after  350 ps;   wait for 10 ns;
    b <= '0', '1' after  450 ps;   wait for 10 ns;

    b <= 'X';  wait for 10 ns;
    b <= '0';  wait for 10 ns;
    b <= 'X', '0' after 250 ps;   wait for 10 ns;
    wait;
  end process stimulus;

end architecture test;
