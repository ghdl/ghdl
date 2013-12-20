
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
-- $Id: ch_03_tb_03_08.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity test_bench_03_08 is
end entity test_bench_03_08;

architecture test_SR_flipflop_checking of test_bench_03_08 is

  signal S, R, Q : bit := '0';

begin

  dut : entity work.SR_flipflop(checking)
    port map ( S => S, R => R, Q => Q );

  stumulus : process is

  begin
    wait for 10 ns;
    S <= '1';			wait for 10 ns;
    S <= '0';			wait for 10 ns;
    S <= '1';			wait for 10 ns;
    S <= '0';			wait for 10 ns;
    R <= '1';			wait for 10 ns;
    R <= '0';			wait for 10 ns;
    R <= '1';			wait for 10 ns;
    R <= '0';			wait for 10 ns;
    S <= '1';	R <= '1';	wait for 10 ns;
    R <= '0';			wait for 10 ns;
    S <= '0';			wait for 10 ns;

    wait;
  end process stumulus;

end architecture test_SR_flipflop_checking;
