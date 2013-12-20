
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

entity tb_SR_flipflop is

end entity tb_SR_flipflop;


----------------------------------------------------------------


architecture test_checking of tb_SR_flipflop is

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

end architecture test_checking;
