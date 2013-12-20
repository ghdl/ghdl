
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


entity tb_flipflop is
end entity tb_flipflop;


architecture test of tb_flipflop is

  signal clk, d, q : bit;

begin

  dut : entity work.flipflop(behavior)
    generic map ( Tsetup => 3 ns )
    port map ( clk => clk, d => d, q => q );

  clk <= '1' after 10 ns, '0' after 20 ns;

  d <= '1' after 8 ns;

end architecture test;
