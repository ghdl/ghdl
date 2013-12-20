
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

entity tb_S_R_flipflop is
end entity tb_S_R_flipflop;


architecture test of tb_S_R_flipflop is

  signal s, r : bit := '0';
  signal q, q_n : bit;

begin

  dut : entity work.S_R_flipflop(functional)
    port map ( s => s, r => r, q => q, q_n => q_n );

  stimulus : process is
  begin
    wait for 10 ns;
    s <= '1';	wait for 10 ns;
    s <= '0';	wait for 10 ns;
    r <= '1';	wait for 10 ns;
    r <= '0';	wait for 10 ns;
    s <= '1';	wait for 10 ns;
    r <= '1';	wait for 10 ns;
    s <= '0';	wait for 10 ns;
    r <= '0';	wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;
