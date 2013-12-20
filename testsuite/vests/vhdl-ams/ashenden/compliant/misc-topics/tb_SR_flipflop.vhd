
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


architecture test of tb_SR_flipflop is

  signal s_n, r_n, q, q_n : bit;

begin

  dut : entity work.SR_flipflop
    port map ( s_n, r_n, q, q_n );

  s_n <= '1',
         '0' after 10 ns, '1' after 15 ns,
         '0' after 30 ns, '1' after 40 ns;

  r_n <= '0', '1' after 5 ns,
         '0' after 20 ns, '1' after 25 ns,
         '0' after 30 ns, '1' after 35 ns;

end architecture test;
