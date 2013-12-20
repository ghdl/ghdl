
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

architecture do_nothing of ROM is
begin
end architecture do_nothing;


entity tb_rom is
end entity tb_rom;


architecture test of tb_rom is

  signal address : natural := 0;
  signal data : bit_vector(0 to 7);
  signal enable : bit := '0';

begin

  dut : entity work.ROM(do_nothing)
    port map ( address => address, data => data, enable => enable );

  stimulus : process is
  begin
    wait for 100 ns;
    address <= 1000;  wait for 10 ns;
    enable <= '1', '0' after 10 ns;  wait for 90 ns;
    address <= 1004;  wait for 10 ns;
    enable <= '1', '0' after 10 ns;  wait for 90 ns;
    address <= 1008;  wait for 10 ns;
    enable <= '1', '0' after 10 ns;  wait for 90 ns;

    wait;
  end process stimulus;

end architecture test;
