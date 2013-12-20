
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

entity tb_coeff_ram is

end entity tb_coeff_ram;


----------------------------------------------------------------


architecture test_abstract of tb_coeff_ram is

  use work.coeff_ram_types.all;

  signal rd, wr : bit := '0';
  signal addr : coeff_ram_address := 0;
  signal d_in, d_out : real := 0.0;

begin

  dut : entity work.coeff_ram(abstract)
    port map ( rd => rd, wr => wr,
	       addr => addr,
	       d_in => d_in, d_out => d_out );

  stumulus : process is

  begin
    wait for 100 ns;

    addr <= 10;  d_in <= 10.0;  wait for 10 ns;
    wr <= '1';			wait for 10 ns;
    d_in <= 20.0;  		wait for 10 ns;
    wr <= '0';			wait for 70 ns;

    addr <= 20;			wait for 10 ns;
    rd <= '1';			wait for 10 ns;
    addr <= 10;			wait for 10 ns;
    rd <= '0';			wait for 10 ns;

    wait;
  end process stumulus;

end architecture test_abstract;
