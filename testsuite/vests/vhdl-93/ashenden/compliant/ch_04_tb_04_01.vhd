
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
-- $Id: ch_04_tb_04_01.vhd,v 1.2 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity test_bench_04_01 is
end entity test_bench_04_01;

library ch4_pkgs;
use ch4_pkgs.pk_04_01.all;

architecture test_coeff_ram_abstract of test_bench_04_01 is

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

end architecture test_coeff_ram_abstract;
