
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
-- $Id: ch_03_tb_03_10.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity test_bench_03_10 is
end entity test_bench_03_10;

architecture test_edge_triggered_register_check_timing of test_bench_03_10 is

  signal clock : bit := '0'; 
  signal d_in, d_out : real := 0.0;

begin

  dut : entity work.edge_triggered_register(check_timing)
    port map ( clock => clock, d_in => d_in, d_out => d_out );

  stumulus : process is

  begin
    wait for 20 ns;

    d_in <= 1.0;			wait for 10 ns;
    clock <= '1', '0' after 10 ns;	wait for 20 ns;

    d_in <= 2.0;			wait for 10 ns;
    clock <= '1', '0' after 5 ns;	wait for 20 ns;

    d_in <= 3.0;			wait for 10 ns;
    clock <= '1', '0' after 4 ns;	wait for 20 ns;

    wait;
  end process stumulus;

end architecture test_edge_triggered_register_check_timing;
