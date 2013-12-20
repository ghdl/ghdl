
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
-- $Id: ch_03_tb_03_09.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity test_bench_03_09 is
end entity test_bench_03_09;

architecture test_max3_check_error of test_bench_03_09 is

  signal a, b, c, z : integer := 0;

begin

  dut : entity work.max3(check_error)
    port map ( a => a, b => b, c => c, z => z );

  stumulus : process is

  begin
    wait for 10 ns;
    a <= 7;				wait for 10 ns;
    b <= 10;		wait for 10 ns;
    c <= 15;	wait for 10 ns;
    a <= 12;				wait for 10 ns;
    a <= 20;				wait for 10 ns;

    wait;
  end process stumulus;

end architecture test_max3_check_error;
