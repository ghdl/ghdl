
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
-- $Id: ch_05_ch_05_06.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_05_06 is

end entity ch_05_06;


----------------------------------------------------------------


architecture test of ch_05_06 is

  signal y : bit := '0';
  signal or_a_b : bit := '0';
  signal clk : bit := '0';

begin


  process_05_3_a : process is
  begin

    -- code from book:

    y <= not or_a_b after 5 ns;

    -- end of code from book

    wait on or_a_b;
  end process process_05_3_a;


  stimulus_05_3_a : process is
  begin
    or_a_b <= '1' after 20 ns,
              '0' after 40 ns;
    wait;
  end process stimulus_05_3_a;


  process_05_3_b : process is
                             constant T_pw : delay_length := 10 ns;
  begin

    -- code from book:

    clk <= '1' after T_pw, '0' after 2*T_pw;

    -- end of code from book

    wait for 2*T_pw;
  end process process_05_3_b;


end architecture test;
