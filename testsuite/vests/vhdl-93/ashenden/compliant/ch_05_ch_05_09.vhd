
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
-- $Id: ch_05_ch_05_09.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_05_09 is

end entity ch_05_09;


----------------------------------------------------------------


architecture test of ch_05_09 is

  signal clk, reset, trigger, test0, test1 : bit := '0';

begin


  process_05_3_h : process is
  begin

    -- code from book:

    wait until clk = '1';

    -- end of code from book

    report "clk rising edge detected";

  end process process_05_3_h;


  ----------------


  process_05_3_i : process is
  begin

    -- code from book:

    wait on clk until reset = '0';

    -- end of code from book

    report "synchronous reset detected";

  end process process_05_3_i;


  ----------------


  process_05_3_j : process is
  begin

    -- code from book:

    wait until trigger = '1' for 1 ms;

    -- end of code from book

    if trigger'event and trigger = '1' then
      report "trigger rising edge detected";
    else
      report "trigger timeout";
    end if;

  end process process_05_3_j;


  ----------------


  -- code from book:

  test_gen : process is
  begin
    test0 <= '0' after 10 ns, '1' after 20 ns, '0' after 30 ns, '1' after 40 ns;
    test1 <= '0' after 10 ns, '1' after 30 ns;
    wait;
  end process test_gen;

  -- end of code from book


  ----------------


  stimulus_05_3_h_i_j : process is
  begin
    clk <= '1' after 10 ns, '0' after 20 ns,
           '1' after 30 ns, '0' after 40 ns,
           '1' after 50 ns, '0' after 60 ns,
           '1' after 70 ns, '0' after 80 ns;
    reset <= '1' after 45 ns, '0' after 75 ns;
    trigger <= '1' after 10 ns, '0' after 20 ns,
               '1' after 30 ns, '0' after 40 ns;
    
    wait;
  end process stimulus_05_3_h_i_j;


end architecture test;
