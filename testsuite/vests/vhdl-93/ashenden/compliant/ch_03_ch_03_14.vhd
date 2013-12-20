
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
-- $Id: ch_03_ch_03_14.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ch_03_14 is
end entity ch_03_14;

architecture test of ch_03_14 is

  -- code from book:

  type controller_state is (initial, idle, active, error);

  -- end of code from book

  signal current_state : controller_state := initial;

begin

  process_3_4_c : process is
  begin

    -- code from book:

    for state in controller_state loop
      -- . . .
      -- not in book:
      current_state <= state;
      wait for 10 ns;
      -- end not in book
    end loop;

    -- end of code from book

    wait;
  end process process_3_4_c;

end architecture test;

