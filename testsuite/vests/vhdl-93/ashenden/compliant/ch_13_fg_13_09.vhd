
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
-- $Id: ch_13_fg_13_09.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

-- not in book

entity alarm_clock is
end entity alarm_clock;

-- end not in book


architecture top_level of alarm_clock is

  --use work.counter_types.digit;
  use work.counter_types.all;

  signal reset_to_midnight, seconds_clk : bit;
  signal seconds_units, seconds_tens : digit;
  -- . . .

begin

  seconds : configuration work.counter_down_to_gate_level
    port map ( clk => seconds_clk, clr => reset_to_midnight,
               q0 => seconds_units, q1 => seconds_tens );

  -- . . .

  -- not in book

  clk_gen : seconds_clk <= not seconds_clk after 20 ns;

  clr_gen : reset_to_midnight <= '1' after 95 ns,
		                 '0' after 135 ns;

  -- end not in book;

end architecture top_level;
