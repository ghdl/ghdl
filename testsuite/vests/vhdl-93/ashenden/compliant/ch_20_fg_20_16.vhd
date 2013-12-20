
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
-- $Id: ch_20_fg_20_16.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_20_16 is
end entity fg_20_16;


architecture test of fg_20_16 is

  signal clk : bit;

  attribute synthesis_hint : string;

begin

  -- code from book

  controller : process is

                         attribute synthesis_hint of control_loop : label is
                       "implementation:FSM(clk)";
                       -- . . .

  begin
    -- . . .    -- initialization
    control_loop : loop
      wait until clk = '1';
      -- . . .
    end loop;
  end process controller;

  -- end code fom book

end architecture test;
