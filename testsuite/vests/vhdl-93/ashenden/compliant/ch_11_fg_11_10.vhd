
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
-- $Id: ch_11_fg_11_10.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of bus_module is
begin

  behavior : process is
                       -- . . .
                       -- not in book
                       constant Tdelay_synch : delay_length := 10 ns;
                     constant wait_delay : delay_length := 100 ns;
                     -- end not in book
  begin
    synch <= '0'  after Tdelay_synch;
    -- . . .
    -- not in book
    wait for wait_delay;
    -- end not in book
    -- ready to start operation
    synch <= 'Z' after Tdelay_synch;
    wait until synch = 'H';
    -- . . .    -- proceed with operation
    -- . . .
  end process behavior;

end architecture behavioral;
