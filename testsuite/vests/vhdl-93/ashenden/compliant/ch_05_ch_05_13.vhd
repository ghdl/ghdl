
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
-- $Id: ch_05_ch_05_13.vhd,v 1.1.1.1 2001-08-22 18:20:47 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity ch_05_13 is

end entity ch_05_13;


----------------------------------------------------------------


library ieee;  use ieee.std_logic_1164.all;

architecture test of ch_05_13 is

  signal s : std_ulogic;

begin


  process_05_3_o : process is
  begin
    s <= '1' after 11 ns,
         'X' after 12 ns,
         '1' after 14 ns,
         '0' after 15 ns,
         '1' after 16 ns,
         '1' after 17 ns,
         '1' after 20 ns,
         '0' after 25 ns;
    wait for 10 ns;

    -- code from book:

    s <= reject 5 ns inertial '1' after 8 ns;

    -- end of code from book

    wait;
  end process process_05_3_o;


end architecture test;
