
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
-- $Id: ap_a_ap_a_01.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ap_a_01 is

end entity ap_a_01;


library ieee;  use ieee.std_logic_1164.all;

architecture test of ap_a_01 is

  signal clk : std_ulogic;

begin

  process (clk) is

                  -- code from book

                  -- end code from book

  begin

    if

      -- code from book

      clk'event and (To_X01(clk) = '1') and (To_X01(clk'last_value) = '0')

      -- end code from book

    then
      report "rising edge on clk";
    end if;

  end process;

  clk <= '0', '1' after 10 ns, '0' after 20 ns,
         '1' after 30 ns, '0' after 40 ns;

end architecture test;

