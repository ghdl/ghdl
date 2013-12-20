
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
-- $Id: ch_15_cg-b.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

architecture behavior of clock_gen is

  constant clock_period : delay_length := 2 * (Tpw + Tps);

begin

  reset_driver : 
    reset <= '1', '0' after 2.5 * clock_period + Tps;

  clock_driver : process is
  begin
    phi1 <= '0';
    phi2 <= '0';
    wait for clock_period / 2;
    loop
      phi1 <= '1', '0' after Tpw;
      phi2 <= '1' after clock_period / 2,
              '0' after clock_period / 2 + Tpw;
      wait for clock_period;
    end loop;
  end process clock_driver;

end architecture behavior;
