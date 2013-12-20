
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
-- $Id: ch_05_fg_05_12.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_05_12 is
end entity fg_05_12;



architecture test of fg_05_12 is

  signal a, z : bit;

begin

  -- code from book

  asym_delay : process (a) is
                             constant Tpd_01 : time := 800 ps;
                           constant Tpd_10 : time := 500 ps;
  begin
    if a = '1' then
      z <= transport a after Tpd_01;
    else  -- a = '0'
      z <= transport a after Tpd_10;
    end if;
  end process asym_delay;

  -- end code from book


  stimulus : process is
  begin
    a <= '1' after 2000 ps,
         '0' after 4000 ps,
         '1' after 6000 ps,
         '0' after 6200 ps;
    wait;
  end process stimulus;

end architecture test;
