
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

entity inline_16 is

end entity inline_16;


----------------------------------------------------------------


architecture test of inline_16 is

  constant Tpd_01 : time := 800 ps;
  constant Tpd_10 : time := 500 ps;

  signal a, z : bit;

begin


  -- code from book:

  asym_delay : z <= transport a after Tpd_01 when a = '1' else
                              a after Tpd_10;

  -- end of code from book


  ----------------


  stimulus : process is
  begin
    a <= '1' after 2000 ps,
         '0' after 4000 ps,
         '1' after 6000 ps,
         '0' after 6200 ps;

    wait;
  end process stimulus;


end architecture test;
