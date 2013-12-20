
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

entity clock_gen is
end entity clock_gen;

architecture test of clock_gen is

  constant T_pw : time := 10 ns;

  signal clk : bit;

begin

  -- code from book

  clock_gen : process is
  begin
    clk <= '1' after T_pw, '0' after 2*T_pw;
    wait for 2*T_pw;
  end process clock_gen;

  -- end code from book

end architecture test;
