
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

entity inline_13 is

end entity inline_13;


----------------------------------------------------------------


architecture test of inline_13 is
begin


  process_3_b : process is

    -- code from book:

    type array1 is array (1 to 100) of integer;
    type array2 is array (100 downto 1) of integer;

    variable a1 : array1;
    variable a2 : array2;

    -- end of code from book

  begin

    a1(11 to 20) := a1(11 to 20);
    a2(50 downto 41) := a2(50 downto 41);

    a1(10 to 1) := a1(10 to 1);
    a2(1 downto 10) := a2(1 downto 10);

    a1(10 downto 1) := a1(10 downto 1);  -- illegal
    a2(1 to 10) := a2(1 to 10);  -- illegal;

    wait;
  end process process_3_b;


end architecture test;
