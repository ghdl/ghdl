
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

entity inline_15 is

end entity inline_15;


----------------------------------------------------------------


architecture test of inline_15 is
begin

  -- code from book:

  erroneous : process is
    variable i, j : integer;
  begin
    i := loop_param;          -- error!
    for loop_param in 1 to 10 loop
      loop_param := 5;        -- error!
    end loop;
    j := loop_param;          -- error!
  end process erroneous;

  -- end of code from book


end architecture test;
