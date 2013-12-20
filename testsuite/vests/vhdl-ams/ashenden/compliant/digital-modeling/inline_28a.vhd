
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

entity inline_28a is

end entity inline_28a;


----------------------------------------------------------------


architecture test of inline_28a is

  quantity disp : real;
  constant min_high : real := 2.5;

begin


  process_3_h : process is
  begin

    -- code from book:

    wait until disp'above(min_high) for 2.0;

    -- end of code from book

    wait;
  end process process_3_h;


end architecture test;
