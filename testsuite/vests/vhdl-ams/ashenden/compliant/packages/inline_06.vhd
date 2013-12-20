
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

-- code from book:

library std, work;  use std.standard.all;

-- end of code from book


entity inline_06 is

end entity inline_06;


----------------------------------------------------------------


architecture test of inline_06 is
begin


  process_4_a : process is

    constant a : integer := 10;
    constant b : integer := 20;
    variable result : boolean;

  begin

    -- code from book:

    result := std.standard."<" ( a, b );

    -- end of code from book

    wait;
  end process process_4_a;


end architecture test;
