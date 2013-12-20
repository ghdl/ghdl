
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

entity inline_10 is

end entity inline_10;


----------------------------------------------------------------


architecture test of inline_10 is
begin


  process is

    use std.textio.all;
    variable L : line;

    -- code from book:

    type speed_category is (stopped, slow, fast, maniacal);
    variable speed : speed_category;

    -- end of code from book

  begin

    speed := stopped;

    -- code from book:

    write ( L, speed_category'image(speed) );

    -- end of code from book

    writeline(output, L);

    speed := slow;
    write ( L, speed_category'image(speed) );
    writeline(output, L);
    speed := fast;
    write ( L, speed_category'image(speed) );
    writeline(output, L);
    speed := maniacal;
    write ( L, speed_category'image(speed) );
    writeline(output, L);

    -- code from book:

    readline( input, L );
    speed := speed_category'value(L.all);

    -- end of code from book

    wait;
  end process;


end architecture test;
