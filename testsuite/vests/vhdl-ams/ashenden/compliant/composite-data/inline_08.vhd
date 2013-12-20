
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

entity inline_08 is

end entity inline_08;


----------------------------------------------------------------


architecture test of inline_08 is
begin


  process_2_a : process is

    -- code from book:

    type sample is array (natural range <>) of integer;

    variable short_sample_buf : sample(0 to 63);

    subtype long_sample is sample(0 to 255);
    variable new_sample_buf, old_sample_buf : long_sample;

    constant lookup_table : sample := ( 1 => 23, 3 => -16, 2 => 100, 4 => 11);

    constant beep_sample : sample := ( 127, 63, 0, -63, -127, -63, 0, 63 );

    -- end of code from book

  begin
    wait;
  end process process_2_a;


end architecture test;
