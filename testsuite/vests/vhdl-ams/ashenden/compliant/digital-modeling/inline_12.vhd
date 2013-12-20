
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

entity inline_12 is

end entity inline_12;


----------------------------------------------------------------


architecture test of inline_12 is

  signal top_a, bottom_a : bit := '0';
  signal top_y, bottom_y : bit;

begin


  block_3_m : block is
    port ( a : in bit;  y : out bit := '1' );
    port map ( a => top_a, y => top_y );

  begin

    -- code from book:

    inv : process (a) is
    begin
      y <= inertial not a after 3 ns;
    end process inv;

    -- end of code from book

  end block block_3_m;


  ----------------


  block_3_n : block is
    port ( a : in bit;  y : out bit := '1' );
    port map ( a => bottom_a, y => bottom_y);

  begin

    -- code from book:

    inv : process (a) is
    begin
      y <= reject 2 ns inertial not a after 3 ns;
    end process inv;

    -- end of code from book

  end block block_3_n;


  ----------------


  stimulus_3_m_n : process is
  begin
    top_a <= '1' after 1 ns,
             '0' after 6 ns,
             '1' after 8 ns;
    bottom_a <= '1' after 1 ns,
                '0' after 6 ns,
                '1' after 9 ns,
                '0' after 11.5 ns,
                '1' after 16 ns,
                '0' after 18 ns,
                '1' after 19 ns,
                '0' after 20 ns;

    wait;
  end process stimulus_3_m_n;


end architecture test;
