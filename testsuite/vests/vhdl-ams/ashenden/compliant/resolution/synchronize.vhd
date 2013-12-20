
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

library ieee;  use ieee.std_logic_1164.all;

package synchronize is

  procedure init_synchronize ( signal synch : out std_logic );

  procedure begin_synchronize ( signal synch : inout std_logic;
                                Tdelay : in delay_length := 0 fs );

  procedure end_synchronize ( signal synch : inout std_logic;
                              Tdelay : in delay_length := 0 fs );

end package synchronize;



package body synchronize is

  -- code from book

  procedure init_synchronize ( signal synch : out std_logic ) is
  begin
    synch <= '0';
  end procedure init_synchronize;

  procedure begin_synchronize ( signal synch : inout std_logic;
                                Tdelay : in delay_length := 0 fs ) is
  begin
    synch <= 'Z' after Tdelay;
    wait until synch = 'H';
  end procedure begin_synchronize;

  procedure end_synchronize ( signal synch : inout std_logic;
                              Tdelay : in delay_length := 0 fs ) is
  begin
    synch <= '0' after Tdelay;
    wait until synch = '0';
  end procedure end_synchronize;

  -- end code from book

end package body synchronize;
