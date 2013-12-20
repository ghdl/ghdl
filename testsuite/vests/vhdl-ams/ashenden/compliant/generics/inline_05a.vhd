
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
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_05a is
end entity inline_05a;



architecture test of inline_05a is

  signal start_n, reset, time_out : std_ulogic;
  terminal interval_rc : electrical;

begin

  -- code from book (in text)

  interval_timer : entity work.timer(behavioral)
    generic map ( threshold => 2.5,
                  clamp_on_resistance => 0.01,
                  clamp_off_resistance => 10.0E+6 )
    port map ( trigger_n => start_n, reset => reset, q => time_out,
               rc_ext => interval_rc );

  -- end code from book

end architecture test;
