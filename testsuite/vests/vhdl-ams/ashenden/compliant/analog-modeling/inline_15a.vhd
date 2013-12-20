
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

library ieee_proposed;
use ieee_proposed.electrical_systems.all;
use ieee_proposed.thermal_systems.all;
                        
entity inline_15a is

end entity inline_15a;


architecture test of inline_15a is

  -- code from book

  terminal bridge1, bridge2 : electrical;
  quantity ambient : temperature;

  -- end code from book

begin

  ambient == 300.0;

  -- code from book

  resistor1 : entity work.temperature_dependent_resistor(linear_approx)
    port map ( n1 => bridge1, n2 => bridge2, temp => ambient );

  -- end code from book

end architecture test;
