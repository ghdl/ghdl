
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_08a is

end entity inline_08a;


architecture test of inline_08a is

  -- code from book

  terminal bias_node : electrical;

  --

  subnature accurate_electrical is electrical
    tolerance "accurate_voltage" across "accurate_current" through;

  --

  terminal n1, n2 : accurate_electrical;

  --

  quantity n1_n2_voltage across n1_n2_current through n1 to n2;

  --

  quantity internal_voltage : voltage tolerance n1_n2_voltage'tolerance;
  quantity internal_current : current tolerance n1_n2_current'tolerance;

  --

  terminal bus_a_end, bus_b_end : electrical_vector(15 downto 0);
  quantity bus_currents through bus_a_end to bus_b_end;

  -- end code from book

begin

  -- code from book

  bias_node'reference == 0.5;

  -- end code from book
  
end architecture test;
