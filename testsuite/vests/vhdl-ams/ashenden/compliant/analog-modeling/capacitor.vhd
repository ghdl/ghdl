
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
                        
entity capacitor is 
  port ( terminal node1, node2 : electrical );
end entity capacitor;

architecture leakage of capacitor is
  constant c : real := 1.0E-6;
  constant r_leak : real := 10.0E6;
  quantity v_cap across i_cap, i_leak through node1 to node2;
begin
  i_cap == c * v_cap'dot; 
  i_leak == v_cap / r_leak;
end architecture leakage;
