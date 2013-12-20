
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
  generic ( cap : capacitance;
            r_esr : resistance := 0.0;
            v_ic : voltage := real'low );
  port ( terminal p1, p2 : electrical );
end entity capacitor;

----------------------------------------------------------------

architecture esr of capacitor is
  
  quantity v across i through p1 to p2;
  quantity vc : voltage;  -- Internal voltage across capacitor
  
begin
  
  if domain = quiescent_domain and v_ic /= real'low use
    vc == v_ic;
    i == 0.0;
  else
    vc == v - (i * r_esr); 
    i == cap * vc'dot;
  end use;
  
end architecture esr;
