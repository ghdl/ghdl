
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
                        
entity battery is
  port ( terminal anode, cathode : electrical );
end entity battery;



library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity ADC is
  port ( terminal a : electrical;
         signal d : out bit );
end entity ADC;




library ieee_proposed;
use ieee_proposed.electrical_systems.all, ieee_proposed.thermal_systems.all;

entity diode_thermal is
  port ( terminal p, m : electrical;
         terminal j : thermal );
end entity diode_thermal;
