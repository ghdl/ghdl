
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
use ieee_proposed.fluidic_systems.all, ieee_proposed.mechanical_systems.all;

package automotive_valve_defs is
  
  subnature valve_fluidic is fluidic
    tolerance "valve_pressure" across "valve_vflow_rate" through;

  subnature valve_translational is translational
    tolerance "valve_displacement" across "valve_force" through;

  -- ...  -- other useful declarations
  
  component automotive_valve is
    port ( terminal p1, p2 : valve_fluidic;
           terminal control : valve_translational );
  end component automotive_valve;

end package automotive_valve_defs;
