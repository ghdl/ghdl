
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

library ieee, ieee_proposed;
use ieee.math_real.all;
use ieee_proposed.energy_systems.all;
use ieee_proposed.electrical_systems.all;
use ieee_proposed.thermal_systems.all;

entity diode is
  port ( terminal p, m : electrical;
         terminal j : thermal );
end entity diode;

----------------------------------------------------------------

architecture one of diode is
  
  constant area : real := 1.0e-3;
  constant Dn : real := 30.0; 		-- electron diffusion coefficient
  constant Dp : real := 15.0; 		-- hole diffusion coefficient
  constant np : real := 6.77e-5;	-- minority charge density
  constant pn : real := 6.77e-6;	-- minority charge density
  constant Ln : real := 5.47e-6;	-- diffusion length for electrons
  constant Lp : real := 12.25e-6;	-- diffusion length for holes
  quantity v across id through p to m;
  quantity vt : voltage := 1.0;		-- threshold voltage
  quantity temp across power through j;
        
begin
  
  vt == temp * K / Q;
  
  id == Q * area * (Dp * (pn / Lp) + Dn * (np / Ln)) * (exp(v / vt) - 1.0);

  power == v * id;
  
end architecture one;
