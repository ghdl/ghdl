
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

entity NMOS_transistor_wa is
  port ( terminal gate, drain, source : electrical );
end entity NMOS_transistor_wa;

----------------------------------------------------------------

architecture noisy of NMOS_transistor_wa is
  
  quantity vgs across igs through gate to source;
  quantity vds across ids through drain to source;
  quantity vsd across source to drain;
  quantity vgd across igd through gate to drain;
  constant threshold_voltage : voltage := 1.0;
  constant k : real := 1.0e-5;
  -- declare quantity in frequency domain for AC analysis
        
begin

  if vds >= 0.0 use  -- transistor is forward biased
    if vgs < threshold_voltage use  -- cutoff region
      ids == 0.0;
    elsif vds > vgs - threshold_voltage use  -- saturation region
      ids == 0.5 * k * (vgs - threshold_voltage)**2;
    else  -- linear/triode region
      ids == k * (vgs - threshold_voltage - 0.5*vds) * vds;
    end use;
  else  -- transistor is reverse biased
    if vgd < threshold_voltage use  -- cutoff region
      ids == 0.0;
    elsif vsd > vgd - threshold_voltage use  -- saturation region
      ids == -0.5 * k * (vgd - threshold_voltage)**2;
    else  -- linear/triode region
      ids == -k * (vgd - threshold_voltage - 0.5*vsd) * vsd;
    end use;
  end use;

  igs == 0.0;
  igd == 0.0;

end architecture noisy;
