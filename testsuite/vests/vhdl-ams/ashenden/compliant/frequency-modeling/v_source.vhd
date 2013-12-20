
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

library ieee;  use ieee.math_real.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity v_source is
  generic ( DC : voltage := 1.0;        -- output peak amplitude
            ac_mag : voltage := 1.0;    -- AC magnitude
            ac_phase : real := 0.0 );   -- AC phase [degree]
  port ( terminal pos, neg : electrical );
end entity v_source;

----------------------------------------------------------------

architecture behavior of v_source is
  
  quantity vout across iout through pos to neg;
  -- declare quantity in frequency domain for AC analysis
  quantity ac_spec : real spectrum ac_mag, math_2_pi*ac_phase/360.0;
        
begin

  if domain = quiescent_domain or domain = time_domain use
    vout == DC;
  else
    vout == ac_spec;  -- used for frequency (AC) analysis
  end use;

end architecture behavior;
