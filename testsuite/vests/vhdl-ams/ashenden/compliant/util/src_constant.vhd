
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

-- Voltage Pulse Source (Includes Frequency Domain settings)

library ieee;  use ieee.math_real.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity src_constant is
  
  generic ( level : real := 1.0;       -- Constant output value (V)
            ac_mag : real := 1.0;      -- AC magnitude 
            ac_phase : real := 0.0 );  -- AC phase (degrees)

  port ( quantity output : out real );

end entity src_constant;


architecture ideal of src_constant is

  -- Declare quantity in frequency domain for AC analysis 
  quantity ac_spec : real spectrum ac_mag, math_2_pi * ac_phase / 360.0;

begin

  if domain = quiescent_domain or domain = time_domain use
    output == level;
  else	
    output == ac_spec;  -- used for frequency (AC) analysis
  end use;

end architecture ideal;
