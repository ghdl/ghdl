
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
                        
entity freq_detect is
  port ( terminal input : electrical;
         terminal freq_out : electrical );
end entity freq_detect;

----------------------------------------------------------------

architecture threshold_crossing of freq_detect is
  
  quantity v_in across input to electrical_ref;
  quantity v_out across i_out through freq_out to electrical_ref;
  signal freq : real := 0.0;
  constant threshold : real := 0.0;
  constant scale_factor : real := 1.0e-6;
        
begin
  
  detect: process ( v_in'above(threshold) ) is
    variable t_previous : real := real'low;
  begin
    if v_in > threshold then
      freq <= scale_factor / ( now - t_previous );
      t_previous := now;
    end if;
  end process detect;
                
  v_out == freq'ramp(1.0e-9, 1.0e-9);
                
end threshold_crossing;
