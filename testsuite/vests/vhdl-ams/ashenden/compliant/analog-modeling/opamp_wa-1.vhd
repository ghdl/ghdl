
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
                        
entity opamp_wa is
  port ( terminal positive_supply, negative_supply : electrical;
         terminal plus_in, minus_in, output : electrical );
end entity opamp_wa;

----------------------------------------------------------------

architecture saturating of opamp_wa is
  
  constant gain : real := 50.0;
  quantity v_pos := 15.0 across positive_supply;
  quantity v_neg := -15.0 across negative_supply;
  quantity v_in across plus_in to minus_in;
  quantity v_out across i_out through output;
  quantity v_amplified : voltage;
        
begin

  if v_in'above(v_pos / gain) use 
    v_amplified == v_pos;
  elsif not v_in'above(v_neg / gain) use
    v_amplified == v_neg;
  else 
    v_amplified == gain * v_in;
  end use;

  break on v_in'above(v_pos/gain), v_in'above(v_neg/gain);
  
  v_out == v_amplified; -- 'slew(1.0e6,-1.0e6);
  
end architecture saturating;
