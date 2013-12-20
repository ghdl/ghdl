
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

entity amp_lim is
  port ( terminal ps : electrical;               -- positive supply terminal
         terminal input, output : electrical );
end entity amp_lim;

----------------------------------------------------------------

architecture simple of amp_lim is

  quantity v_pwr across i_pwr through ps to electrical_ref;
  quantity vin across iin through input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  quantity v_amplified : voltage ;
  constant gain : real := 1.0;

begin

  v_amplified == gain * vin;

  if v_amplified'above(v_pwr) use 
    vout == v_pwr;
  else 
    vout == v_amplified;
  end use;

  break on v_amplified'above(v_pwr);

  -- ignore loading effects
  i_pwr == 0.0;
  iin == 0.0;

end architecture simple;
