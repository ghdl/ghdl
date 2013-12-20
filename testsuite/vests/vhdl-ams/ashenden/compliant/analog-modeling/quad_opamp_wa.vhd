
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

entity quad_opamp_wa is
  port (terminal n1, n2, output : electrical_vector(1 to 4));
end entity quad_opamp_wa ;

----------------------------------------------------------------

architecture slew_limited of quad_opamp_wa is
  
  quantity vin across n1 to n2;
  quantity vout across iout through output;
  quantity vamp1 : real;
  quantity vamp2 : real;
  quantity vamp3 : real;
  quantity vamp4 : real;
  constant gain : real := 50.0;
        
begin

  vamp1 == gain*vin(1);	
  vamp2 == gain*vin(2);
  vamp3 == gain*vin(3);
  vamp4 == gain*vin(4);

  vout(1) == vamp1'slew(1.0e6,-1.0e6);
  vout(2) == vamp2'slew(1.0e6,-1.0e6);
  vout(3) == vamp3'slew(1.0e6,-1.0e6);
  vout(4) == vamp4'slew(1.0e6,-1.0e6);

end architecture slew_limited ;
