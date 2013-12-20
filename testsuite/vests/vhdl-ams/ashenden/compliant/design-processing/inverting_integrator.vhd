
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

library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inverting_integrator is 
  port ( terminal input, output : electrical;
         signal rst : in std_ulogic );
end entity inverting_integrator;

----------------------------------------------------------------

architecture structural of inverting_integrator is
  terminal internal : electrical;
begin
  
  r1 : entity work.resistor(ideal)
    port map ( node1 => input, node2 => internal);
  
  c1 : entity work.capacitor(leakage)
    port map ( node1 => internal, node2 => output );
  
  amp : entity work.opamp(slew_limited)
    port map ( plus_in => electrical_ref, minus_in => internal,
               output => output);
  
  switch : entity work.analog_switch(ideal)
    port map ( n1 => internal, n2 => output, control => rst );
  
end architecture structural;
