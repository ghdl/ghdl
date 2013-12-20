
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
use ieee_proposed.mechanical_systems.all;
use ieee_proposed.electrical_systems.all;

entity propulsion is
  port ( signal clk, reset : in bit;    -- control inputs
         signal rpm : in natural;       -- requested rpm
         signal forward : in bit );     -- requested direction
end entity propulsion;

architecture mixed of propulsion is
  terminal p1, p2 : electrical;
  terminal shaft1, shaft2, shaft3 : rotational_v;
  signal forward_gear : bit;
  -- ...
begin

  motor : entity work.dc_motor(ideal)
    port map ( p1, p2, shaft1 );

  gear : entity work.gear_av(ideal)
    port map ( forward_gear, shaft1, shaft2 );

  intertia : entity work.inertia_av(ideal)
    port map ( shaft2, shaft3 );

  prop : entity work.propeller(ideal)
    port map ( shaft3 );

  control_section : process is
    -- variable declarations for control_section to control voltage inputs
    -- and gear shifting
    -- ...
  begin
    -- ...
    wait on clk, reset;
  end process control_section;

  -- ...

end architecture mixed;
