
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

library ieee_proposed;  use ieee_proposed.thermal_systems.all;

entity thermostat is
  port ( quantity sensor_temp : in temperature;
         signal desired_temp : in real;
         signal heater_on : out boolean );
end entity thermostat;

----------------------------------------------------

architecture example of thermostat is
begin

  controller : process ( desired_temp, 
                         sensor_temp'above(desired_temp + 2.0),
                         sensor_temp'above(desired_temp - 2.0) ) is
  begin
    if sensor_temp < desired_temp - 2.0 then
      heater_on <= true;
    elsif sensor_temp > desired_temp + 2.0 then
      heater_on <= false;
    end if;
  end process controller;

end architecture example;
