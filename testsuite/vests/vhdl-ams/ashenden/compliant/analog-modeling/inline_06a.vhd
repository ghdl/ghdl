
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

entity inline_06a is

end entity inline_06a;


architecture test of inline_06a is

  -- code from book

  terminal a_bus : electrical_vector(1 to 8);
  terminal b_bus : electrical_vector(8 downto 1);

  --

  quantity a_to_b_drops across a_to_b_currents through a_bus to b_bus;

  --

  nature electrical_bus is
    record
      strobe: electrical;
      databus : electrical_vector(0 to 7);
    end record;
  
  terminal t1, t2 : electrical_bus;

  --

  quantity bus_voltages across t1 to t2;

  --

  terminal p1, p2 : electrical_vector(0 to 3);
  
  quantity v across i through p1 to p2;

  -- end code from book


begin

  block_1 : block is
                    
    terminal anode, cathode : electrical;

    -- code from book

    quantity battery_voltage tolerance "battery_tolerance" across
             battery_current tolerance "battery_tolerance" through anode to cathode;

    -- end code from book

  begin
  end block block_1;


  block_2 : block is
                    
    terminal anode, cathode : electrical;

    -- code from book

    quantity battery_volts := 5.0 across
             battery_amps := 0.0 through
             anode to cathode;

    -- end code from book

  begin
  end block block_2;


end architecture test;
