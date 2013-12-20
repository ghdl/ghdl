
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
use ieee_proposed.fluidic_systems.all;

entity inline_02a is

end entity inline_02a;


----------------------------------------------------------------


architecture test of inline_02a is
begin


  block_1_a : block is

    -- code from book:

    nature electrical_bus is array (0 to 31) of electrical;

    -- end of code from book

  begin
  end block block_1_a;


  block_1_b : block is

    -- code from book:

    nature electrical_bus is array (31 downto 0) of electrical;

    -- end of code from book

  begin
  end block block_1_b;


  block_1_c : block is

    -- code from book:

    type engine_nodes is (intake, compressor, combustion, exhaust);

    --

    nature engine_flows is array (intake to exhaust) of fluidic;
    
    --

    subtype bus_lines is integer range 0 to 31;
    nature electrical_bus is array (bus_lines) of electrical;

    --

    subtype pressure is real tolerance "default_pressure";
    subtype pipes is integer range 0 to 15;

    --

    type gas_pressures is array (pipes) of pressure;

    --

    terminal system_bus : electrical_bus;
    terminal ferrari_engine, chevy_engine : engine_flows;

    --

    quantity bus_voltages across bus_currents through
	     system_bus to electrical_ref;

    -- end of code from book

  begin
  end block block_1_c;


end architecture test;
