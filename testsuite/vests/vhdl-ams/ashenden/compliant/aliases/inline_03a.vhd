
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
                        
entity inline_03a is

end entity inline_03a;


architecture test of inline_03a is

  -- code from book

  nature electrical_bus is
    record
      strobe : electrical;
      databus : electrical_vector(0 to 7);
    end record;
  terminal ebus : electrical_bus;
  quantity bus_voltages across ebus to ground;

  --

  alias e_strobe is bus_voltages.strobe;
  alias e_data is bus_voltages.databus;

  -- end code from book

begin

end architecture test;
