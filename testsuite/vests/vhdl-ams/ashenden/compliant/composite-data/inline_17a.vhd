
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

entity inline_17a is

end entity inline_17a;


----------------------------------------------------------------


library ieee_proposed;  use ieee_proposed.electrical_systems.all;

architecture test of inline_17a is

  -- code from book:

  nature electrical_bus is record
      strobe : electrical;
      bus_lines : electrical_vector(0 to 15);
    end record electrical_bus;

  terminal address_bus, data_bus : electrical_bus;
  
  quantity data_voltages across data_currents through data_bus;

  -- end of code from book

begin
end architecture test;
