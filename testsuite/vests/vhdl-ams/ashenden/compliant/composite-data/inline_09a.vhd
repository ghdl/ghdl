
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

entity inline_09a is

end entity inline_09a;


----------------------------------------------------------------


library ieee_proposed;  use ieee_proposed.electrical_systems.all;

architecture test of inline_09a is

  -- code from book:

  nature electrical_vector is array (natural range <>) of electrical;

  terminal local_bus : electrical_vector(15 downto 0);
  
  subnature long_bus is electrical_vector(7 downto 0);
  terminal remote_bus : long_bus;
    
  -- end of code from book

begin
end architecture test;
