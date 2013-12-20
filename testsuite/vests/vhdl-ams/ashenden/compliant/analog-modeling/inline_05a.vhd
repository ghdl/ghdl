
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

entity inline_05a is

end entity inline_05a;


architecture test of inline_05a is

begin

  block_1 : block is

    constant cap : real := 1.0e-9;
    constant rleak : real := 1.0E6;

    -- code from book

    terminal p1, p2 : electrical;
    quantity vcap across icap, ileak through p1 to p2;

    -- end code from book

  begin

    -- code from book

    icap == cap * vcap'dot;
  
    ileak == vcap / rleak;

    -- end code from book

  end block block_1;


  block_2 : block is

    -- code from book

    nature electrical_vector is array (natural range <>) of electrical;
    terminal a_bus : electrical_vector(1 to 8);
    terminal signal_ground : electrical;

    --

    quantity bus_drops across bus_currents through a_bus to signal_ground;

    --

    terminal p1 : electrical_vector(0 to 3);
    terminal p2 : electrical;
  
    quantity v across i through p1 to p2;

    --

    constant tc1 : real := 1.0e-3; -- Linear temperature coefficient
    constant tc2 : real := 1.0e-6; -- Second-order temperature coefficient
    constant temp : real := 27.0; -- Ambient temperature
    constant tnom : real := 50.0; -- Nominal temperature
    constant res : real_vector := (1.0e3, 2.0e3, 4.0e3, 8.0e3); -- Nominal resistances

    --

    constant res_factor : real := (1.0 + tc1*(temp-tnom) + tc2*(temp-tnom)**2);

    -- end code from book

  begin

    -- code from book

    v(0) == i(0) * res(0) * res_factor;
    v(1) == i(1) * res(1) * res_factor;
    v(2) == i(2) * res(2) * res_factor;
    v(3) == i(3) * res(3) * res_factor;

    -- end code from book

  end block block_2;

end architecture test;
