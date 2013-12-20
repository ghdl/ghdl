
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
use ieee_proposed.electrical_systems.all;
use ieee_proposed.mechanical_systems.all;
                        
entity inline_21a is

end entity inline_21a;


architecture test of inline_21a is

  -- code from book

  quantity d : displacement;
  
  limit d : displacement with 0.001;

  --

  quantity drive_shaft_av, axle_av, wheel_av : angular_velocity;

  --

  limit drive_shaft_av, axle_av, wheel_av : angular_velocity with 0.01;

  --

  limit all : angular_velocity with 0.01;

  --

  quantity input, preamp_out, mixer_out, agc_out : voltage;
  
  limit input, preamp_out : voltage with 1.0E-9;
  limit others : voltage with 1.0E-7;

  --

  terminal bus1 : electrical_vector(1 to 8);
  terminal bus2 : electrical_vector(1 to 8);
  quantity v_bus across bus1 to bus2;
  limit v_bus : voltage_vector with 1.0E-3;

  -- end code from book

begin

end architecture test;
