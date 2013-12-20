
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

library ieee;  use ieee.math_real.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
use ieee_proposed.energy_systems.all;


entity inline_02a is

end entity inline_02a;


architecture test of inline_02a is

  constant k_Boltzmann : real := K;
  constant temp : real := 300.0;
  constant res : real := 10_000.0;
  terminal r_p1, r_p2 : electrical;
  quantity resistor_voltage across resistor_current through r_p1 to r_p2;

  constant k_noise : real := 1.0;

  function G ( f : real ) return real is
  begin
    return 1.0;
  end function G;

  constant k_flicker : real := 1.0;
  constant ids : real := 0.01;
  constant af : real := 1.0;

  -- code from book

  quantity thermal_noise_source : real noise 4.0 * k_Boltzmann * temp * res;

  --
  
  quantity shaped_noise_source : real noise k_noise * temp * G(frequency);
  
  --
  
  quantity flicker_noise_source : real noise k_flicker * ids**af / frequency;
  
  -- end code from book

begin

  -- code from book

  resistor_voltage == resistor_current * res + thermal_noise_source;

  -- end code from book

end architecture test;
