
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

entity inline_01a is

end entity inline_01a;


architecture test of inline_01a is

  quantity capacitor_voltage : real;
  constant capacitance : real := 1.0e-9;

  subtype current is real;
  
  -- code from book

  subtype charge is real tolerance "default_charge";
  quantity capacitor_charge : charge;

  --

  quantity engine_power : real tolerance "approximate_power";

  --

  quantity I_sense : current := 0.15; -- initial value is 150mA

  --

  quantity amplifier_gains : real_vector (3 downto 0) := (1.0, 1.0, 1.0, 0.5);

  -- end code from book

begin

  -- code from book

  capacitor_charge == capacitor_voltage * capacitance;

  -- end code from book

end architecture test;
