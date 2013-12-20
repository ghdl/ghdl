
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

entity inline_04a is

end entity inline_04a;


architecture test of inline_04a is
begin

  block_1 : block is

    -- code from book

    subtype voltage is real tolerance "low_voltage";
    subtype current is real tolerance "low_current";
    nature electrical is voltage across current through electrical_ref reference;
    terminal anode, cathode : electrical;

    --

    subtype illuminance is real tolerance "default_illuminance";
    subtype optic_flux is real tolerance "default_optic_flux";
    nature radiant is illuminance across optic_flux through radiant_ref reference;
    terminal light_bulb, light_emitting_diode : radiant;

    --

    nature electrical_vector is array (natural range <>) of electrical;
    terminal a_bus : electrical_vector(1 to 8);

    --

    quantity light_illuminance across light_bulb;
    quantity LED_flux through light_emitting_diode;

    -- end code from book

    terminal n1, n2 : electrical;

    -- code from book

    quantity voltage_drop across
             inductive_current, capacitive_current, resistive_current through
             n1 to n2;
 
    -- end code from book

  begin
  end block block_1;



  block_2 : block is

    subtype voltage is real tolerance "low_voltage";
    subtype current is real tolerance "low_current";
    nature electrical is voltage across current through electrical_ref reference;

    -- code from book

    terminal anode, cathode : electrical;

    --

    quantity battery_voltage across battery_current through anode to cathode;
    quantity leakage_voltage across leakage_current through anode;

    -- end code from book

  begin
  end block block_2;

end architecture test;
