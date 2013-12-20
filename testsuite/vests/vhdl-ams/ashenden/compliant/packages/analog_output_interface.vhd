
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

-- not in book


library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity analog_output_interface is
  port ( signal wr : in std_ulogic;
         signal data : std_ulogic_vector(7 downto 0);
         terminal analog_out : electrical );
end entity analog_output_interface;


----------------


library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity analog_interface_dac is
  port ( signal d_in : std_ulogic_vector(7 downto 0);
         terminal output : electrical;
         terminal plus_supply, minus_supply : electrical );
end entity analog_interface_dac;


architecture macroblock of analog_interface_dac is

begin

end architecture macroblock;

-- end not in book




architecture structural of analog_output_interface is
  
  -- This architecture implements the interface as a register connected to a DAC.
  -- NOTE: it uses the analog power supply terminals from clock_power_pkg
  -- to supply the DAC.
  
  signal register_out : -- . . .;
  -- not in book
    std_ulogic_vector(7 downto 0);
  -- end not in book

begin

  -- ...
  
  dac : entity work.analog_interface_dac(macroblock)
    port map ( d_in => register_out, output => analog_out,
               plus_supply => work.clock_power_pkg.analog_plus_supply,
               minus_supply => work.clock_power_pkg.analog_ground );
  
end architecture structural;
