
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
                        
entity sensor_interface is

end entity sensor_interface;

-- end not in book



architecture structural of sensor_interface is
  
  component adc is
    generic ( width : positive );
    port ( terminal analog_in : electrical;
           signal clock : in std_logic;
           signal start : in std_logic;
           signal eoc : out std_logic;
           signal data_out : out std_logic_vector(0 to width - 1) );
  end component adc;

  -- ...

  -- not in book
  terminal sensor_input : electrical;
  signal clk, start_conversion, end_conversion : std_logic;
  signal sensor_data : std_logic_vector(0 to 7);
  -- end not in book
  
begin
  
  sensor_adc : component adc
    generic map ( width => sensor_data'length )
    port map ( analog_in => sensor_input,
               clock => clk,
               start => start_conversion,
               eoc => end_conversion,
               data_out => sensor_data );

  -- ...
  
end architecture structural;
