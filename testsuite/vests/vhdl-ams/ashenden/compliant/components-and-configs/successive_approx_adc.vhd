
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

library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity successive_approx_adc is
  generic ( t_setup, t_hold, t_pd : delay_length;
            width : positive );
  port ( terminal analog_in : electrical;
         signal clock : in std_logic;
         signal start : in std_logic;
         signal eoc : out std_logic;
         signal data_out : out std_logic_vector(0 to width - 1) );
end entity successive_approx_adc;


-- not in book

architecture struct of successive_approx_adc is

begin

end architecture struct;
