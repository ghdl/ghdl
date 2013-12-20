
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

entity notch_filter is
  port ( terminal input, output, vdd, vss, gnd : electrical );
end entity notch_filter;

----------------------------------------------------------------

architecture opamp_based of notch_filter is
  
  component simple_opamp is
    port ( terminal plus_in, minus_in, output, vdd, vss, gnd : electrical );
  end component simple_opamp;
  -- ...
  
  terminal opamp1_in, opamp1_out, opamp2_in, -- ...
    -- not in book
    other_terminal
    -- end not in book
    : electrical;

begin
  
  opamp1 : component simple_opamp
    port map ( plus_in => gnd, minus_in => opamp1_in, output => opamp1_out,
               vdd => vdd, vss => vss, gnd => gnd );

    opamp2 : component simple_opamp
      port map ( plus_in => gnd, minus_in => opamp2_in, output => output,
                 vdd => vdd, vss => vss, gnd => gnd );

  -- other component instances
  -- ...
  
end architecture opamp_based;
