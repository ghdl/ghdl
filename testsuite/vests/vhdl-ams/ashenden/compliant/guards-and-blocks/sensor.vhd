
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

entity sensor is

  generic ( threshold : real;		-- voltage threshold
            tipd_clk : delay_length;	-- input prop delay on clk
            tipd_input : real;	        -- input prop delay on sensor input
            topd_q : delay_length );	-- output prop delay on q

  port ( terminal input : electrical;   -- sensor analog input
         signal clk : in bit;	        -- edge–triggered clock input
         signal q : out bit );	        -- sensor digital output

end entity sensor;


architecture detailed_timing of sensor is

  quantity vin across input;	 -- analog input values
  quantity v_delayed : voltage;	 -- input voltage delayed
  signal clk_delayed : bit;	 -- clk input port delayed
  signal q_int : bit;		 -- q output with zero delay

begin

  input_port_delay : block is
  begin
    v_delayed == vin'delayed(tipd_input);
    clk_delayed <= clk'delayed(tipd_clk);
  end block input_port_delay;

  AD_conversion : block is
  begin
    q_int <= '1' when vin'above(threshold) else
             '0';
  end block AD_conversion;

  output_port_delay : block is
  begin
    q <= q_int'delayed(topd_q);
  end block output_port_delay;

end architecture detailed_timing;
