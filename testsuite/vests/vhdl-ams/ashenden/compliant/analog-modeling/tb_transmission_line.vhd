
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

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity tb_transmission_line is

end tb_transmission_line;

architecture TB_transmission_line of tb_transmission_line is
    quantity in_src, line_out : voltage;
    -- Component declarations
    -- Signal declarations
begin
    -- Signal assignments
    -- Component instances
    q1 : entity work.src_pulse(ideal)
        generic map(
            initial => 0.0,
            pulse => 1.0e1,
			ti2p => 1.0e-12,
			tp2i => 1.0e-12,
			delay => 1 ps,
			width => 20 ns,
			period => 50 ns
        )
        port map(
            output => in_src
        );
    
    T1 : entity work.transmission_line_wa(abstract)
        port map(
            vin => in_src,
            vout => line_out
        );
 
end TB_transmission_line;


