
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

library IEEE;
use IEEE.std_logic_1164.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity tb_lpf_dot_ltf_ztf is
end tb_lpf_dot_ltf_ztf;

architecture TB_lpf_dot_ltf_ztf of tb_lpf_dot_ltf_ztf is
    -- Component declarations
    -- Signal declarations
    terminal in_src : electrical;
    terminal out_dot, out_ltf, out_ztf1, out_ztf4, out_RC : electrical;
begin
    -- Signal assignments
    -- Component instances
    vio : entity work.v_sine(ideal)
        generic map(
            freq => 100.0,
            amplitude => 5.0
        )
        port map(
            pos => in_src,
            neg => ELECTRICAL_REF
        );

	RC1 : entity work.lowpass(RC)

		port map(
            input => in_src,
			output => out_RC
        );
	dot1 : entity work.lowpass(dot)

		port map(
            input => in_src,
			output => out_dot
        );
    ltf1 : entity work.lowpass(ltf)

    	port map(
            input => in_src,
			output => out_ltf
        );
    ztf1 : entity work.lowpass(ztf)
 
    	port map(
            input => in_src,
			output => out_ztf1
        );
    ztf4 : entity work.lowpass(z_minus_1)
 
    	port map(
            input => in_src,
			output => out_ztf4
        );
end TB_lpf_dot_ltf_ztf;
