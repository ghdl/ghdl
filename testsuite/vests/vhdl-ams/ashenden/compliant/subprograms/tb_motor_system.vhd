
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

entity tb_motor_system is
end tb_motor_system ;

architecture TB_motor_system of tb_motor_system is
    -- Component declarations
    -- Signal declarations
	terminal in_src, x1_out, x2_out, x3_out : electrical;

begin
    v7 : entity work.v_sine(ideal)
        generic map(
            freq => 10.0,
            amplitude => 1.0
        )
        port map(
            pos => in_src,
			neg => electrical_ref
        );
	state_var1: entity work.motor_system_wa(simple)
		port map(
            vp => in_src,
            vm => ELECTRICAL_REF,
			px1 => x1_out,
			px2 => x2_out,
			px3 => x3_out
        );
end TB_motor_system ;


