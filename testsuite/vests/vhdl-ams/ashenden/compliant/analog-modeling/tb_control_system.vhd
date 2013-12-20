
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

entity tb_control_system is
end tb_control_system;

architecture TB_control_system of tb_control_system is
    -- Component declarations
    -- Signal declarations
    quantity in_src, fb : real;
    quantity output : real;
begin
    -- Signal assignments
    -- Component instances
    src3 : entity work.src_sine(ideal)
        generic map(
            freq => 100.0,
            amplitude => 1.0
        )
        port map(
            output => in_src
        );
    XCMP12 : entity work.control_system(simple_feedback)
        port map(
            target => in_src,
            output => output,
		feedback => fb
        );
	 gain1 : entity work.gain(simple)
        generic map(
            k => 1.0
        )
	  port map (
		input => output,
		output => fb
		);
end TB_control_system;
