
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

library IEEE; use IEEE.std_logic_1164.all;
library IEEE_proposed; use IEEE_proposed.electrical_systems.all;

entity tb_2in_switch is
end tb_2in_switch;

architecture TB_2in_switch of tb_2in_switch is
    -- Component declarations
    -- Signal declarations
    terminal p_in1, p_in2, p_out : electrical;
	signal ctl_ulogic : std_ulogic;
    signal ctl_logic : std_logic;
begin
    -- Signal assignments
	ctl_ulogic <= To_X01(ctl_logic); -- Convert X01Z to X01
    -- Component instances
    vdc1 : entity work.v_constant(ideal)
        generic map(
            level => 1.0
        )
        port map(
            pos => p_in1,
            neg => ELECTRICAL_REF
        );
    vdc2 : entity work.v_constant(ideal)
        generic map(
            level => 3.0
        )
        port map(
            pos => p_in2,
            neg => ELECTRICAL_REF
        );
    Clk1 : entity work.clock(ideal)
        generic map(
            period => 10.0ms
        )        
		port map(
            clk_out => ctl_logic
        );
    R1 : entity work.resistor(ideal)
        generic map(
            res => 100.0
        )        
		port map(
            p1 => p_out,
            p2 => electrical_ref
        );
    swtch : entity work.switch_dig_2in(ideal)
        port map(
            p_in1 => p_in1,
            p_in2 => p_in2,
			p_out => p_out,
			sw_state => ctl_ulogic
        );
end TB_2in_switch;
