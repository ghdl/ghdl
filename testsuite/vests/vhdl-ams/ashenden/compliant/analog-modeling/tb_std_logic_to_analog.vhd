
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

library IEEE; use IEEE.std_logic_1164.all; use IEEE.std_logic_arith.all;
library IEEE_proposed; use IEEE_proposed.electrical_systems.all;

entity tb_std_logic_to_analog is
end tb_std_logic_to_analog;

architecture TB_std_logic2analog of tb_std_logic_to_analog is
    -- Component declarations
    -- Signal declarations
	terminal ana_out : electrical ;
    signal ina : std_logic ;

begin
    -- Signal assignments
    -- Component instances
    d2a1 : entity work.std_logic_to_analog(ideal)
        port map(
           d => ina,			-- bit type pin
           a => ana_out
        );
    clk1 : entity work.clock_duty(ideal)
        generic map(
            off_time => 2 ms,
            on_time => 1 ms
        )
        port map(
            CLOCK_OUT => ina	-- std_logic type pin
        );
    R1 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => ana_out,
            p2 => electrical_ref
        );
end TB_std_logic2analog;
