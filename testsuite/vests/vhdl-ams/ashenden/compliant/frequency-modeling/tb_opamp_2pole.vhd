
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

entity tb_opamp_2pole is
end tb_opamp_2pole;

architecture TB_opamp_2pole of tb_opamp_2pole is
    -- Component declarations
    -- Signal declarations
    terminal in_src, op_neg2, out_opamp2 : electrical;
    terminal out_opamp1, op_neg1, out_opamp3_res, op_neg3_res : electrical;
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

    OP1 : entity work.opamp_2pole(dot)
    	port map(
            in_pos => electrical_ref,
            in_neg => op_neg1,
		output => out_opamp1
        );
    R1in : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => in_src,
            p2 => op_neg1
        );
    R1F : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => op_neg1,
            p2 => out_opamp1
        );
    Rload1 : entity work.resistor(ideal)
        generic map(
            res => 1.0e3
        )
        port map(
            p1 => out_opamp1,
            p2 => electrical_ref
        );
	OP2 : entity work.opamp_2pole(ltf)
    	port map(
            in_pos => electrical_ref,
            in_neg => op_neg2,
		output => out_opamp2
        );
    R2in : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => in_src,
            p2 => op_neg2
        );
    R2F : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => op_neg2,
            p2 => out_opamp2
        );
    Rload2 : entity work.resistor(ideal)
        generic map(
            res => 1.0e3
        )
        port map(
            p1 => out_opamp2,
            p2 => electrical_ref
        );
	OP3R : entity work.opamp_2pole_res(ltf)
    	port map(
            in_pos => electrical_ref,
            in_neg => op_neg3_res,
			output => out_opamp3_res
        );
    Rin3R : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => in_src,
            p2 => op_neg3_res
        );
    R3F : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => op_neg3_res,
            p2 => out_opamp3_res
        );
    Rload3R : entity work.resistor(ideal)
        generic map(
            res => 1.0e3
        )
        port map(
            p1 => out_opamp3_res,
            p2 => electrical_ref
        );
end TB_opamp_2pole;
