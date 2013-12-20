
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
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity tb_quad_opamp is
end tb_quad_opamp ;

architecture TB_quad_opamp of tb_quad_opamp is
    -- Component declarations
    -- Signal declarations
    terminal amp_out : electrical_vector(1 to 4);
    terminal inm : electrical_vector(1 to 4);
    terminal inp : electrical_vector(1 to 4);
begin
    -- Signal assignments
    -- Component instances
    opamp_quad_slew1 : entity work.quad_opamp_wa(slew_limited)
        port map(
            n1 => inp,
            n2 => inm,
            output => amp_out
        );
    R4 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => amp_out(4)
        );
    v4 : entity work.v_pulse(ideal)
        generic map(
            period => 200 us,
            width => 100 us,
            delay => 10 us,
            tp2i => 0.9 us,
            ti2p => 0.70 us,
            pulse => 5.0
        )
        port map(
            pos => inm(1),
            neg => ELECTRICAL_REF
        );
    R5 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => amp_out(3)
        );
    R6 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => amp_out(2)
        );
    R7 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => amp_out(1)
        );
    v5 : entity work.v_pulse(ideal)
        generic map(
            pulse => 5.0,
            ti2p => 0.70 us,
            tp2i => 0.9 us,
            delay => 10 us,
            width => 100 us,
            period => 200 us
        )
        port map(
            pos => inm(2),
            neg => ELECTRICAL_REF
        );
    v6 : entity work.v_pulse(ideal)
        generic map(
            pulse => 5.0,
            ti2p => 0.70 us,
            tp2i => 0.9 us,
            delay => 10 us,
            width => 100 us,
            period => 200 us
        )
        port map(
            pos => inm(3),
            neg => ELECTRICAL_REF
        );
    v7 : entity work.v_pulse(ideal)
        generic map(
            pulse => 5.0,
            ti2p => 0.70 us,
            tp2i => 0.9 us,
            delay => 10 us,
            width => 100 us,
            period => 200 us
        )
        port map(
            pos => inm(4),
            neg => ELECTRICAL_REF
        );
    R8 : entity work.resistor(ideal)
        generic map(
            res => 10.0e-3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => inp(1)
        );
    R9 : entity work.resistor(ideal)
        generic map(
            res => 10.0e-3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => inp(2)
        );
    R10 : entity work.resistor(ideal)
        generic map(
            res => 10.0e-3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => inp(3)
        );
    R11 : entity work.resistor(ideal)
        generic map(
            res => 10.0e-3
        )
        port map(
            p1 => ELECTRICAL_REF,
            p2 => inp(4)
        );
end TB_quad_opamp ;
