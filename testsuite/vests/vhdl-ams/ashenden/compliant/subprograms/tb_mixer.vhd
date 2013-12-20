
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

entity tb_mixer is
end tb_mixer;

architecture TB_mixer of tb_mixer is
    -- Component declarations
    -- Signal declarations
    terminal mix_in : electrical_vector(1 to 8);
    terminal pseudo_gnd : electrical;
begin
    -- Signal assignments
    -- Component instances
    v3 : entity work.v_sine(ideal)
        generic map(
            amplitude => 5.0,
            freq => 1.0e3
        )
        port map(
            pos => mix_in(7),
            neg => ELECTRICAL_REF
        );
    v4 : entity work.v_sine(ideal)
        generic map(
            amplitude => 4.0,
            freq => 2.0e3
        )
        port map(
            pos => mix_in(8),
            neg => ELECTRICAL_REF
        );
    v9 : entity work.v_sine(ideal)
        generic map(
            freq => 1.0e3,
            amplitude => 5.0
        )
        port map(
            pos => mix_in(5),
            neg => ELECTRICAL_REF
        );
    v10 : entity work.v_sine(ideal)
        generic map(
            freq => 2.0e3,
            amplitude => 4.0
        )
        port map(
            pos => mix_in(6),
            neg => ELECTRICAL_REF
        );
    R2 : entity work.resistor(ideal)
        generic map(
            res => 1.0e3
        )
        port map(
            p1 => pseudo_gnd,
            p2 => ELECTRICAL_REF
        );
    mixer1 : entity work.mixer_wa(weighted)
        port map(
            inputs => mix_in,
            output => pseudo_gnd
        );
    v14 : entity work.v_sine(ideal)
        generic map(
            amplitude => 4.0,
            freq => 2.0e3
        )
        port map(
            pos => mix_in(2),
            neg => ELECTRICAL_REF
        );
    v15 : entity work.v_sine(ideal)
        generic map(
            amplitude => 5.0,
            freq => 1.0e3
        )
        port map(
            pos => mix_in(1),
            neg => ELECTRICAL_REF
        );
    v16 : entity work.v_sine(ideal)
        generic map(
            freq => 2.0e3,
            amplitude => 4.0
        )
        port map(
            pos => mix_in(4),
            neg => ELECTRICAL_REF
        );
    v17 : entity work.v_sine(ideal)
        generic map(
            freq => 1.0e3,
            amplitude => 5.0
        )
        port map(
            pos => mix_in(3),
            neg => ELECTRICAL_REF
        );
end TB_mixer;
