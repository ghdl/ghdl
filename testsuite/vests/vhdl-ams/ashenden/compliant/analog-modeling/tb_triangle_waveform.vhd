
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
entity tb_triangle_waveform is
end tb_triangle_waveform;

architecture TB_triangle_waveform of tb_triangle_waveform is
    -- Component declarations
    -- Signal declarations
    terminal in_src : electrical;
begin
    -- Signal assignments
    -- Component instances
    vio : entity work.triangle_waveform_wa(ideal)
        port map(
            pos => in_src,
            neg => ELECTRICAL_REF
        );
    R1 : entity work.resistor(ideal)
        generic map(
            res => 10.0e9
        )
        port map(
            p1 => in_src,
            p2 => ELECTRICAL_REF
        );
end TB_triangle_waveform;
