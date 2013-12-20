
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

entity tb_v_source is

end tb_v_source ;

architecture TB_v_source of tb_v_source is
    terminal sin_out1, sin_out2 : electrical;
    -- Component declarations
    -- Signal declarations
begin
    -- Signal assignments
    -- Component instances
    v1 : entity work.v_source(behavior)
        port map(
            pos => sin_out1,
            neg => ELECTRICAL_REF
        );

    R1 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => sin_out1,
            p2 => electrical_ref
        );
    v2 : entity work.v_constant(ideal)
        generic map(
            level => 1.0
        )
   		port map(
            pos => sin_out2,
            neg => ELECTRICAL_REF
        );

    R2 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => sin_out2,
           p2 => electrical_ref
        );
end TB_v_source ;


