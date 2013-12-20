
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
    terminal in_src, out_flt : electrical;
    -- Component declarations
    -- Signal declarations
begin
    -- Signal assignments
    -- Component instances
    vio : entity work.v_source(source_sine)
        port map(
            p => in_src,
            m => ELECTRICAL_REF
        );

    R1 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => in_src,
            p2 => electrical_ref
        );
end TB_v_source ;


