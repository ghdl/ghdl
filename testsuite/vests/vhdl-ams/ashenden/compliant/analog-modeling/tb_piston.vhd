
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
use IEEE_proposed.mechanical_systems.all;

entity tb_piston is
end tb_piston;

architecture TB_piston of tb_piston is
    -- Component declarations
    -- Signal declarations
    terminal n1, n2 : translational;
begin
    -- Signal assignments
    -- Component instances
    Force1 : entity work.ForcePulse_t(ideal)
        generic map(
            initial => 0.0,
            pulse => 20.0e-3,
            ti2p => 1 ms,
            tp2i => 1 ms,
            delay => 1 ms,
            width => 1 sec,
            period => 3 sec
        )
        port map(
            trans_pos => n1,
            trans_neg => TRANSLATIONAL_REF
        );
    mass1 : entity work.piston(simple)
        port map(
            motion => n1
        );
    Force2 : entity work.ForcePulse_t(ideal)
        generic map(
            initial => 0.0,
            pulse => 20.0e-3,
            ti2p => 1 ms,
            tp2i => 1 ms,
            delay => 1 ms,
            width => 1 sec,
            period => 3 sec
        )
        port map(
            trans_pos => n2,
            trans_neg => TRANSLATIONAL_REF
        );
    mass2 : entity work.mass_t(ideal)
        generic map(
            m => 10.0
        )
        port map(
            trans1 => n2
        );
end TB_piston;
