
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

entity tb_moving_mass is
end tb_moving_mass;

architecture TB_moving_mass of tb_moving_mass is
    -- Component declarations
    -- Signal declarations
    terminal msd_discrete, msd_mdl : translational;
begin
    -- Signal assignments
    -- Component instances
    mass1 : entity work.mass_t(ideal)
        generic map(
            m => 10.0
        )
        port map(
            trans1 => msd_discrete
        );
    spring2 : entity work.spring_t(linear)
        generic map(
            k => 2.0
        )
        port map(
            trans1 => msd_discrete,
            trans2 => TRANSLATIONAL_REF
        );
    damper1 : entity work.damper_t(ideal)
        generic map(
            d => 5.0
        )
        port map(
            trans1 => msd_discrete,
            trans2 => TRANSLATIONAL_REF
        );
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
            trans_pos => msd_discrete,
            trans_neg => TRANSLATIONAL_REF
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
            trans_pos => msd_mdl,
            trans_neg => TRANSLATIONAL_REF
        );
    moving_mass4 : entity work.moving_mass_wa(behavioral)
        port map(
            external_attachment => msd_mdl
        );
end TB_moving_mass;
