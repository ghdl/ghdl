
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

entity tb_inv_integrator is
end tb_inv_integrator;

architecture TB_inv_integrator of tb_inv_integrator is
    -- Component declarations
    -- Signal declarations
    terminal vin : electrical;
    terminal vout : electrical;
begin
    -- Signal assignments
    -- Component instances
    v1 : entity work.v_sine(ideal)
        generic map(
            amplitude => 0.2,
            freq => 1.0e3
        )
        port map(
            pos => vin,
            neg => ELECTRICAL_REF
        );
    inverting_integ1 : entity work.inverting_integrator(structural)
        port map(
            output => vout,
            input => vin
        );
    RLoad : entity work.load_res(ideal)
        generic map(
            R => 100.0
        )
        port map(
            node1 => vout,
            node2 => ELECTRICAL_REF
        );
end TB_inv_integrator;
