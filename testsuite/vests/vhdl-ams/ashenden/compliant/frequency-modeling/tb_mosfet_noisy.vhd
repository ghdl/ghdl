
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

entity tb_mosfet_noisy is
end tb_mosfet_noisy ;

architecture TB_mosfet_noisy of tb_mosfet_noisy is
    -- Component declarations
    -- Signal declarations
    terminal d : electrical;
    terminal g : electrical;
begin
    -- Signal assignments
    -- Component instances
    mosfet1 : entity work.nmos_transistor_wa(noisy)
        port map(
            gate => g,
            drain => d,
            source => ELECTRICAL_REF
        );
    v1 : entity work.v_constant(ideal)
        generic map(
            level => 4.0
        )
        port map(
            pos => g,
            neg => ELECTRICAL_REF
        );
    mosfet2 : entity work.nmos_transistor_wa(noisy)
        port map(
            gate => g,
            drain => ELECTRICAL_REF,
            source => d
        );
    v4 : entity work.v_pulse(ideal)
        generic map(
            initial => 0.0,
            pulse => 5.0,
            ti2p => 1 ms,
            tp2i => 1 ms,
            delay => 1 us,
            width => 1 us,
            period => 2.002 ms
        )
        port map(
            pos => d,
            neg => ELECTRICAL_REF
        );
end TB_mosfet_noisy ;
