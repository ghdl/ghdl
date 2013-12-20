
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

--
-- File : C:\VHDL-AMS\CaseStudies\CS4_CommSystem\Default\genhdl\vhdl\tb_pll.vhd
-- CDB  : C:\VHDL-AMS\CaseStudies\CS4_CommSystem\default\default.cdb
-- By   : CDB2VHDL Netlister version 16.1.0.2
-- Time : Fri Apr 05 12:08:46 2002

-- Entity/architecture declarations 

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.fluidic_systems.all;
use IEEE_proposed.thermal_systems.all;
use IEEE_proposed.radiant_systems.all;

entity tb_pll is
end tb_pll;

architecture tb_pll of tb_pll is
    -- Component declarations
    -- Signal declarations
    signal f_ref : real;
    terminal lf_out : electrical;
    terminal v_ref : electrical;
    signal vco_f : real;
    terminal vco_out : electrical;
begin
    -- Signal assignments
    -- Component instances
    PLL6 : entity work.PLL(behavioral)
        generic map(
            Fp => 20.0e3,
            Fz => 1.0e6,
            Kv => 100.0e3,
            Fc => 1.0e6
        )
        port map(
            input => v_ref,
            lf_out => lf_out,
            vco_out => vco_out
        );
    v1 : entity work.v_SweptSine(bhv)
        generic map(
            StartFreq => 900.0e3,
            SweepRate => 2000.0e6,
            FinishFreq => 1.1e6,
            InitDelay => 80.0e-6,
            PeakAmp => 5.0
        )
        port map(
            pos => v_ref,
            neg => ELECTRICAL_REF
        );
    MeasFreq9 : entity work.MeasFreq(ThresDetect)
        port map(
            input => v_ref,
            f_out => f_ref
        );
    MeasFreq10 : entity work.MeasFreq(ThresDetect)
        port map(
            input => vco_out,
            f_out => vco_f
        );
end tb_pll;
