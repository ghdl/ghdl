
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
use IEEE_proposed.mechanical_systems.all;

entity tb_sensor is
end tb_sensor;

architecture tb_sensor of tb_sensor is
    -- Component declarations
    -- Signal declarations
    terminal vin : electrical;
    signal clk, q : bit;
    signal  lclclkinitwire : bit := '0';
begin
    -- Signal assignments
    -- Component instances
    v1 : entity work.v_sine(ideal)
        generic map(
           freq => 10.0,
           amplitude => 1.0
        )
        port map(
            pos => vin,
			neg => electrical_ref
        );
    sens1 : entity work.sensor_wa(detailed_timing)
        generic map(
		threshold => 0.25,
            tipd_clk => 10 ns,
            tipd_input => 20.0e-9,
            topd_q => 10 ns
        )
        port map(
            input => vin,
            clk => clk,
            q => q
        );
  -- ctrl
    P_ctrl :
    process
    begin
      if (lclclkinitwire /= '1')
      then
        clk <= '0';
        wait for 1000.000 ns;
      else
        clk <= '1';
        wait for 5240.000 ns;
        clk <= '0';
        wait for 34760.000 ns;
      end if;
    end process P_ctrl;

    KillerProc :
    process
    begin
      wait for 1 ns;
      lclclkinitwire <= '1';
      wait;
    end process;
end tb_sensor;
