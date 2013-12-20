
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

library IEEE; use IEEE.std_logic_1164.all; use IEEE.std_logic_arith.all;
library IEEE_proposed; use IEEE_proposed.electrical_systems.all;

entity tb_a2d_d2a is

end tb_a2d_d2a;

architecture TB_a2d_d2a of tb_a2d_d2a is
    -- Component declarations
    -- Signal declarations
	terminal ana_out : electrical;
	terminal analog_in : electrical;
	signal clock : std_ulogic;
	signal start : std_ulogic;
	signal eoc : std_ulogic;
	signal eoc_logic: std_logic;
	signal oe : std_logic;
	signal data_bus : std_ulogic_vector(0 to 9);
	signal latch : std_ulogic;
	signal latch_logic : std_logic;
	signal nn_eoc : std_logic;
	signal or_out : std_logic;
	signal n_eoc : std_logic;
begin
    -- Signal assignments
	eoc_logic <= To_X01Z(eoc);	-- convert std_ulogic to std_logic
	latch <= To_X01(latch_logic);	-- convert std_logic to std_ulogic
    -- Component instances
    ad1 : entity work.a2d_nbit(sar)
        port map(
            dout => data_bus,
            ain => analog_in,
            clk => clock,
            start => start,
            eoc => eoc
        );
    v1 : entity work.v_sine(ideal)
        generic map(
            freq => 2.5,
            amplitude => 2.5,
			offset => 2.5,
            phase => 0.0
        )
        port map(
            pos => analog_in,
            neg => ELECTRICAL_REF
        );
    inv1 : entity work.inverter(ideal)
        generic map(
            delay => 2us
        )
        port map(
            input => or_out,
            output => oe
        );
    inv2 : entity work.inverter(ideal)
        generic map(
            delay => 2us
        )
        port map(
            input => n_eoc,
            output => nn_eoc
        );
    or1 : entity work.or2(ideal)
        port map(
            in1 => n_eoc,
            in2 => nn_eoc,
            output => or_out
        );
    inv3 : entity work.inverter(ideal)
        generic map(
            delay => 0us
        )
        port map(
            input => eoc_logic,
            output => n_eoc
        );
    U2 : entity work.buff(ideal)
        generic map(
            delay => 250ns
        )
        port map(
            input => oe,
            output => latch_logic
        );
    da1 : entity work.dac_10_bit(behavioral)
        port map(
            bus_in => data_bus,
            analog_out => ana_out,
            clk => latch
        );
  -- clock
    P_clock :
    process
    begin
        clock <= '1';
        wait for 50.0 us;
        clock <= '0';
        wait for 50.0 us;
    end process P_clock;

  -- start
    P_start :
    process
    begin
        start <= '0';
        wait for 2.0 ms;
        start <= '1';
        wait for 0.2 ms;
        start <= '0';
        wait for 2.0 ms;
    end process P_start;


end TB_a2d_d2a;
