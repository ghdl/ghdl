-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:				 	Martin Zabel
--									Patrick Lehmann
-- 
-- Module:				 	UART bit clock / baud rate generator
--
-- Description:
-- ------------------------------------
--	TODO
-- 
--	old comments:
--		UART BAUD rate generator
--		bclk_r    = bit clock is rising
--		bclk_x8_r = bit clock times 8 is rising
--
--
-- License:
-- ============================================================================
-- Copyright 2008-2015 Technische Universitaet Dresden - Germany
--										 Chair for VLSI-Design, Diagnostics and Architecture
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--		http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- ============================================================================

library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library PoC;
use			PoC.utils.all;
use			PoC.strings.all;
use			PoC.physical.all;
use			PoC.components.all;
use			PoC.uart.all;


entity uart_bclk is
	generic (
		CLOCK_FREQ		: FREQ			:= 100 MHz;
		BAUDRATE			: BAUD			:= 115200 Bd
	);
	port (
		clk				: in	std_logic;
		rst				: in	std_logic;
		bclk			: out	std_logic;
		bclk_x8		: out	std_logic
	);
end entity;


architecture rtl of uart_bclk is
	constant UART_OVERSAMPLING_RATE		: POSITIVE					:= 8;
	constant TIME_UNIT_INTERVAL				: TIME							:= 1 sec / (to_real(BAUDRATE, 1 Bd) * real(UART_OVERSAMPLING_RATE));
	constant BAUDRATE_COUNTER_MAX			: POSITIVE					:= TimingToCycles(TIME_UNIT_INTERVAL, CLOCK_FREQ);
	constant BAUDRATE_COUNTER_BITS		: POSITIVE					:= log2ceilnz(BAUDRATE_COUNTER_MAX + 1);

  -- registers
  signal x8_cnt : unsigned(BAUDRATE_COUNTER_BITS - 1 downto 0)	:= (others => '0');
  signal x1_cnt : unsigned(2 downto 0)													:= (others => '0');

  -- control signals
  signal x8_cnt_done : std_logic;
  signal x1_cnt_done : std_logic;

	signal bclk_r			: STD_LOGIC		:= '0';
	signal bclk_x8_r	: STD_LOGIC		:= '0';
begin
	assert FALSE		-- LF works in QuartusII
		report "uart_bclk:" & LF &
					 "  CLOCK_FREQ="		& to_string(CLOCK_FREQ, 3) & LF &
					 "  BAUDRATE="			& to_string(BAUDRATE, 3) & LF &
					 "  COUNTER_MAX="		& INTEGER'image(BAUDRATE_COUNTER_MAX) & LF &
					 "  COUNTER_BITS="	& INTEGER'image(BAUDRATE_COUNTER_BITS)
		severity NOTE;
	
	assert io_UART_IsTypicalBaudRate(BAUDRATE)
		report "The baudrate " & to_string(BAUDRATE, 3) & " is not known to be a typical baudrate!"
		severity WARNING;

	x8_cnt			<= upcounter_next(cnt => x8_cnt, rst => (rst or x8_cnt_done)) when rising_edge(clk);
  x8_cnt_done <= upcounter_equal(cnt => x8_cnt, value => BAUDRATE_COUNTER_MAX - 1);
	
	x1_cnt			<= upcounter_next(cnt => x1_cnt, rst => rst, en => x8_cnt_done) when rising_edge(clk);
  x1_cnt_done <= comp_allzero(x1_cnt);
  
  -- outputs
	-- ---------------------------------------------------------------------------
	-- only x8_cnt_done is pulsed for one clock cycle!
	bclk_r			<= (x1_cnt_done and x8_cnt_done)	when rising_edge(clk);
	bclk_x8_r		<= x8_cnt_done										when rising_edge(clk);
  
	bclk				<= bclk_r;
	bclk_x8			<= bclk_x8_r;
end;
