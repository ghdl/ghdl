-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Module:					uart_rx_tb
--
-- Authors:					Patrick Lehmann
-- 
-- Description:
-- ------------------------------------
-- Testbench for arith_counter_bcd
-- 
-- License:
-- ============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany
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
use			PoC.vectors.all;
use			PoC.strings.all;
use			PoC.physical.all;
use			PoC.simulation.all;
use			PoC.uart.all;


entity uart_rx_tb is
end entity;


architecture tb of uart_rx_tb is
	constant CLOCK_FREQ		: FREQ			:= 100 MHz;
	constant BAUDRATE			: BAUD			:= 4.2 MBd;
	
	signal Clock					: STD_LOGIC;
	signal Reset					: STD_LOGIC;

	signal BitClock				: STD_LOGIC;
	signal BitClock_x8		: STD_LOGIC;
	
	signal UART_RX				: STD_LOGIC;
	
	signal RX_Strobe			: STD_LOGIC;
	signal RX_Data				: T_SLV_8;
	
	function simGenerateWaveform_UART_Word(Data : T_SLV_8; Baudrate : BAUD := 115.200 kBd) return T_SIM_WAVEFORM_SL is
		constant BIT_TIME : TIME											:= to_time(to_freq(Baudrate));
		variable Result		: T_SIM_WAVEFORM_SL(0 to 9)	:= (others =>	(Delay => BIT_TIME, Value => '-'));
	begin
		Result(0).Value := '0';
		for i in Data'range loop
			Result(i + 1).Value	:= Data(i);
		end loop;
		Result(9).Value := '1';
		return Result;
	end function;
	
	function simGenerateWaveform_UART_Stream(Data : T_SLVV_8; Baudrate : BAUD := 115.200 kBd) return T_SIM_WAVEFORM_SL is
		variable Result : T_SIM_WAVEFORM_SL(0 to (Data'length * 10) - 1);
	begin
		for i in Data'range loop
			Result(i * 10 to ((i + 1) * 10) - 1)	:= simGenerateWaveform_UART_Word(Data(i), BAUDRATE);
		end loop;
		return Result;
	end function;
	
	constant DATA_STREAM	: T_SLVV_8	:= (x"12", x"45", x"FE", x"C4", x"02");
	
begin
	simGenerateClock(Clock, CLOCK_FREQ);
	simGenerateWaveform(Reset, simGenerateWaveform_Reset(Pause => 50 ns));
	p1f: if true generate
	  simGenerateWaveform(UART_RX, simGenerateWaveform_UART_Stream(DATA_STREAM, BAUDRATE), '1');
	end generate;
	p1t: if false generate
		process
			constant wf : T_SIM_WAVEFORM_SL := simGenerateWaveform_UART_Stream(DATA_STREAM, BAUDRATE);
		begin
			simGenerateWaveform(UART_RX, wf, '1');
		end process;
	end generate;
	
	bclk : entity PoC.uart_bclk
		generic map (
			CLOCK_FREQ	=> CLOCK_FREQ,
			BAUDRATE		=> BAUDRATE
		)
		port map (
			clk					=> Clock,
			rst					=> Reset,
			bclk				=> BitClock,
			bclk_x8			=> BitClock_x8
		);

	RX : entity PoC.uart_rx
		generic map (
			OUT_REGS	=> FALSE
		)
		port map (
			clk				=> Clock,
			rst				=> Reset,
			bclk_x8		=> BitClock_x8,
			dos				=> RX_Strobe,
			dout			=> RX_Data,
			rxd				=> UART_RX
		);

	process
	begin
		for i in DATA_STREAM'range loop
			wait until rising_edge(Clock) and (RX_Strobe = '1');
			report TIME'image(NOW) severity NOTE;
			tbAssert((RX_Data = DATA_STREAM(i)), "Data Byte " & INTEGER'image(i) & " received: " & to_string(RX_Data, 'h') & " expected: " & to_string(DATA_STREAM(i), 'h'));
		end loop;
		
		wait for 1 us;
		simStop;
		
		tbPrintResult;
		wait;
	end process;
	
end architecture;
