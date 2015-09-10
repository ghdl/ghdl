-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:				 	Martin Zabel
--									Patrick Lehmann
-- 
-- Package:				 	Component declarations for PoC.io.uart
--
-- Description:
-- ------------------------------------
--	TODO
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

library	PoC;
use			PoC.utils.all;
use			PoC.physical.all;


package uart is
	type T_IO_UART_FLOWCONTROL_KIND is (
		UART_FLOWCONTROL_NONE,
		UART_FLOWCONTROL_XON_XOFF,
		UART_FLOWCONTROL_RTS_CTS,
		UART_FLOWCONTROL_RTR_CTS
	);
	
	constant C_IO_UART_TYPICAL_BAUDRATES		: T_BAUDVEC		:= (
		 0 =>		 300 Bd,	 1 =>		 600 Bd,	 2 =>		1200 Bd,	 3 =>		1800 Bd,	 4 =>		2400 Bd,
		 5 =>		4000 Bd,	 6 =>		4800 Bd,	 7 =>		7200 Bd,	 8 =>		9600 Bd,	 9 =>	 14400 Bd,
		10 =>	 16000 Bd,	11 =>	 19200 Bd,	12 =>	 28800 Bd,	13 =>	 38400 BD,	14 =>	 51200 Bd,
		15 =>	 56000 Bd,	16 =>	 57600 Bd,	17 =>	 64000 Bd,	18 =>	 76800 Bd,	19 =>	115200 Bd,
		20 =>	128000 Bd,	21 =>	153600 Bd,	22 =>	230400 Bd,	23 =>	250000 Bd,	24 =>	256000 BD,
		25 =>	460800 Bd,	26 =>	500000 Bd,	27 =>	576000 Bd,	28 =>	921600 Bd
	);
	
	function io_UART_IsTypicalBaudRate(br : BAUD) return BOOLEAN;

	-- Bit clock generator
	component uart_bclk is
		generic (
			CLOCK_FREQ		: FREQ			:= 100 MHz;
			BAUDRATE			: BAUD			:= 115200 Bd
		);
		port (
			clk				: in	std_logic;
			rst				: in	std_logic;
			bclk_r		: out	std_logic;
			bclk_x8_r	: out	std_logic
		);
	end component;

	-- Transmitter
	component uart_tx is
		port (
			clk    : in  std_logic;
			rst    : in  std_logic;
			bclk_r : in  std_logic;
			stb    : in  std_logic;
			din    : in  std_logic_vector(7 downto 0);
			rdy    : out std_logic;
			txd    : out std_logic
		);
	end component;

	-- Receiver
	component uart_rx is
		generic (
			OUT_REGS : boolean
		);
		port (
			clk       : in  std_logic;
			rst       : in  std_logic;
			bclk_x8_r : in  std_logic;
			rxd       : in  std_logic;
			dos       : out std_logic;
			dout      : out std_logic_vector(7 downto 0)
		);
	end component;

	-- Wrappers
	-- ===========================================================================
	-- UART with FIFOs and optional flow control
	component uart_fifo is
	generic (
		CLOCK_FREQ			: FREQ														:= 100 MHz;
		BAUDRATE				: BAUD														:= 115200 Bd;
		FLOWCONTROL			: T_IO_UART_FLOWCONTROL_KIND			:= UART_FLOWCONTROL_NONE;
		TX_MIN_DEPTH		: POSITIVE												:= 16;
		TX_ESTATE_BITS	: NATURAL													:= 1;
		RX_MIN_DEPTH		: POSITIVE												:= 16;
		RX_FSTATE_BITS	: NATURAL													:= 1;
		RX_OUT_REGS			: BOOLEAN													:= FALSE;
		
		SWFC_XON_CHAR			: std_logic_vector(7 downto 0)	:= x"11";		-- ^Q
    SWFC_XON_TRIGGER	: real													:= 0.0625;
		SWFC_XOFF_CHAR		: std_logic_vector(7 downto 0)	:= x"13";		-- ^S
		SWFC_XOFF_TRIGGER	: real													:= 0.75
	);
	port (
		Clock					: in	std_logic;
		Reset					: in	std_logic;

		-- FIFO interface
		TX_put				: in	STD_LOGIC;
		TX_Data				: in	STD_LOGIC_VECTOR(7 downto 0);
		TX_Full				: out	STD_LOGIC;
		TX_EmptyState	: out	STD_LOGIC_VECTOR(TX_ESTATE_BITS - 1 downto 0);
		
		RX_Valid			: out	STD_LOGIC;
		RX_Data				: out	STD_LOGIC_VECTOR(7 downto 0);
		RX_got				: in	STD_LOGIC;
		RX_FullState	: out	STD_LOGIC_VECTOR(RX_FSTATE_BITS - 1 downto 0);
		RX_Overflow		: out	std_logic;
		
		-- External Pins
		UART_RX				: in	std_logic;
		UART_TX				: out	std_logic
	);
	end component;
end package;


package body uart is
	function io_UART_IsTypicalBaudRate(br : BAUD) return BOOLEAN is
	begin
		for i in C_IO_UART_TYPICAL_BAUDRATES'range loop
			next when (br /= C_IO_UART_TYPICAL_BAUDRATES(i));
			return TRUE;
		end loop;
		return FALSE;
	end function;
end package body;
