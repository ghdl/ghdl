-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Package:					VHDL package for component declarations, types and functions
--									associated to the PoC.arith namespace
--
-- Description:
-- ------------------------------------
--		For detailed documentation see below.
-- 
-- License:
-- =============================================================================
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
-- =============================================================================

library IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library PoC;
use 		PoC.utils.all;


package arith is
	component arith_firstone is
		generic (
			N			: positive																			-- Length of Token Chain
		);
		port (
			tin		: in	std_logic		:= '1';												-- Enable:		Fed Token
			rqst	: in	std_logic_vector(N-1 downto 0);						-- Request:		Token Requests
			grnt	: out std_logic_vector(N-1 downto 0);						-- Grant:			Token Output
			tout	: out std_logic;											 					-- Inactive:	Unused Token
			bin		: out std_logic_vector(log2ceil(N)-1 downto 0)	-- Binary Grant Index
		);
	end component;

	component arith_counter_bcd is
		generic (
			DIGITS	: positive);
		port (
			clk			: in	std_logic;
			rst			: in	std_logic;
			inc			: in	std_logic;
			val			: out T_BCD_VECTOR(DIGITS-1 downto 0));
	end component;
	
	component arith_counter_gray is
		generic (
			BITS	: positive;								-- Bit width of the counter
			INIT	: natural					:= 0		-- Initial/reset counter value
		);
		port (
			clk		: in	std_logic;
			rst		: in	std_logic;													-- Reset to INIT value
			inc		: in	std_logic;													-- Increment
			dec		: in	std_logic		:= '0';									-- Decrement
			val		: out std_logic_vector(BITS-1 downto 0);	-- Value output
			cry		: out std_logic														-- Carry output
		);
	end component;
	
	component arith_div
		generic (
			N						: positive;
			RAPOW				: positive;
			REGISTERED	: boolean);
		port (
			clk					: in	std_logic;
			rst					: in	std_logic;
			start				: in	std_logic;
			rdy					: out std_logic;
			arg1, arg2	: in	std_logic_vector(N-1 downto 0);
			res					: out std_logic_vector(N-1 downto 0));
	end component;

	component arith_div_pipelined
		generic (
			DIVIDEND_BITS		: POSITIVE;
			DIVISOR_BITS		: POSITIVE;
			RADIX						: POSITIVE
		);
		port (
			Clock						: in	STD_LOGIC;
			Reset						: in	STD_LOGIC;
			Enable					: in	STD_LOGIC;
			Dividend				: in	STD_LOGIC_VECTOR(DIVIDEND_BITS - 1 downto 0);
			Divisor					: in	STD_LOGIC_VECTOR(DIVISOR_BITS - 1 downto 0);
			Quotient				: out	STD_LOGIC_VECTOR(DIVIDEND_BITS - 1 downto 0);
			Valid						: out STD_LOGIC
		);
	end component;
	
	component arith_prng
		generic (
			BITS		: positive;
			SEED		: std_logic_vector := "0"
		);
		port (
			clk		: in	std_logic;
			rst		: in	std_logic;
			got		: in	std_logic;
			val		: out std_logic_vector(BITS-1 downto 0));
	end component;

	component arith_muls_wide
		generic (
			NA		: integer range 2 to 18;
			NB		: integer range 19 to 36;
			SPLIT	: positive
		);
		port (
			a			: in	signed(NA-1 downto 0);
			b			: in	signed(NB-1 downto 0);
			p			: out signed(NA+NB-1 downto 0)
		);
	end component;

	component arith_sqrt
		generic (
			N		: positive
		);
		port (
			rst			: in	std_logic;
			clk			: in	std_logic;
			arg			: in	std_logic_vector(N-1 downto 0);
			start		: in	std_logic;
			sqrt		: out std_logic_vector((N-1)/2 downto 0);
			rdy			: out std_logic
		);
	end component;

	type tArch		 is (AAM, CAI, CCA, PAI);
	type tBlocking is (DFLT, FIX, ASC, DESC);
	type tSkipping is (PLAIN, CCC, PPN_KS, PPN_BK);
	
	component arith_addw is
		generic (
			N						: positive;						-- Operand Width
			K						: positive;						-- Block Count

			ARCH				: tArch			:= AAM;		-- Architecture
			BLOCKING		: tBlocking	:= DFLT;	-- Blocking Scheme
			SKIPPING		: tSkipping	:= CCC;		-- Carry Skip Scheme
			P_INCLUSIVE	: boolean		:= false	-- Use Inclusive Propagate, i.e. c^1
		);
		port (
			a, b	: in std_logic_vector(N-1 downto 0);
			cin		: in std_logic;

			s			: out std_logic_vector(N-1 downto 0);
			cout	: out std_logic
		);
	end component;

	component arith_same is
		generic (
			N		: positive														-- Input width
		);
		port (
			g		: in	std_logic		:= '1';							-- Guard Input (!g => !y)
			x		: in	std_logic_vector(N-1 downto 0);	-- Input Vector
			y		: out std_logic												-- All-same Output
		);
	end component;
	
	component arith_inc_ovcy_xilinx is
		generic (
			N		: positive												 		-- Bit Width
		);
		port (
			p		: in	std_logic_vector(N-1 downto 0);	-- Argument
			g		: in	std_logic;											-- Increment Guard
			v		: out std_logic												-- Overflow Output
		);
	end component;
end package;
