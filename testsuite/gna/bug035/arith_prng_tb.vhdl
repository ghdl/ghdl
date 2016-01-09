-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Testbench:				Pseudo-Random Number Generator (PRNG).
-- 
-- Authors:					Patrick Lehmann
-- 
-- Description:
-- ------------------------------------
--		Automated testbench for PoC.arith_prng
--		The Pseudo-Random Number Generator is instantiated for 8 bits. The
--		output sequence is compared to 256 pre calculated values.
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

LIBRARY IEEE;
USE			IEEE.STD_LOGIC_1164.ALL;
USE			IEEE.NUMERIC_STD.ALL;

LIBRARY PoC;
USE			PoC.utils.ALL;
USE			PoC.vectors.ALL;
USE			PoC.strings.ALL;
USE			PoC.simulation.ALL;


ENTITY arith_prng_tb IS
END;


ARCHITECTURE test OF arith_prng_tb IS
	CONSTANT CLOCK_PERIOD_100MHZ			: TIME															:= 10 ns;

	CONSTANT COMPARE_LIST_8_BITS			: T_SLVV_8(0 TO 255)								:= (
		x"12", x"24", x"48", x"90", x"21", x"42", x"85", x"0A", x"14", x"28", x"51", x"A2", x"45", x"8B", x"17", x"2E",
		x"5D", x"BB", x"77", x"EF", x"DE", x"BC", x"79", x"F2", x"E4", x"C9", x"93", x"27", x"4E", x"9C", x"38", x"70",
		x"E1", x"C3", x"86", x"0C", x"18", x"31", x"63", x"C6", x"8C", x"19", x"33", x"67", x"CE", x"9D", x"3A", x"74",
		x"E9", x"D2", x"A5", x"4B", x"96", x"2D", x"5B", x"B7", x"6E", x"DD", x"BA", x"75", x"EB", x"D6", x"AD", x"5A",
		x"B5", x"6A", x"D5", x"AB", x"56", x"AC", x"58", x"B1", x"62", x"C4", x"88", x"11", x"22", x"44", x"89", x"13",
		x"26", x"4C", x"98", x"30", x"61", x"C2", x"84", x"08", x"10", x"20", x"40", x"81", x"02", x"05", x"0B", x"16",
		x"2C", x"59", x"B3", x"66", x"CC", x"99", x"32", x"65", x"CA", x"95", x"2B", x"57", x"AE", x"5C", x"B9", x"73",
		x"E7", x"CF", x"9F", x"3E", x"7C", x"F8", x"F1", x"E2", x"C5", x"8A", x"15", x"2A", x"55", x"AA", x"54", x"A8",
		x"50", x"A0", x"41", x"83", x"06", x"0D", x"1A", x"35", x"6B", x"D7", x"AF", x"5E", x"BD", x"7B", x"F6", x"EC",
		x"D8", x"B0", x"60", x"C0", x"80", x"00", x"01", x"03", x"07", x"0F", x"1E", x"3D", x"7A", x"F4", x"E8", x"D0",
		x"A1", x"43", x"87", x"0E", x"1C", x"39", x"72", x"E5", x"CB", x"97", x"2F", x"5F", x"BF", x"7F", x"FE", x"FD",
		x"FB", x"F7", x"EE", x"DC", x"B8", x"71", x"E3", x"C7", x"8E", x"1D", x"3B", x"76", x"ED", x"DA", x"B4", x"68",
		x"D1", x"A3", x"47", x"8F", x"1F", x"3F", x"7E", x"FC", x"F9", x"F3", x"E6", x"CD", x"9B", x"36", x"6D", x"DB",
		x"B6", x"6C", x"D9", x"B2", x"64", x"C8", x"91", x"23", x"46", x"8D", x"1B", x"37", x"6F", x"DF", x"BE", x"7D",
		x"FA", x"F5", x"EA", x"D4", x"A9", x"52", x"A4", x"49", x"92", x"25", x"4A", x"94", x"29", x"53", x"A6", x"4D",
		x"9A", x"34", x"69", x"D3", x"A7", x"4F", x"9E", x"3C", x"78", x"F0", x"E0", x"C1", x"82", x"04", x"09", x"12"
	);

	SIGNAL SimStop			: std_logic := '0';

	SIGNAL Clock				: STD_LOGIC			:= '1';
	SIGNAL Reset				: STD_LOGIC			:= '0';
	SIGNAL Test_got			: STD_LOGIC			:= '0';
	SIGNAL PRNG_Value		: T_SLV_8;
	
BEGIN

	Clock <= Clock xnor SimStop after CLOCK_PERIOD_100MHZ / 2.0;

	PROCESS
	BEGIN
		WAIT UNTIL rising_edge(Clock);
		
		Reset						<= '1';
		WAIT UNTIL rising_edge(Clock);
	
		Reset						<= '0';
		WAIT UNTIL rising_edge(Clock);

		FOR I IN 0 TO 255 LOOP
			Test_got				<= '1';
			WAIT UNTIL rising_edge(Clock);
			tbAssert((PRNG_Value = COMPARE_LIST_8_BITS(I)), "I=" & integer'image(I) &	" Value=" & raw_format_slv_hex(PRNG_Value) & " Expected=" & raw_format_slv_hex(COMPARE_LIST_8_BITS(I)));
		END LOOP;
		
		-- Report overall simulation result
		tbPrintResult;
		SimStop	<= '1';
                assert now < 3000 ns severity failure;
		WAIT;
	END PROCESS;

	prng : entity PoC.arith_prng
		generic map (
			BITS		=> 8,
			SEED		=> x"12"
		)
		port map (
			clk			=> Clock,						
			rst			=> Reset,						-- reset value to initial seed
			got			=> Test_got,				-- the current value has been got, and a new value should be calculated
			val			=> PRNG_Value				-- the pseudo-random number
		);

END;
