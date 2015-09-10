-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Testbench:				Tests global constants, functions and settings
--
-- Authors:					Patrick Lehmann
--
-- Description:
-- ------------------------------------
--		TODO
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

entity strings_tb is
end strings_tb;

library IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library	PoC;
use			PoC.config.all;
use			PoC.utils.all;
use			PoC.strings.all;
use			PoC.simulation.all;


architecture tb of strings_tb is
	constant raw_format_slv_dec_result0		: STRING		:= raw_format_slv_dec(STD_LOGIC_VECTOR'(x"12"));
	constant raw_format_slv_dec_result1		: STRING		:= raw_format_slv_dec(x"3456");
	constant raw_format_slv_dec_result2		: STRING		:= raw_format_slv_dec(x"12345678");
	constant raw_format_slv_dec_result3		: STRING		:= raw_format_slv_dec(x"A1B2C3D4E5F607A8");
	
	constant str_length_result0						: INTEGER		:= str_length("");
	constant str_length_result1						: INTEGER		:= str_length((1 to 3 => C_POC_NUL));
	constant str_length_result2						: INTEGER		:= str_length("Hello");
	constant str_length_result3						: INTEGER		:= str_length("Hello" & (1 to 3 => C_POC_NUL));
	
	constant str_match_result0						: BOOLEAN		:= str_match("", "");
	constant str_match_result1						: BOOLEAN		:= str_match("", (1 to 3 => C_POC_NUL));
	constant str_match_result2						: BOOLEAN		:= str_match("Hello", "hello");
	constant str_match_result3						: BOOLEAN		:= str_match("Hello", "Hello");
	constant str_match_result4						: BOOLEAN		:= str_match("Hello World", "Hello");
	constant str_match_result5						: BOOLEAN		:= str_match("Hello", "Hello World");
	constant str_match_result6						: BOOLEAN		:= str_match("Hello", "Hello" & (1 to 3 => C_POC_NUL));
	
	constant str_imatch_result0						: BOOLEAN		:= str_imatch("", "");
	constant str_imatch_result1						: BOOLEAN		:= str_imatch("", (1 to 3 => C_POC_NUL));
	constant str_imatch_result2						: BOOLEAN		:= str_imatch("Hello", "hello");
	constant str_imatch_result3						: BOOLEAN		:= str_imatch("Hello", "Hello");
	constant str_imatch_result4						: BOOLEAN		:= str_imatch("Hello World", "Hello");
	constant str_imatch_result5						: BOOLEAN		:= str_imatch("Hello", "Hello World");
	constant str_imatch_result6						: BOOLEAN		:= str_imatch("Hello", "Hello" & (1 to 3 => C_POC_NUL));
begin
	process
	begin
		-- raw_format_slv_dec tests
		tbAssert((raw_format_slv_dec_result0 = "18"),										"raw_format_slv_dec(0x12)="								& raw_format_slv_dec_result0	&	"    Expected='18'");
		tbAssert((raw_format_slv_dec_result1 = "13398"),								"raw_format_slv_dec(0x3456)="							& raw_format_slv_dec_result1	&	"    Expected='13398'");
		tbAssert((raw_format_slv_dec_result2 = "305419896"),						"raw_format_slv_dec(0x12345678)="					& raw_format_slv_dec_result2	&	"    Expected='305419896'");
		tbAssert((raw_format_slv_dec_result3 = "11651590505119483816"),	"raw_format_slv_dec(0xA1b2c3d4e5f607a8)="	& raw_format_slv_dec_result3	&	"    Expected='11651590505119483816'");
		
		-- str_length tests
		tbAssert((str_length_result0 = 0),			"str_length('')="											& INTEGER'image(str_length_result0)	& "  Expected=0");
		tbAssert((str_length_result1 = 0),			"str_length('\0\0\0')="								& INTEGER'image(str_length_result1)	& "  Expected=0");
		tbAssert((str_length_result2 = 5),			"str_length('Hello')="								& INTEGER'image(str_length_result2)	& "  Expected=5");
		tbAssert((str_length_result3 = 5),			"str_length('Hello\0\0\0')="					& INTEGER'image(str_length_result3)	& "  Expected=5");
		
		-- str_match tests
		tbAssert((str_match_result0 = TRUE),		"str_match('', '')="									& BOOLEAN'image(str_match_result0)	& "  Expected=TRUE");
		tbAssert((str_match_result1 = TRUE),		"str_match('', '\0\0\0')="						& BOOLEAN'image(str_match_result1)	& "  Expected=TRUE");
		tbAssert((str_match_result2 = FALSE),		"str_match('Hello', 'hello')="				& BOOLEAN'image(str_match_result2)	& "  Expected=FALSE");
		tbAssert((str_match_result3 = TRUE),		"str_match('Hello', 'Hello')="				& BOOLEAN'image(str_match_result3)	& "  Expected=TRUE");
		tbAssert((str_match_result4 = FALSE),		"str_match('Hello World', 'Hello')="	& BOOLEAN'image(str_match_result4)	& "  Expected=FALSE");
		tbAssert((str_match_result5 = FALSE),		"str_match('Hello', 'Hello World')="	& BOOLEAN'image(str_match_result5)	& "  Expected=FALSE");
		tbAssert((str_match_result6 = TRUE),		"str_match('Hello', 'Hello\0\0\0')="	& BOOLEAN'image(str_match_result6)	& "  Expected=TRUE");
		
		-- str_imatch tests
		tbAssert((str_imatch_result0 = TRUE),		"str_imatch('', '')="									& BOOLEAN'image(str_imatch_result0) & "  Expected=TRUE");
		tbAssert((str_imatch_result1 = TRUE),		"str_imatch('', '\0\0\0')="						& BOOLEAN'image(str_imatch_result1) & "  Expected=TRUE");
		tbAssert((str_imatch_result2 = TRUE),		"str_imatch('Hello', 'hello')="				& BOOLEAN'image(str_imatch_result2) & "  Expected=TRUE");
		tbAssert((str_imatch_result3 = TRUE),		"str_imatch('Hello', 'Hello')="				& BOOLEAN'image(str_imatch_result3) & "  Expected=TRUE");
		tbAssert((str_imatch_result4 = FALSE),	"str_imatch('Hello World', 'Hello')="	& BOOLEAN'image(str_imatch_result4) & "  Expected=FALSE");
		tbAssert((str_imatch_result5 = FALSE),	"str_imatch('Hello', 'Hello World')="	& BOOLEAN'image(str_imatch_result5) & "  Expected=FALSE");
		tbAssert((str_imatch_result6 = TRUE),		"str_imatch('Hello', 'Hello\0\0\0')="	& BOOLEAN'image(str_imatch_result6) & "  Expected=TRUE");

		-- str_pos tests
		-- str_ipos tests
		-- str_find tests
		-- str_ifind tests
		-- str_replace tests
		-- str_substr tests
		-- str_ltrim tests
		-- str_rtrim tests
		-- str_trim tests
		-- str_toLower tests
		-- str_toUpper tests
		
		-- simulation completed
		
		-- Report overall simulation result
		tbPrintResult;
		wait;
	end process;
end;
