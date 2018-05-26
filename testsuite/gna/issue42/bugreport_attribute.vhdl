-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
-- Reproducer:			Experiments on custom attributes ended in a crash.
-- 
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany
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
--
-- Issue:
--	I'm not sure if my experimental code is allowed in VHDL, but it let GHDL
--	crash. So I'm reporting just an unhandled exception.
--
--	GHDL's output is:
--		.\attribute.vhdl:64:58: can't match 'image attribute with type character
--		.\attribute.vhdl:64:53: (location of 'image attribute)
--		finish_sem_name: cannot handle IIR_KIND_OVERLOAD_LIST (??:??:??)
--		
--		******************** GHDL Bug occurred ****************************
--		Please report this bug on https://github.com/tgingold/ghdl/issues
--		GHDL release: GHDL 0.34dev (commit: 2016-02-11;  git branch: paebbels/master';  hash: f24fdfb) [Dunoon edition]
--		Compiled with GNAT Version: GPL 2015 (20150428-49)
--		In directory: H:\Austausch\PoC\temp\ghdl\
--		Command line:
--		C:\Tools\GHDL.new\bin\ghdl.exe -a --std=08 .\attribute.vhdl
--		Exception TYPES.INTERNAL_ERROR raised
--		Exception information:
--		Exception name: TYPES.INTERNAL_ERROR
--		Message: errorout.adb:66
--		******************************************************************
--
-- GHDL calls:
--	PS> ghdl.exe -a --std=93c .\attribute.vhdl
--	PS> ghdl.exe -a --std=08 .\attribute.vhdl
--	
library IEEE;
use			IEEE.std_logic_1164.all;


entity test is
end entity;

architecture tb of test is
	function to_string(slv : STD_LOGIC_VECTOR) return STRING is
		variable Result		: STRING(slv'length - 1 downto 0);
	begin
		for i in slv'range loop
			Result(i + 1)	:= STD_LOGIC'image(slv(i));
		end loop;
		return Result;
	end function;
	
	attribute serialize		: to_string;
	
	signal mySignal		: STD_LOGIC_VECTOR(7 downto 0);
	attribute serialize of mySignal		: signal is to_string[STD_LOGIC_VECTOR return STRING];
	
begin
	mySignal		<= x"24";

	process
	begin
		report "mySignal=" & mySignal'serialize severity NOTE;
		wait;
	end process;
end architecture;
