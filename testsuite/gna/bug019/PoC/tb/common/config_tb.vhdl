-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Testbench:				Tests global constants, functions and settings
--
-- Authors:					Thomas B. Preusser
--									Patrick Lehmann
--
-- Description:
-- ------------------------------------
--		TODO
-- 
-- License:
-- =============================================================================
-- Copyright 2007-2014 Technische Universitaet Dresden - Germany
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

entity config_tb is
end config_tb;


library	PoC;
use			PoC.config.all;
use			PoC.utils.all;
use			PoC.simulation.all;


architecture tb of config_tb is
	signal SimQuiet		: BOOLEAN		:= true;
begin
	process
	begin
		if (SimQuiet = FALSE) then
			report "is simulation?: " & boolean'image(SIMULATION)								severity note;
			report "Vendor:         " & vendor_t'image(VENDOR)									severity note;
			report "Device:         " & device_t'image(DEVICE)									severity note;
			report "Device Number:  " & integer'image(DEVICE_NUMBER)						severity note;
			report "Device Subtype: " & T_DEVICE_SUBTYPE'image(DEVICE_SUBTYPE)	severity note;
			report "Device Series:  " & integer'image(DEVICE_SERIES)						severity note;
			report "--------------------------------------------------"					severity note;
			report "LUT fan-in:     " & integer'image(LUT_FANIN)								severity note;
			report "Transceiver:    " & T_TRANSCEIVER'image(TRANSCEIVER_TYPE)		severity note;
		end if;

		tbAssert((SIMULATION = TRUE),											"SIMULATION="				& BOOLEAN'image(SIMULATION)											&	"    Expected=TRUE");
		tbAssert((VENDOR = VENDOR_XILINX),								"VENDOR= "					& VENDOR_T'image(VENDOR)												&	"    Expected=VENDOR_XILINX");
		tbAssert((DEVICE = DEVICE_KINTEX7),								"DEVICE="						& DEVICE_T'image(DEVICE)												&	"    Expected=DEVICE_KINTEX7");
		tbAssert((DEVICE_FAMILY = DEVICE_FAMILY_KINTEX),	"DEVICE_FAMILY="		& T_DEVICE_FAMILY'image(DEVICE_FAMILY)					&	"    Expected=DEVICE_FAMILY_KINTEX");
		tbAssert((DEVICE_NUMBER = 325),										"DEVICE_NUMBER="		& INTEGER'image(DEVICE_NUMBER)									&	"    Expected=325");
		tbAssert((DEVICE_SUBTYPE = DEVICE_SUBTYPE_T),			"DEVICE_SUBTYPE="		& T_DEVICE_SUBTYPE'image(DEVICE_SUBTYPE)				&	"    Expected=DEVICE_SUBTYPE_T");
		tbAssert((DEVICE_SERIES = 7),											"DEVICE_SERIES="		& INTEGER'image(DEVICE_SERIES)									&	"    Expected=7");
		tbAssert((LUT_FANIN = 6),													"LUT_FANIN="				& INTEGER'image(LUT_FANIN)											&	"    Expected=6");
		tbAssert((TRANSCEIVER_TYPE = TRANSCEIVER_GTXE2),	"TRANSCEIVER_TYPE="	& T_TRANSCEIVER'image(TRANSCEIVER_TYPE)					&	"    Expected=TRANSCEIVER_GTXE2");
		
		-- simulation completed
		
		-- Report overall simulation result
		tbPrintResult;
		wait;
	end process;
end;
