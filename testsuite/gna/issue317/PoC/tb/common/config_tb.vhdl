-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
--
-- =============================================================================
-- Authors:					Thomas B. Preusser
--									Patrick Lehmann
--
-- Testbench:				Tests global constants, functions and settings
--
-- Description:
-- ------------------------------------
--		TODO
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany
--										 Chair of VLSI-Design, Diagnostics and Architecture
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

library	PoC;
use			PoC.config.all;
use			PoC.utils.all;
-- simulation only packages
use			PoC.sim_types.all;
use			PoC.simulation.all;


entity config_tb is
end config_tb;


architecture tb of config_tb is
	signal SimQuiet		: boolean		:= true;
begin

	procChecker : process
		constant simProcessID	: T_SIM_PROCESS_ID := simRegisterProcess("Checker");
	begin
		if not SimQuiet then
			report "is simulation?:    " & boolean'image(SIMULATION)							severity note;
			report "Vendor:            " & T_VENDOR'image(VENDOR)									severity note;
			report "Device:            " & T_DEVICE'image(DEVICE)									severity note;
			report "Device Family:     " & T_DEVICE_FAMILY'image(DEVICE_FAMILY)		severity note;
			report "Device Subtype:    " & T_DEVICE_SUBTYPE'image(DEVICE_SUBTYPE)	severity note;
			report "Device Series:     " & T_DEVICE_SERIES'image(DEVICE_SERIES)		severity note;
			report "Device Generation: " & integer'image(DEVICE_GENERATION)				severity note;
			report "Device Number:     " & integer'image(DEVICE_NUMBER)						severity note;
			report "--------------------------------------------------"						severity note;
			report "LUT fan-in:        " & integer'image(LUT_FANIN)								severity note;
			report "Transceiver:       " & T_TRANSCEIVER'image(TRANSCEIVER_TYPE)	severity note;
		end if;

		simAssertion((SIMULATION = TRUE),												"SIMULATION="					& boolean'image(SIMULATION)											&	"    Expected=TRUE");
		simAssertion((VENDOR = VENDOR_GENERIC),									"VENDOR= "						& T_VENDOR'image(VENDOR)												&	"    Expected=VENDOR_XILINX");
		simAssertion((DEVICE = DEVICE_GENERIC),									"DEVICE="							& T_DEVICE'image(DEVICE)												&	"    Expected=DEVICE_KINTEX7");
		simAssertion((DEVICE_FAMILY = DEVICE_FAMILY_GENERIC),		"DEVICE_FAMILY="			& T_DEVICE_FAMILY'image(DEVICE_FAMILY)					&	"    Expected=DEVICE_FAMILY_KINTEX");
		simAssertion((DEVICE_NUMBER = 0),												"DEVICE_NUMBER="			& integer'image(DEVICE_NUMBER)									&	"    Expected=325");
		simAssertion((DEVICE_SUBTYPE = DEVICE_SUBTYPE_GENERIC),	"DEVICE_SUBTYPE="			& T_DEVICE_SUBTYPE'image(DEVICE_SUBTYPE)				&	"    Expected=DEVICE_SUBTYPE_T");
		simAssertion((DEVICE_GENERATION = 0),										"DEVICE_GENERATION="	& integer'image(DEVICE_GENERATION)							&	"    Expected=7");
		simAssertion((DEVICE_SERIES = DEVICE_SERIES_GENERIC),		"DEVICE_SERIES="			& T_DEVICE_SERIES'image(DEVICE_SERIES)					&	"    Expected=DEVICE_SERIES_7_SERIES");
		simAssertion((LUT_FANIN = 6),														"LUT_FANIN="					& integer'image(LUT_FANIN)											&	"    Expected=6");
		simAssertion((TRANSCEIVER_TYPE = TRANSCEIVER_GENERIC),	"TRANSCEIVER_TYPE="		& T_TRANSCEIVER'image(TRANSCEIVER_TYPE)					&	"    Expected=TRANSCEIVER_GTXE2");

		-- This process is finished
		simDeactivateProcess(simProcessID);
		wait;  -- forever
	end process;
end architecture;
