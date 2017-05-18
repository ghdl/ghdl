-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Package:					Global configuration settings.
--
-- Description:
-- -------------------------------------
--		This file evaluates the settings declared in the project specific package my_config.
--		See also template file my_config.vhdl.template.
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany,
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

library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library PoC;
use			PoC.utils.all;

package config_private is
	-- TODO:
	-- ===========================================================================
	subtype T_BOARD_STRING					is string(1 to 16);
	subtype T_BOARD_CONFIG_STRING		is string(1 to 64);
	subtype T_DEVICE_STRING					is string(1 to 32);

	-- Data structures to describe UART / RS232
	type T_BOARD_UART_DESC is record
		IsDTE											: boolean;									-- Data terminal Equipment (e.g. PC, Printer)
		FlowControl								: T_BOARD_CONFIG_STRING;		-- (NONE, SW, HW_CTS_RTS, HW_RTR_RTS)
		BaudRate									: T_BOARD_CONFIG_STRING;		-- e.g. "115.2 kBd"
		BaudRate_Max							: T_BOARD_CONFIG_STRING;
	end record;

	-- Data structures to describe Ethernet
	type T_BOARD_ETHERNET_DESC is record
		IPStyle										: T_BOARD_CONFIG_STRING;
		RS_DataInterface					: T_BOARD_CONFIG_STRING;
		PHY_Device								: T_BOARD_CONFIG_STRING;
		PHY_DeviceAddress					: std_logic_vector(7 downto 0);
		PHY_DataInterface					: T_BOARD_CONFIG_STRING;
		PHY_ManagementInterface		: T_BOARD_CONFIG_STRING;
	end record;

	subtype T_BOARD_ETHERNET_DESC_INDEX		is natural range 0 to 7;
	type		T_BOARD_ETHERNET_DESC_VECTOR	is array(natural range <>) of T_BOARD_ETHERNET_DESC;

	-- Data structures to describe a board layout
	type T_BOARD_INFO is record
		BoardName			: T_BOARD_CONFIG_STRING;
		FPGADevice		: T_BOARD_CONFIG_STRING;
		UART					: T_BOARD_UART_DESC;
		Ethernet			: T_BOARD_ETHERNET_DESC_VECTOR(T_BOARD_ETHERNET_DESC_INDEX);
		EthernetCount	: T_BOARD_ETHERNET_DESC_INDEX;
	end record;

	type T_BOARD_INFO_VECTOR	is array (natural range <>) of T_BOARD_INFO;

	constant C_POC_NUL										: character;
	constant C_BOARD_STRING_EMPTY					: T_BOARD_STRING;
	constant C_BOARD_CONFIG_STRING_EMPTY	: T_BOARD_CONFIG_STRING;
	constant C_DEVICE_STRING_EMPTY				: T_DEVICE_STRING;
	constant C_BOARD_INFO_LIST						: T_BOARD_INFO_VECTOR;

	function conf(str : string) return T_BOARD_CONFIG_STRING;
end package;


package body config_private is
	constant C_POC_NUL										: character								:= '~';
	constant C_BOARD_STRING_EMPTY					: T_BOARD_STRING					:= (others => C_POC_NUL);
	constant C_BOARD_CONFIG_STRING_EMPTY	: T_BOARD_CONFIG_STRING		:= (others => C_POC_NUL);
	constant C_DEVICE_STRING_EMPTY				: T_DEVICE_STRING					:= (others => C_POC_NUL);

	function conf(str : string) return T_BOARD_CONFIG_STRING is
		constant ConstNUL		: string(1 to 1)				:= (others => C_POC_NUL);
		variable Result			: string(1 to T_BOARD_CONFIG_STRING'length);
	begin
		Result := (others => C_POC_NUL);
		if (str'length > 0) then
			Result(1 to bound(T_BOARD_CONFIG_STRING'length, 1, str'length)) := ite((str'length > 0), str(1 to imin(T_BOARD_CONFIG_STRING'length, str'length)), ConstNUL);
		end if;
		return Result;
	end function;

	constant C_BOARD_ETHERNET_DESC_EMPTY	: T_BOARD_ETHERNET_DESC		:= (
		IPStyle										=> C_BOARD_CONFIG_STRING_EMPTY,
		RS_DataInterface					=> C_BOARD_CONFIG_STRING_EMPTY,
		PHY_Device								=> C_BOARD_CONFIG_STRING_EMPTY,
		PHY_DeviceAddress					=> x"00",
		PHY_DataInterface					=> C_BOARD_CONFIG_STRING_EMPTY,
		PHY_ManagementInterface		=> C_BOARD_CONFIG_STRING_EMPTY
	);

	-- predefined UART descriptions
	function brd_CreateUART(IsDTE : boolean; FlowControl : string; BaudRate : string; BaudRate_Max : string := "") return T_BOARD_UART_DESC is
		variable Result			: T_BOARD_UART_DESC;
	begin
		Result.IsDTE				:= IsDTE;
		Result.FlowControl	:= conf(FlowControl);
		Result.BaudRate			:= conf(BaudRate);
		Result.BaudRate_Max	:= ite((BaudRate_Max = ""), conf(BaudRate), conf(BaudRate_Max));
		return Result;
	end function;

	--																																					IsDTE		FlowControl			BaudRate
	constant C_BOARD_UART_EMPTY							: T_BOARD_UART_DESC	:= brd_CreateUART(TRUE,		"NONE",				"0 Bd");
	constant C_BOARD_UART_DTE_115200_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(TRUE,		"NONE",				"115.2 kBd");
	constant C_BOARD_UART_DCE_115200_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"NONE",				"115.2 kBd");
	constant C_BOARD_UART_DCE_115200_HWCTS	: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"HW_CTS_RTS",	"115.2 kBd");
	constant C_BOARD_UART_DCE_460800_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"NONE",				"460.8 kBd");
	constant C_BOARD_UART_DTE_921600_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"NONE",				"921.6 kBd");

	function brd_CreateEthernet(IPStyle : string; RS_DataInt : string; PHY_Device : string; PHY_DevAddress : std_logic_vector(7 downto 0); PHY_DataInt : string; PHY_MgntInt : string) return T_BOARD_ETHERNET_DESC is
		variable Result		: T_BOARD_ETHERNET_DESC;
	begin
		Result.IPStyle									:= conf(IPStyle);
		Result.RS_DataInterface					:= conf(RS_DataInt);
		Result.PHY_Device								:= conf(PHY_Device);
		Result.PHY_DeviceAddress				:= PHY_DevAddress;
		Result.PHY_DataInterface				:= conf(PHY_DataInt);
		Result.PHY_ManagementInterface	:= conf(PHY_MgntInt);
		return Result;
	end function;

	constant C_BOARD_ETH_EMPTY							: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("", "", "", x"00", "", "");
	constant C_BOARD_ETH_SOFT_GMII_88E1111	: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"07", "GMII", "MDIO");
	constant C_BOARD_ETH_HARD_GMII_88E1111	: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("HARD", "GMII", "MARVEL_88E1111", x"07", "GMII", "MDIO");
	constant C_BOARD_ETH_SOFT_SGMII_88E1111	: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"07", "SGMII", "MDIO_OVER_IIC");

	constant C_BOARD_ETH_NONE		: T_BOARD_ETHERNET_DESC_VECTOR(T_BOARD_ETHERNET_DESC_INDEX)	:= (others => C_BOARD_ETH_EMPTY);


	-- Board Descriptions
	-- ===========================================================================
	constant C_BOARD_INFO_LIST		: T_BOARD_INFO_VECTOR		:= (
		(
			BoardName =>			conf("GENERIC"),
			FPGADevice =>			conf("GENERIC"),											-- GENERIC
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet =>				(
				0 => C_BOARD_ETH_HARD_GMII_88E1111,
				others => C_BOARD_ETH_EMPTY
			),
			EthernetCount =>	1
		),
	-- Altera boards
		-- =========================================================================
		(
			BoardName =>			conf("DE0"),
			FPGADevice =>			conf("EP3C16F484"),										-- EP3C16F484
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("S2GXAV"),
			FPGADevice =>			conf("EP2SGX90FF1508C3"),							-- EP2SGX90FF1508C3
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("DE4"),
			FPGADevice =>			conf("EP4SGX230KF40C2"),							-- EP4SGX230KF40C2
			UART =>						C_BOARD_UART_DCE_460800_NONE,
			Ethernet => (
				0 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"00", "RGMII", "MDIO"),
				1 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"01", "RGMII", "MDIO"),
				2 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"02", "RGMII", "MDIO"),
				3 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"03", "RGMII", "MDIO"),
				others => C_BOARD_ETH_EMPTY
			),
			EthernetCount =>	4
		),(
			BoardName =>			conf("DE5"),
			FPGADevice =>			conf("EP5SGXEA7N2F45C2"),							-- EP5SGXEA7N2F45C2
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		-- Lattice boards
		-- =========================================================================
		(
			BoardName =>			conf("ECP5 Versa"),
			FPGADevice => 		conf("LFE5UM-45F-6BG381C"),						-- LFE5UM-45F-6BG381C
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		-- Xilinx boards
		-- =========================================================================
		(
			BoardName =>			conf("S3SK200"),
			FPGADevice => 		conf("XC3S200-4FT256"),								-- XC3S200-4FT256
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("S3SK1000"),
			FPGADevice =>			conf("XC3S1000-4FT256"),							-- XC2S1000-4FT256
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("S3ESK500"),
			FPGADevice =>			conf("XC3S500E-4FG320"),							-- XC3S500E-4FG320
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("S3ESK1600"),
			FPGADevice =>			conf("XC3S1600E-4FG320"),							-- XC3S1600E-4FG320
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("ATLYS"),
			FPGADevice =>			conf("XC6SLX45-3CSG324"),							-- XC6SLX45-3CSG324
			UART =>						C_BOARD_UART_DCE_460800_NONE,
			Ethernet =>	(
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("ZC706"),
			FPGADevice =>			conf("XC7Z045-2FFG900"),							-- XC7Z045-2FFG900C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("ZedBoard"),
			FPGADevice =>			conf("XC7Z020-1CLG484"),							-- XC7Z020-1CLG484
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),(
			BoardName =>			conf("AC701"),
			FPGADevice =>			conf("XC7A200T-2FBG676C"),						-- XC7A200T-2FBG676C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_SOFT_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("KC705"),
			FPGADevice =>			conf("XC7K325T-2FFG900C"),						-- XC7K325T-2FFG900C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_SOFT_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("ML505"),
			FPGADevice =>			conf("XC5VLX50T-1FF1136"),						-- XC5VLX50T-1FF1136
			UART =>						C_BOARD_UART_DCE_115200_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("ML506"),
			FPGADevice =>			conf("XC5VSX50T-1FFG1136"),						-- XC5VSX50T-1FFG1136
			UART =>						C_BOARD_UART_DCE_115200_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("ML507"),
			FPGADevice =>			conf("XC5VFX70T-1FFG1136"),						-- XC5VFX70T-1FFG1136
			UART =>						C_BOARD_UART_DCE_115200_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("XUPV5"),
			FPGADevice =>			conf("XC5VLX110T-1FF1136"),						-- XC5VLX110T-1FF1136
			UART =>						C_BOARD_UART_DCE_115200_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("ML605"),
			FPGADevice =>			conf("XC6VLX240T-1FF1156"),						-- XC6VLX240T-1FF1156
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("VC707"),
			FPGADevice =>			conf("XC7VX485T-2FFG1761C"),					-- XC7VX485T-2FFG1761C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_SOFT_SGMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),(
			BoardName =>			conf("VC709"),
			FPGADevice =>			conf("XC7VX690T-2FFG1761C"),					-- XC7VX690T-2FFG1761C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		-- Custom Board (MUST BE LAST ONE)
		-- =========================================================================
		(
			BoardName =>			conf("Custom"),
			FPGADevice =>			conf("Device is unknown for a custom board"),
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		)
	);
end package body;


library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library	PoC;
use			PoC.my_config.all;
use			PoC.my_project.all;
use 		PoC.config_private.all;
use			PoC.utils.all;


package config is
	constant PROJECT_DIR			: string	:= MY_PROJECT_DIR;
	constant OPERATING_SYSTEM	: string	:= MY_OPERATING_SYSTEM;
	constant POC_VERBOSE			: boolean := MY_VERBOSE;

	-- List of known FPGA / Chip vendors
	-- ---------------------------------------------------------------------------
	type T_VENDOR is (
		VENDOR_UNKNOWN,
		VENDOR_GENERIC,
		VENDOR_ALTERA,
		VENDOR_LATTICE,
		VENDOR_XILINX
	);

	-- List of known synthesis tool chains
	-- ---------------------------------------------------------------------------
	type T_SYNTHESIS_TOOL is (
		SYNTHESIS_TOOL_UNKNOWN,
		SYNTHESIS_TOOL_GENERIC,
		SYNTHESIS_TOOL_ALTERA_QUARTUS2,
		SYNTHESIS_TOOL_LATTICE_LSE,
		SYNTHESIS_TOOL_SYNOPSIS,
		SYNTHESIS_TOOL_XILINX_XST,
		SYNTHESIS_TOOL_XILINX_VIVADO
	);

	-- List of known device families
	-- ---------------------------------------------------------------------------
	type T_DEVICE_FAMILY is (
		DEVICE_FAMILY_UNKNOWN,
		DEVICE_FAMILY_GENERIC,
		-- Altera
		DEVICE_FAMILY_ARRIA,
		DEVICE_FAMILY_CYCLONE,
		DEVICE_FAMILY_STRATIX,
		-- Lattice
		DEVICE_FAMILY_ICE,
		DEVICE_FAMILY_MACHXO,
		DEVICE_FAMILY_ECP,
		-- Xilinx
		DEVICE_FAMILY_SPARTAN,
		DEVICE_FAMILY_ZYNQ,
		DEVICE_FAMILY_ARTIX,
		DEVICE_FAMILY_KINTEX,
		DEVICE_FAMILY_VIRTEX
	);

	type T_DEVICE_SERIES is (
		DEVICE_SERIES_UNKNOWN,
		DEVICE_SERIES_GENERIC,
		-- Xilinx FPGA series
		DEVICE_SERIES_7_SERIES,
		DEVICE_SERIES_ULTRASCALE,
		DEVICE_SERIES_ULTRASCALE_PLUS
	);

	-- List of known devices
	-- ---------------------------------------------------------------------------
	type T_DEVICE is (
		DEVICE_UNKNOWN,
		DEVICE_GENERIC,
		-- Altera
		DEVICE_MAX2, DEVICE_MAX10,																					-- Altera.Max
		DEVICE_ARRIA1, DEVICE_ARRIA2, DEVICE_ARRIA5, DEVICE_ARRIA10,				-- Altera.Arria
		DEVICE_CYCLONE1, DEVICE_CYCLONE2, DEVICE_CYCLONE3, DEVICE_CYCLONE4,	-- Altera.Cyclone
			DEVICE_CYCLONE5,																									--
		DEVICE_STRATIX1, DEVICE_STRATIX2, DEVICE_STRATIX3, DEVICE_STRATIX4,	-- Altera.Stratix
			DEVICE_STRATIX5, DEVICE_STRATIX10,																--
		-- Lattice
		DEVICE_ICE40, DEVICE_ICE65, DEVICE_ICE5,														-- Lattice.iCE
		DEVICE_MACHXO, DEVICE_MACHXO2,																			-- Lattice.MachXO
		DEVICE_ECP3, DEVICE_ECP4, DEVICE_ECP5,															-- Lattice.ECP
		-- Xilinx
		DEVICE_SPARTAN3, DEVICE_SPARTAN6,																		-- Xilinx.Spartan
		DEVICE_ZYNQ7, DEVICE_ZYNQ_ULTRA_PLUS,																-- Xilinx.Zynq
		DEVICE_ARTIX7,																											-- Xilinx.Artix
		DEVICE_KINTEX7, DEVICE_KINTEX_ULTRA, DEVICE_KINTEX_ULTRA_PLUS,			-- Xilinx.Kintex
		DEVICE_VIRTEX4, DEVICE_VIRTEX5,	DEVICE_VIRTEX6, DEVICE_VIRTEX7,			-- Xilinx.Virtex
			DEVICE_VIRTEX_ULTRA, DEVICE_VIRTEX_ULTRA_PLUS											--
	);

	-- List of known device subtypes
	-- ---------------------------------------------------------------------------
	type T_DEVICE_SUBTYPE is (
		DEVICE_SUBTYPE_NONE,
		DEVICE_SUBTYPE_GENERIC,
		-- Altera
		DEVICE_SUBTYPE_E,
		DEVICE_SUBTYPE_GS,
		DEVICE_SUBTYPE_GX,
		DEVICE_SUBTYPE_GT,
		-- Lattice
		DEVICE_SUBTYPE_U,
		DEVICE_SUBTYPE_UM,
		-- Xilinx
		DEVICE_SUBTYPE_X,
		DEVICE_SUBTYPE_T,
		DEVICE_SUBTYPE_XT,
		DEVICE_SUBTYPE_HT,
		DEVICE_SUBTYPE_LX,
		DEVICE_SUBTYPE_SXT,
		DEVICE_SUBTYPE_LXT,
		DEVICE_SUBTYPE_TXT,
		DEVICE_SUBTYPE_FXT,
		DEVICE_SUBTYPE_CXT,
		DEVICE_SUBTYPE_HXT
	);

	-- List of known transceiver (sub-)types
	-- ---------------------------------------------------------------------------
	type T_TRANSCEIVER is (
		TRANSCEIVER_NONE,
		TRANSCEIVER_GENERIC,
		-- TODO: add more? Altera transceivers
		-- Altera transceivers
		TRANSCEIVER_GXB,																										-- Altera GXB transceiver
		--Lattice transceivers
		TRANSCEIVER_MGT,																										-- Lattice transceiver
		-- Xilinx transceivers
		TRANSCEIVER_GTP_DUAL,	TRANSCEIVER_GTPE1, TRANSCEIVER_GTPE2,					-- Xilinx GTP transceivers
		TRANSCEIVER_GTX,			TRANSCEIVER_GTXE1, TRANSCEIVER_GTXE2,					-- Xilinx GTX transceivers
		TRANSCEIVER_GTH,			TRANSCEIVER_GTHE1, TRANSCEIVER_GTHE2,					-- Xilinx GTH transceivers
		TRANSCEIVER_GTZ,																										-- Xilinx GTZ transceivers
		TRANSCEIVER_GTY																											-- Xilinx GTY transceivers
	);

	-- Properties of an FPGA architecture
	-- ===========================================================================
	type T_DEVICE_INFO is record
		Vendor						: T_VENDOR;
		Device						: T_DEVICE;
		DevFamily					: T_DEVICE_FAMILY;
		DevGeneration			: natural;
		DevNumber					: natural;
		DevSubType				: T_DEVICE_SUBTYPE;
		DevSeries					: T_DEVICE_SERIES;

		TransceiverType		: T_TRANSCEIVER;
		LUT_FanIn					: positive;
	end record;

	-- Functions extracting board and PCB properties from "MY_BOARD"
	-- which is declared in package "my_config".
	-- ===========================================================================
	function BOARD(BoardConfig : string := C_BOARD_STRING_EMPTY)								return natural;
	function BOARD_INFO(BoardConfig : string := C_BOARD_STRING_EMPTY)						return T_BOARD_INFO;
	function BOARD_NAME(BoardConfig : string := C_BOARD_STRING_EMPTY) 					return string;
	function BOARD_DEVICE(BoardConfig : string := C_BOARD_STRING_EMPTY) 				return string;
	function BOARD_UART_BAUDRATE(BoardConfig : string := C_BOARD_STRING_EMPTY)	return string;

	-- Functions extracting device and architecture properties from "MY_DEVICE"
	-- which is declared in package "my_config".
	-- ===========================================================================
	function VENDOR(DeviceString : string := C_DEVICE_STRING_EMPTY)							return T_VENDOR;
	function SYNTHESIS_TOOL(DeviceString : string := C_DEVICE_STRING_EMPTY)			return T_SYNTHESIS_TOOL;
	function DEVICE(DeviceString : string := C_DEVICE_STRING_EMPTY)							return T_DEVICE;
	function DEVICE_FAMILY(DeviceString : string := C_DEVICE_STRING_EMPTY)			return T_DEVICE_FAMILY;
	function DEVICE_SUBTYPE(DeviceString : string := C_DEVICE_STRING_EMPTY)			return T_DEVICE_SUBTYPE;
	function DEVICE_SERIES(DeviceString : string := C_DEVICE_STRING_EMPTY)			return T_DEVICE_SERIES;
	function DEVICE_GENERATION(DeviceString : string := C_DEVICE_STRING_EMPTY)	return natural;
	function DEVICE_NUMBER(DeviceString : string := C_DEVICE_STRING_EMPTY)			return natural;

	function TRANSCEIVER_TYPE(DeviceString : string := C_DEVICE_STRING_EMPTY)		return T_TRANSCEIVER;
	function LUT_FANIN(DeviceString : string := C_DEVICE_STRING_EMPTY)					return positive;

	function DEVICE_INFO(DeviceString : string := C_DEVICE_STRING_EMPTY)				return T_DEVICE_INFO;

	-- Convert T_DEVICE to string representation as required by "altera_mf" library
	-- ===========================================================================
	function getAlteraDeviceName (device : T_DEVICE) return string;

	-- force FSM to predefined encoding in debug mode
	-- ===========================================================================
	function getFSMEncoding_gray(debug : boolean) return string;
end package;


package body config is
	-- inlined function from PoC.utils, to break dependency
	-- ===========================================================================
	function ite(cond : boolean; value1 : string; value2 : string) return string is begin
		if cond then	return value1;	else	return value2;	end if;
	end function;

	-- chr_is* function
	function chr_isDigit(chr : character) return boolean is
	begin
		return ((character'pos('0') <= CHARACTER'pos(chr)) and (character'pos(chr) <= CHARACTER'pos('9')));
	end function;

	function chr_isAlpha(chr : character) return boolean is
	begin
		return (((character'pos('a') <= CHARACTER'pos(chr)) and (character'pos(chr) <= CHARACTER'pos('z'))) or
						((character'pos('A') <= CHARACTER'pos(chr)) and (character'pos(chr) <= CHARACTER'pos('Z'))));
	end function;

	function str_length(str : string) return natural is
	begin
		for i in str'range loop
			if str(i) = C_POC_NUL then
				return i - str'low;
			end if;
		end loop;
		return str'length;
	end function;

	function str_trim(str : string) return string is
	begin
		for i in str'range loop
			if str(i) = C_POC_NUL then
				return str(str'low to i-1);
			end if;
		end loop;
		return str;
	end function;

	function str_imatch(str1 : string; str2 : string) return boolean is
		constant len	: natural 		:= imin(str1'length, str2'length);
		variable chr1	: character;
		variable chr2	: character;
	begin
		-- if both strings are empty
		if ((str1'length = 0 ) and (str2'length = 0)) then		return TRUE;	end if;
		-- compare char by char
		for i in 0 to len-1 loop
			chr1	:= str1(str1'low + i);
			chr2	:= str2(str2'low + i);
			if (character'pos('A') <= CHARACTER'pos(chr1)) and (character'pos(chr1) <= CHARACTER'pos('Z')) then
				chr1	:= character'val(CHARACTER'pos(chr1) - character'pos('A') + CHARACTER'pos('a'));
			end if;
			if (character'pos('A') <= CHARACTER'pos(chr2)) and (character'pos(chr2) <= CHARACTER'pos('Z')) then
				chr2	:= character'val(CHARACTER'pos(chr2) - character'pos('A') + CHARACTER'pos('a'));
			end if;
			if chr1 /= chr2 then
				return FALSE;
			elsif (chr1 = C_POC_NUL) xor (chr2 = C_POC_NUL) then
				return FALSE;
			elsif (chr1 = C_POC_NUL) and (chr2 = C_POC_NUL) then
				return TRUE;
			end if;
		end loop;
		-- check special cases,
		if ((str1'length = len) and (str2'length = len)) then 	-- both strings are fully consumed and equal
			return TRUE;
		elsif (str1'length > len) then
			return (str1(str1'low + len) = C_POC_NUL);						-- str1 is longer, but str_length equals len
		else
      return (str2(str2'low + len) = C_POC_NUL);						-- str2 is longer, but str_length equals len
		end if;
	end function;

	function str_find(str : string; pattern : string; start : natural := 0) return boolean is
	begin
		for i in imax(str'low, start) to (str'high - pattern'length + 1) loop
			exit when (str(i) = C_POC_NUL);
			if (str(i to i + pattern'length - 1) = pattern) then
				return TRUE;
			end if;
		end loop;
		return FALSE;
	end function;

	-- private functions required by board description
	-- ModelSim requires that this functions is defined before it is used below.
	-- ===========================================================================
	function getLocalDeviceString(DeviceString : string) return string is
		constant ConstNUL				: string(1 to 1)				:= (others => C_POC_NUL);
		constant MY_DEVICE_STR	: string								:= BOARD_DEVICE;
		variable Result					: string(1 to T_DEVICE_STRING'length);
	begin
		Result := (others => C_POC_NUL);
		-- report DeviceString for debugging
		if POC_VERBOSE then
			report "getLocalDeviceString: DeviceString='" & str_trim(DeviceString) & "' MY_DEVICE='" & str_trim(MY_DEVICE) & "' MY_DEVICE_STR='" & str_trim(MY_DEVICE_STR) & "'"  severity NOTE;
		end if;
		-- if DeviceString is populated
		if (str_length(DeviceString) /= 0) and not str_imatch(DeviceString, "None") then
			Result(1 to bound(T_DEVICE_STRING'length, 1, DeviceString'length))	:= ite((DeviceString'length > 0), DeviceString(1 to imin(T_DEVICE_STRING'length, DeviceString'length)), ConstNUL);
		-- if MY_DEVICE is set, prefer it
		elsif (str_length(MY_DEVICE) /= 0) and not str_imatch(MY_DEVICE, "None") then
			Result(1 to bound(T_DEVICE_STRING'length, 1, MY_DEVICE'length))			:= ite((MY_DEVICE'length > 0), MY_DEVICE(1 to imin(T_DEVICE_STRING'length, MY_DEVICE'length)), ConstNUL);
		-- otherwise use MY_BOARD
		else
			Result(1 to bound(T_DEVICE_STRING'length, 1, MY_DEVICE_STR'length))	:= ite((MY_DEVICE_STR'length > 0), MY_DEVICE_STR(1 to imin(T_DEVICE_STRING'length, MY_DEVICE_STR'length)), ConstNUL);
		end if;
		return Result;
	end function;

	function extractFirstNumber(str : string) return natural is
		variable low			: integer;
		variable high			: integer;
		variable Result		: natural;
		variable Digit		: integer;
	begin
		low			:= -1;
		high		:= -1;
		for i in str'low to str'high loop
			if chr_isDigit(str(i)) then
				low := i;
				exit;
			end if;
		end loop;
		-- abort if no digit can be found
		if low = -1 then		return 0; end if;

		for i in (low + 1) to str'high loop
			if chr_isAlpha(str(i)) then
				high := i - 1;
				exit;
			end if;
		end loop;

		if high = -1 then		return 0; end if;
		-- return INTEGER'value(str(low to high));			-- 'value(...) is not supported by Vivado Synth 2014.1

		-- convert substring to a number
		for i in low to high loop
			if not chr_isDigit(str(i)) then
				return 0;
			end if;
			Result	:= (Result * 10) + (character'pos(str(i)) - character'pos('0'));
		end loop;
		return Result;
	end function;

	-- Public functions
	-- ===========================================================================
	-- TODO: comment
	function BOARD(BoardConfig : string := C_BOARD_STRING_EMPTY) return natural is
		constant MY_BRD			: T_BOARD_CONFIG_STRING	:= ite((BoardConfig /= C_BOARD_STRING_EMPTY), conf(BoardConfig), conf(MY_BOARD));
		constant BOARD_NAME	: string								:= str_trim(MY_BRD);
	begin
		if POC_VERBOSE then	report "PoC configuration: Used board is '" & BOARD_NAME & "'" severity NOTE;		end if;
		for i in C_BOARD_INFO_LIST'range loop
			if str_imatch(BOARD_NAME, C_BOARD_INFO_LIST(i).BoardName) then
				return  i;
			end if;
		end loop;

		report "Unknown board name in MY_BOARD = " & MY_BRD & "." severity failure;
		return C_BOARD_INFO_LIST'high;
	end function;

	function BOARD_INFO(BoardConfig : string := C_BOARD_STRING_EMPTY) return T_BOARD_INFO is
		constant BRD	: natural := BOARD(BoardConfig);
  begin
		return  C_BOARD_INFO_LIST(BRD);
	end function;

	-- TODO: comment
	function BOARD_NAME(BoardConfig : string := C_BOARD_STRING_EMPTY) return string is
		constant BRD	: natural := BOARD(BoardConfig);
  begin
		return str_trim(C_BOARD_INFO_LIST(BRD).BoardName);
	end function;

	-- TODO: comment
	function BOARD_DEVICE(BoardConfig : string := C_BOARD_STRING_EMPTY) return string is
		constant BRD	: natural := BOARD(BoardConfig);
  begin
		return str_trim(C_BOARD_INFO_LIST(BRD).FPGADevice);
	end function;

	function BOARD_UART_BAUDRATE(BoardConfig : string := C_BOARD_STRING_EMPTY) return string is
		constant BRD	: natural := BOARD(BoardConfig);
  begin
		return str_trim(C_BOARD_INFO_LIST(BRD).UART.BaudRate);
	end function;

	-- purpose: extract vendor from MY_DEVICE
	function VENDOR(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_VENDOR is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN_STR2	: string(1 to 2)  := MY_DEV(1 to 2);			-- TODO: test if alias declarations also work out on all platforms
		constant VEN_STR3	: string(1 to 3)  := MY_DEV(1 to 3);			-- TODO: test if alias declarations also work out on all platforms
	begin
		case VEN_STR2 is
			when "GE" =>		return VENDOR_GENERIC;
			when "EP" =>		return VENDOR_ALTERA;
			when "XC" =>		return VENDOR_XILINX;
			when others =>	null;
		end case;
		case VEN_STR3 is
			when "iCE" =>		return VENDOR_LATTICE;		-- iCE devices
			when "LCM" =>		return VENDOR_LATTICE;		-- MachXO device
			when "LFE" =>		return VENDOR_LATTICE;		-- ECP devices
			when others =>	report "Unknown vendor in MY_DEVICE = '" & MY_DEV & "'" severity failure;
											return VENDOR_UNKNOWN;
		end case;
	end function;

	function SYNTHESIS_TOOL(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_SYNTHESIS_TOOL is
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
	begin
		case VEN is
			when VENDOR_GENERIC =>
				return SYNTHESIS_TOOL_GENERIC;
			when VENDOR_ALTERA =>
				return SYNTHESIS_TOOL_ALTERA_QUARTUS2;
			when VENDOR_LATTICE =>
				return SYNTHESIS_TOOL_LATTICE_LSE;
				--return SYNTHESIS_TOOL_SYNOPSIS;
			when VENDOR_XILINX =>
				if (1 fs /= 1 us) then
					return SYNTHESIS_TOOL_XILINX_XST;
				else
					return SYNTHESIS_TOOL_XILINX_VIVADO;
				end if;
			when others =>
				return SYNTHESIS_TOOL_UNKNOWN;
		end case;
	end function;

	-- purpose: extract device from MY_DEVICE
	function DEVICE(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
		constant DEV_STR	: string(3 to  4)	:= MY_DEV(3 to 4);			-- TODO: test if alias declarations also work out on all platforms
	begin
		case VEN is
			when VENDOR_GENERIC =>
				if		(MY_DEV(1 to 7) = "GENERIC") then	return DEVICE_GENERIC;
				else	report "Unknown Generic device in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;
			when VENDOR_ALTERA =>
				case DEV_STR is
					when "1C"	 =>		return DEVICE_CYCLONE1;
					when "2C"	 =>		return DEVICE_CYCLONE2;
					when "3C"	 =>		return DEVICE_CYCLONE3;
					when "1S"	 =>		return DEVICE_STRATIX1;
					when "2S"	 =>		return DEVICE_STRATIX2;
					when "4S"	 =>		return DEVICE_STRATIX4;
					when "5S"	 =>		return DEVICE_STRATIX5;
					when others =>	report "Unknown Altera device in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when VENDOR_LATTICE =>
				if		(MY_DEV(1 to 6) = "LCMX02") then	return DEVICE_MACHXO2;
				elsif	(MY_DEV(1 to 5) = "LCMX0") then		return DEVICE_MACHXO;
				elsif	(MY_DEV(1 to 5) = "iCE40") then		return DEVICE_ICE40;
				elsif	(MY_DEV(1 to 5) = "iCE65") then		return DEVICE_ICE65;
				elsif	(MY_DEV(1 to 4) = "iCE5") then		return DEVICE_ICE5;
				elsif	(MY_DEV(1 to 4) = "LFE3") then		return DEVICE_ECP3;
				elsif	(MY_DEV(1 to 4) = "LFE4") then		return DEVICE_ECP4;
				elsif	(MY_DEV(1 to 4) = "LFE5") then		return DEVICE_ECP5;
				else	report "Unknown Lattice device in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when VENDOR_XILINX =>
				case DEV_STR is
					when "7A"	 =>		return DEVICE_ARTIX7;
					when "7K"	 =>		return DEVICE_KINTEX7;
					when "KU"	 =>		return DEVICE_KINTEX_ULTRA;
					when "3S"	 =>		return DEVICE_SPARTAN3;
					when "6S"	 =>		return DEVICE_SPARTAN6;
					when "4V"	 =>		return DEVICE_VIRTEX4;
					when "5V"	 =>		return DEVICE_VIRTEX5;
					when "6V"	 =>		return DEVICE_VIRTEX6;
					when "7V"	 =>		return DEVICE_VIRTEX7;
					when "VU"	 =>		return DEVICE_VIRTEX_ULTRA;
					when "7Z"	 =>		return DEVICE_ZYNQ7;
					when others =>	report "Unknown Xilinx device in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when others => report "Unknown vendor in MY_DEVICE = " & MY_DEV & "." severity failure;
		end case;
		return DEVICE_UNKNOWN;
	end function;

	-- purpose: extract device from MY_DEVICE
	function DEVICE_FAMILY(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE_FAMILY is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
		constant FAM_CHAR	: character				:= MY_DEV(4);
	begin
		case VEN is
			when VENDOR_GENERIC =>
													return DEVICE_FAMILY_GENERIC;
			when VENDOR_ALTERA =>
				case FAM_CHAR is
					when 'C' =>			return DEVICE_FAMILY_CYCLONE;
					when 'S' =>			return DEVICE_FAMILY_STRATIX;
					when others =>	report "Unknown Altera device family in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when VENDOR_LATTICE =>
				case FAM_CHAR is
					--when 'M' =>		return DEVICE_FAMILY_MACHXO;
					when 'E' =>			return DEVICE_FAMILY_ECP;
					when others =>	report "Unknown Lattice device family in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when VENDOR_XILINX =>
				case FAM_CHAR is
					when 'A' =>			return DEVICE_FAMILY_ARTIX;
					when 'K' =>			return DEVICE_FAMILY_KINTEX;
					when 'S' =>			return DEVICE_FAMILY_SPARTAN;
					when 'V' =>			return DEVICE_FAMILY_VIRTEX;
					when 'Z' =>			return DEVICE_FAMILY_ZYNQ;
					when others =>	report "Unknown Xilinx device family in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when others => report "Unknown vendor in MY_DEVICE = '" & MY_DEV & "'" severity failure;
		end case;
		return DEVICE_FAMILY_UNKNOWN;
	end function;

	-- some devices share some common features: e.g. XADC, BlockRAM, ...
	function DEVICE_SERIES(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE_SERIES is
		constant MY_DEV	: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant DEV		: T_DEVICE				:= DEVICE(DeviceString);
	begin
		case DEV is
			when DEVICE_GENERIC =>
				return DEVICE_SERIES_GENERIC;
			-- all Xilinx ****7 devices
			when DEVICE_ARTIX7 | DEVICE_KINTEX7 | DEVICE_VIRTEX7 | DEVICE_ZYNQ7 =>
				return DEVICE_SERIES_7_SERIES;
			-- all Xilinx ****UltraScale devices
			when DEVICE_KINTEX_ULTRA | DEVICE_VIRTEX_ULTRA =>
				return DEVICE_SERIES_ULTRASCALE;
			-- all Xilinx ****UltraScale+ devices
			when DEVICE_KINTEX_ULTRA_PLUS | DEVICE_VIRTEX_ULTRA_PLUS | DEVICE_ZYNQ_ULTRA_PLUS =>
				return DEVICE_SERIES_ULTRASCALE_PLUS;
			when others =>
				return DEVICE_SERIES_UNKNOWN;
		end case;
	end function;

	function DEVICE_GENERATION(DeviceString : string := C_DEVICE_STRING_EMPTY) return natural is
		constant SERIES		: T_DEVICE_SERIES		:= DEVICE_SERIES(DeviceString);
	begin
		if SERIES = DEVICE_SERIES_7_SERIES then
			return 7;
		else
			return 0;
		end if;
	end function;

	function DEVICE_NUMBER(DeviceString : string := C_DEVICE_STRING_EMPTY) return natural is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
	begin
		case VEN is
			when VENDOR_GENERIC =>	return 0;
			when VENDOR_ALTERA =>		return extractFirstNumber(MY_DEV(5 to MY_DEV'high));
			when VENDOR_LATTICE =>	return extractFirstNumber(MY_DEV(6 to MY_DEV'high));
			when VENDOR_XILINX =>		return extractFirstNumber(MY_DEV(5 to MY_DEV'high));
			when others =>					report "Unknown vendor in MY_DEVICE = '" & MY_DEV & "'" severity failure;
															return 0;
		end case;
	end function;

	function DEVICE_SUBTYPE(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE_SUBTYPE is
		constant MY_DEV				: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant DEV					: T_DEVICE				:= DEVICE(MY_DEV);
		constant DEV_SUB_STR	: string(1 to 2)	:= MY_DEV(5 to 6);																-- WORKAROUND: for GHDL
	begin
		case DEV is
			when DEVICE_GENERIC =>																															return DEVICE_SUBTYPE_GENERIC;
			-- TODO: extract Arria GX subtype
			when DEVICE_ARRIA1 =>
				report "TODO: parse Arria device subtype." severity failure;
				return DEVICE_SUBTYPE_NONE;
			-- TODO: extract ArriaII GX,GZ subtype
			when DEVICE_ARRIA2 =>
				report "TODO: parse ArriaII device subtype." severity failure;
				return DEVICE_SUBTYPE_NONE;
			-- TODO: extract ArriaV GX, GT, SX, GZ subtype
			when DEVICE_ARRIA5 =>
				report "TODO: parse ArriaV device subtype." severity failure;
				return DEVICE_SUBTYPE_NONE;
			-- TODO: extract Arria10 GX, GT, SX subtype
			when DEVICE_ARRIA10 =>
				report "TODO: parse Arria10 device subtype." severity failure;
				return DEVICE_SUBTYPE_NONE;
			-- Altera Cyclon I, II, III, IV, V devices have no subtype
			when DEVICE_CYCLONE1 | DEVICE_CYCLONE2 | DEVICE_CYCLONE3 | DEVICE_CYCLONE4 |
					 DEVICE_CYCLONE5 =>																															return DEVICE_SUBTYPE_NONE;

			when DEVICE_STRATIX2 =>
				if chr_isDigit(DEV_SUB_STR(1)) then																						return DEVICE_SUBTYPE_NONE;
				elsif DEV_SUB_STR = "GX" then																										return DEVICE_SUBTYPE_GX;
				else	report "Unknown Stratix II subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_STRATIX4 =>
				if		(DEV_SUB_STR(1) = 'E') then																									return DEVICE_SUBTYPE_E;
				elsif DEV_SUB_STR = "GX" then																										return DEVICE_SUBTYPE_GX;
--				elsif	(DEV_SUB_STR = "GT") then																										return DEVICE_SUBTYPE_GT;
				else	report "Unknown Stratix IV subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			-- TODO: extract StratixV subtype
			when DEVICE_STRATIX5 =>
				report "TODO: parse Stratix V device subtype." severity failure;
				return DEVICE_SUBTYPE_NONE;

			when DEVICE_ECP5 =>
				if		(DEV_SUB_STR(1) = 'U') then																									return DEVICE_SUBTYPE_U;
				elsif DEV_SUB_STR = "UM" then																										return DEVICE_SUBTYPE_UM;
				else	report "Unknown Lattice ECP5 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_SPARTAN3 =>
				report "TODO: parse Spartan3 / Spartan3E / Spartan3AN device subtype." severity failure;
				return DEVICE_SUBTYPE_NONE;

			when DEVICE_SPARTAN6 =>
				if		((DEV_SUB_STR = "LX") and (not	str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LX;
				elsif	((DEV_SUB_STR = "LX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LXT;
				else	report "Unknown Virtex-5 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

		  when DEVICE_VIRTEX4 =>
		    report "Unkown Virtex 4" severity failure;

			when DEVICE_VIRTEX5 =>
				if		((DEV_SUB_STR = "LX") and (not	str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LX;
				elsif	((DEV_SUB_STR = "LX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LXT;
				elsif	((DEV_SUB_STR = "SX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_SXT;
				elsif	((DEV_SUB_STR = "TX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_TXT;
				elsif	((DEV_SUB_STR = "FX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_FXT;
				else	report "Unknown Virtex-5 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_VIRTEX6 =>
				if		((DEV_SUB_STR = "LX") and (not	str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LX;
				elsif	((DEV_SUB_STR = "LX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LXT;
				elsif	((DEV_SUB_STR = "SX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_SXT;
				elsif	((DEV_SUB_STR = "CX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_CXT;
				elsif	((DEV_SUB_STR = "HX") and (			str_find(MY_DEV(7 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_HXT;
				else	report "Unknown Virtex-6 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_ARTIX7 =>
				if		(													(			str_find(MY_DEV(5 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_T;
				else	report "Unknown Artix-7 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_KINTEX7 =>
				if		(													(			str_find(MY_DEV(5 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_T;
				else	report "Unknown Kintex-7 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_KINTEX_ULTRA =>																															return DEVICE_SUBTYPE_NONE;
			when DEVICE_KINTEX_ULTRA_PLUS =>																												return DEVICE_SUBTYPE_NONE;

			when DEVICE_VIRTEX7 =>
				if		(														(		str_find(MY_DEV(5 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_T;
				elsif	((DEV_SUB_STR(1) = 'X') and (		str_find(MY_DEV(6 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_XT;
				elsif	((DEV_SUB_STR(1) = 'H') and (		str_find(MY_DEV(6 to MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_HT;
				else	report "Unknown Virtex-7 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_VIRTEX_ULTRA =>																															return DEVICE_SUBTYPE_NONE;
			when DEVICE_VIRTEX_ULTRA_PLUS =>																												return DEVICE_SUBTYPE_NONE;

			when DEVICE_ZYNQ7 =>																																		return DEVICE_SUBTYPE_NONE;
			when DEVICE_ZYNQ_ULTRA_PLUS =>																													return DEVICE_SUBTYPE_NONE;

			when others => report "Device sub-type is unknown for the given device." severity failure;
		end case;
		return DEVICE_SUBTYPE_NONE;
	end function;

	function LUT_FANIN(DeviceString : string := C_DEVICE_STRING_EMPTY) return positive is
		constant MY_DEV	: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant DEV		: T_DEVICE				:= DEVICE(DeviceString);
		constant SERIES	: T_DEVICE_SERIES	:= DEVICE_SERIES(DeviceString);
	begin
		case SERIES is
			when DEVICE_SERIES_GENERIC =>																			return 6;
			when DEVICE_SERIES_7_SERIES | DEVICE_SERIES_ULTRASCALE |
					 DEVICE_SERIES_ULTRASCALE_PLUS =>															return 6;
			when others => null;
		end case;
		case DEV is
			when DEVICE_CYCLONE1 | DEVICE_CYCLONE2 | DEVICE_CYCLONE3 =>				return 4;
			when DEVICE_STRATIX1 | DEVICE_STRATIX2 =>													return 4;
			when DEVICE_STRATIX4 | DEVICE_STRATIX5 =>													return 6;

			when DEVICE_ECP5 =>																								return 4;

			when DEVICE_SPARTAN3 =>																						return 4;
			when DEVICE_SPARTAN6 =>																						return 6;
			when DEVICE_VIRTEX4 | DEVICE_VIRTEX5 | DEVICE_VIRTEX6 =>					return 6;

			when others => report "LUT fan-in is unknown for the given device, using default (4)." severity failure;
										 return 4;
		end case;
	end function;

	function TRANSCEIVER_TYPE(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_TRANSCEIVER is
		constant MY_DEV		: string(1 to 32)		:= getLocalDeviceString(DeviceString);
		constant DEV			: T_DEVICE					:= DEVICE(DeviceString);
		constant DEV_NUM	: natural						:= DEVICE_NUMBER(DeviceString);
		constant DEV_SUB	: T_DEVICE_SUBTYPE	:= DEVICE_SUBTYPE(DeviceString);
	begin
		case DEV is
			when DEVICE_GENERIC =>																						return TRANSCEIVER_GENERIC;
			when DEVICE_MAX2 | DEVICE_MAX10 =>																return TRANSCEIVER_NONE;		-- Altera MAX II, 10 devices have no transceivers
			when DEVICE_CYCLONE1 | DEVICE_CYCLONE2 | DEVICE_CYCLONE3 =>				return TRANSCEIVER_NONE;		-- Altera Cyclon I, II, III devices have no transceivers

			when DEVICE_STRATIX2 =>						return TRANSCEIVER_GXB;
			when DEVICE_STRATIX4 =>						return TRANSCEIVER_GXB;
			--when DEVICE_STRATIX5 =>						return TRANSCEIVER_GXB;

			when DEVICE_ECP5 =>								return TRANSCEIVER_MGT;

			when DEVICE_SPARTAN3 =>						return TRANSCEIVER_NONE;		-- Xilinx Spartan3 devices have no transceivers
			when DEVICE_SPARTAN6 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_LX =>			return TRANSCEIVER_NONE;
					when DEVICE_SUBTYPE_LXT =>		return TRANSCEIVER_GTPE1;
					when others =>								report "Unknown Spartan-6 subtype: " & T_DEVICE_SUBTYPE'image(DEV_SUB) severity failure;
				end case;

			when DEVICE_VIRTEX4 =>
					report "Unknown Virtex-4" severity failure;

			when DEVICE_VIRTEX5 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_LX =>			return TRANSCEIVER_NONE;
					when DEVICE_SUBTYPE_SXT =>		return TRANSCEIVER_GTP_DUAL;
					when DEVICE_SUBTYPE_LXT =>		return TRANSCEIVER_GTP_DUAL;
					when DEVICE_SUBTYPE_TXT =>		return TRANSCEIVER_GTX;
					when DEVICE_SUBTYPE_FXT =>		return TRANSCEIVER_GTX;
					when others =>								report "Unknown Virtex-5 subtype: " & T_DEVICE_SUBTYPE'image(DEV_SUB) severity failure;
				end case;

			when DEVICE_VIRTEX6 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_LX =>			return TRANSCEIVER_NONE;
					when DEVICE_SUBTYPE_SXT =>		return TRANSCEIVER_GTXE1;
					when DEVICE_SUBTYPE_LXT =>		return TRANSCEIVER_GTXE1;
					when DEVICE_SUBTYPE_HXT =>		return TRANSCEIVER_GTXE1;
					when others =>								report "Unknown Virtex-6 subtype: " & T_DEVICE_SUBTYPE'image(DEV_SUB) severity failure;
				end case;

			when DEVICE_ARTIX7 =>							return TRANSCEIVER_GTPE2;
			when DEVICE_KINTEX7 =>						return TRANSCEIVER_GTXE2;
			when DEVICE_VIRTEX7 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_T =>			return TRANSCEIVER_GTXE2;
					when DEVICE_SUBTYPE_XT =>
						if DEV_NUM = 485 then			return TRANSCEIVER_GTXE2;
						else												return TRANSCEIVER_GTHE2;
						end if;
					when DEVICE_SUBTYPE_HT =>			return TRANSCEIVER_GTHE2;
					when others =>								report "Unknown Virtex-7 subtype: " & T_DEVICE_SUBTYPE'image(DEV_SUB) severity failure;
				end case;
			when DEVICE_ZYNQ7 =>
				case DEV_NUM is
					when 10 | 20 =>								return TRANSCEIVER_NONE;
					when 15 =>										return TRANSCEIVER_GTPE2;
					when others =>								return TRANSCEIVER_GTXE2;
				end case;

			when others => report "Unknown device." severity failure;
		end case;
		return TRANSCEIVER_NONE;
	end function;

	-- purpose: extract architecture properties from DEVICE
	function DEVICE_INFO(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE_INFO is
		variable Result					: T_DEVICE_INFO;
	begin
		Result.Vendor						:= VENDOR(DeviceString);
		Result.Device						:= DEVICE(DeviceString);
		Result.DevFamily				:= DEVICE_FAMILY(DeviceString);
		Result.DevSubType				:= DEVICE_SUBTYPE(DeviceString);
		Result.DevSeries				:= DEVICE_SERIES(DeviceString);
		Result.DevGeneration		:= DEVICE_GENERATION(DeviceString);
		Result.DevNumber				:= DEVICE_NUMBER(DeviceString);
		Result.TransceiverType	:= TRANSCEIVER_TYPE(DeviceString);
		Result.LUT_FanIn				:= LUT_FANIN(DeviceString);

		return Result;
	end function;


	-- Convert T_DEVICE to string representation as required by "altera_mf" library
	function getAlteraDeviceName (device : T_DEVICE) return string is
	begin
		case device is
			when DEVICE_ARRIA1		=> return "Arria";
			when DEVICE_ARRIA2		=> return "Arria II";
			when DEVICE_ARRIA5		=> return "Arria V";
			when DEVICE_ARRIA10		=> return "Arria 10";
			when DEVICE_CYCLONE1	=> return "Cyclone";
			when DEVICE_CYCLONE2	=> return "Cyclone II";
			when DEVICE_CYCLONE3	=> return "Cyclone III";
			when DEVICE_CYCLONE4	=> return "Cyclone IV";
			when DEVICE_CYCLONE5	=> return "Cyclone V";
			when DEVICE_STRATIX1	=> return "Stratix";
			when DEVICE_STRATIX2	=> return "Stratix II";
			when DEVICE_STRATIX3	=> return "Stratix III";
			when DEVICE_STRATIX4	=> return "Stratix IV";
			when DEVICE_STRATIX5	=> return "Stratix V";
			when DEVICE_STRATIX10 => return "Stratix 10";
			when others =>
				report "Unknown Altera device." severity failure;
				return "";
		end case;
	end function;

	-- force FSM to predefined encoding in debug mode
	function getFSMEncoding_gray(debug : boolean) return string is
	begin
		if debug then
			return "gray";
		else
			case VENDOR is
				when VENDOR_ALTERA =>		return "default";
				--when VENDOR_LATTICE =>	return "default";
				when VENDOR_XILINX =>		return "auto";
				when others =>					report "Unknown vendor." severity failure;
																return "";
			end case;
		end if;
	end function;
end package body;
