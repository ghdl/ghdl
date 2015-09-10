-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
--
-- ============================================================================
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Package:					Global configuration settings.
--
-- Description:
-- ------------------------------------
--		This file evaluates the settings declared in the project specific package my_config.
--		See also template file my_config.vhdl.template.
--
-- License:
-- ============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
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
use			PoC.my_config.all;
use			PoC.my_project.all;
use			PoC.utils.all;


package config is
	constant PROJECT_DIR			: string	:= MY_PROJECT_DIR;
	constant OPERATING_SYSTEM	: string	:= MY_OPERATING_SYSTEM;

	-- TODO: 
	-- ===========================================================================
	subtype T_BOARD_STRING						is STRING(1 to 8);
	subtype T_BOARD_CONFIG_STRING8		is STRING(1 to 8);
	subtype T_BOARD_CONFIG_STRING16		is STRING(1 to 16);
	subtype T_BOARD_CONFIG_STRING32		is STRING(1 to 32);
--	subtype T_BOARD_CONFIG_STRING64		is STRING(1 to 64);
	subtype T_DEVICE_STRING						is STRING(1 to 32);
	
	constant C_BOARD_STRING_EMPTY						: T_BOARD_STRING;
	constant C_BOARD_CONFIG_STRING8_EMPTY		: T_BOARD_CONFIG_STRING8;
	constant C_BOARD_CONFIG_STRING16_EMPTY	: T_BOARD_CONFIG_STRING16;
	constant C_BOARD_CONFIG_STRING32_EMPTY	: T_BOARD_CONFIG_STRING32;
--	constant C_BOARD_CONFIG_STRING64_EMPTY	: T_BOARD_CONFIG_STRING64;
	constant C_DEVICE_STRING_EMPTY					: T_DEVICE_STRING;
	
	-- List of known boards
	-- ---------------------------------------------------------------------------
	type T_BOARD is (
		BOARD_CUSTOM,
		-- Spartan-3 boards
		BOARD_S3SK200,	BOARD_S3SK1000,
		BOARD_S3ESK500,	BOARD_S3ESK1600,
		-- Spartan-6 boards
		BOARD_ATLYS,
		-- Kintex-7 boards
		BOARD_KC705,
		-- Virtex-5 boards
		BOARD_ML505,
		-- Virtex-6 boards
		BOARD_ML605,
		-- Virtex-7 boards
		BOARD_VC707,
		-- Zynq-7000 boards
		BOARD_ZEDBOARD,
		-- Cyclon III boards
		BOARD_DE0,
		-- Stratix II boards
		BOARD_S2GXAV,
		-- Stratix IV boards
		BOARD_DE4,
		-- Stratix V boards
		BOARD_DE5
	);
	
	-- List of known FPGA / Chip vendors
	-- ---------------------------------------------------------------------------
	type T_VENDOR is (
		VENDOR_ALTERA,
		VENDOR_LATTICE,
		VENDOR_XILINX
	);
	subtype vendor_t	is T_VENDOR;

	-- List of known synthesis tool chains
	-- ---------------------------------------------------------------------------
	type T_SYNTHESIS_TOOL is (
		SYNTHESIS_TOOL_ALTERA_QUARTUS2,
		SYNTHESIS_TOOL_SYNOPSIS,
		SYNTHESIS_TOOL_XILINX_XST,
		SYNTHESIS_TOOL_XILINX_VIVADO
	);
	
	-- List of known devices
	-- ---------------------------------------------------------------------------
	type T_DEVICE is (
		DEVICE_SPARTAN3, DEVICE_SPARTAN6,																		-- Xilinx.Spartan
		DEVICE_ZYNQ7,																												-- Xilinx.Zynq
		DEVICE_ARTIX7,																											-- Xilinx.Artix
		DEVICE_KINTEX7,																											-- Xilinx.Kintex
		DEVICE_VIRTEX5,	DEVICE_VIRTEX6, DEVICE_VIRTEX7,											-- Xilinx.Virtex

		DEVICE_CYCLONE1, DEVICE_CYCLONE2, DEVICE_CYCLONE3,									-- Altera.Cyclone
		DEVICE_STRATIX1, DEVICE_STRATIX2, DEVICE_STRATIX4, DEVICE_STRATIX5	-- Altera.Stratix
	);
	subtype device_t	is T_DEVICE;

	-- List of known device families
	-- ---------------------------------------------------------------------------
	type T_DEVICE_FAMILY is (
		-- Xilinx
		DEVICE_FAMILY_SPARTAN,
		DEVICE_FAMILY_ZYNQ,
		DEVICE_FAMILY_ARTIX,
		DEVICE_FAMILY_KINTEX,
		DEVICE_FAMILY_VIRTEX,

		DEVICE_FAMILY_CYCLONE,
		DEVICE_FAMILY_STRATIX
	);

	-- List of known device subtypes
	-- ---------------------------------------------------------------------------
	type T_DEVICE_SUBTYPE is (
		DEVICE_SUBTYPE_NONE,
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
		DEVICE_SUBTYPE_HXT,
		-- Altera
		DEVICE_SUBTYPE_E,
		DEVICE_SUBTYPE_GS,
		DEVICE_SUBTYPE_GX,
		DEVICE_SUBTYPE_GT
	);

	-- List of known transceiver (sub-)types
	-- ---------------------------------------------------------------------------
	type T_TRANSCEIVER is (
		TRANSCEIVER_GTP_DUAL,	TRANSCEIVER_GTPE1, TRANSCEIVER_GTPE2,					-- Xilinx GTP transceivers
		TRANSCEIVER_GTX,			TRANSCEIVER_GTXE1, TRANSCEIVER_GTXE2,					-- Xilinx GTX transceivers
		TRANSCEIVER_GTH,			TRANSCEIVER_GTHE1, TRANSCEIVER_GTHE2,					-- Xilinx GTH transceivers
		TRANSCEIVER_GTZ,																										-- Xilinx GTZ transceivers

		-- TODO: add Altera transceivers
		TRANSCEIVER_GXB,																										-- Altera GXB transceiver

		TRANSCEIVER_NONE
	);

	-- Properties of an FPGA architecture
	-- ===========================================================================
	type T_DEVICE_INFO is record
		Vendor						: T_VENDOR;
		Device						: T_DEVICE;
		DevFamily					: T_DEVICE_FAMILY;
		DevNumber					: natural;
		DevSubType				: T_DEVICE_SUBTYPE;
		DevSeries					: natural;
		
		TransceiverType		: T_TRANSCEIVER;
		LUT_FanIn					: positive;
	end record;
	
	-- Data structures to describe UART / RS232
	type T_BOARD_UART_DESC is record
		IsDTE											: BOOLEAN;									-- Data terminal Equipment (e.g. PC, Printer)
		FlowControl								: T_BOARD_CONFIG_STRING16;	-- (NONE, SW, HW_CTS_RTS, HW_RTR_RTS)
		BaudRate									: T_BOARD_CONFIG_STRING16;	-- e.g. "115.2 kBd"
		BaudRate_Max							: T_BOARD_CONFIG_STRING16;
	end record;
	
	-- Data structures to describe Ethernet
	type T_BOARD_ETHERNET_DESC is record
		IPStyle										: T_BOARD_CONFIG_STRING8;
		RS_DataInterface					: T_BOARD_CONFIG_STRING8;
		PHY_Device								: T_BOARD_CONFIG_STRING16;
		PHY_DeviceAddress					: STD_LOGIC_VECTOR(7 downto 0);
		PHY_DataInterface					: T_BOARD_CONFIG_STRING8;
		PHY_ManagementInterface		: T_BOARD_CONFIG_STRING16;
	end record;

	subtype T_BOARD_ETHERNET_DESC_INDEX		is NATURAL range 0 to 7;
	type		T_BOARD_ETHERNET_DESC_VECTOR	is array(NATURAL range <>) of T_BOARD_ETHERNET_DESC;

	-- Data structures to describe a board layout
	type T_BOARD_INFO is record
		FPGADevice		: T_DEVICE_STRING;
		UART					: T_BOARD_UART_DESC;
		Ethernet			: T_BOARD_ETHERNET_DESC_VECTOR(T_BOARD_ETHERNET_DESC_INDEX);
		EthernetCount	: T_BOARD_ETHERNET_DESC_INDEX;
	end record;

	type T_BOARD_INFO_VECTOR	is array (T_BOARD) of T_BOARD_INFO;

	-- QUESTION: replace archprops with DEVICE_INFO ?
	type archprops_t is record
		LUT_K						: positive;	-- LUT Fanin
	end record;

	-- Functions extracting board and PCB properties from "MY_BOARD"
	-- which is declared in package "my_config".
	-- ===========================================================================
	function BOARD(BoardConfig : string := C_BOARD_STRING_EMPTY)								return T_BOARD;
	function BOARD_INFO(BoardConfig : STRING := C_BOARD_STRING_EMPTY)						return T_BOARD_INFO;
	function BOARD_DEVICE(BoardConfig : STRING := C_BOARD_STRING_EMPTY) 				return STRING;
	function BOARD_UART_BAUDRATE(BoardConfig : STRING := C_BOARD_STRING_EMPTY)	return STRING;
	
	-- Functions extracting device and architecture properties from "MY_DEVICE"
	-- which is declared in package "my_config".
	-- ===========================================================================
	function VENDOR(DeviceString : string := C_DEVICE_STRING_EMPTY)						return T_VENDOR;
	function SYNTHESIS_TOOL(DeviceString : string := C_DEVICE_STRING_EMPTY)		return T_SYNTHESIS_TOOL;
	function DEVICE(DeviceString : string := C_DEVICE_STRING_EMPTY)						return T_DEVICE;
	function DEVICE_FAMILY(DeviceString : string := C_DEVICE_STRING_EMPTY)		return T_DEVICE_FAMILY;
	function DEVICE_NUMBER(DeviceString : string := C_DEVICE_STRING_EMPTY)		return natural;
	function DEVICE_SUBTYPE(DeviceString : string := C_DEVICE_STRING_EMPTY)		return T_DEVICE_SUBTYPE;
	function DEVICE_SERIES(DeviceString : string := C_DEVICE_STRING_EMPTY)		return natural;

	function TRANSCEIVER_TYPE(DeviceString : string := C_DEVICE_STRING_EMPTY)	return T_TRANSCEIVER;
	function LUT_FANIN(DeviceString : string := C_DEVICE_STRING_EMPTY)				return positive;

	function DEVICE_INFO(DeviceString : string := C_DEVICE_STRING_EMPTY)			return T_DEVICE_INFO;

	function ARCH_PROPS return archprops_t;

	-- force FSM to predefined encoding in debug mode
	function getFSMEncoding_gray(debug : BOOLEAN) return STRING;
end package;


package body config is
	-- private functions required by board description
	-- ModelSim requires that this functions is defined before it is used below.
	-- ===========================================================================
	function ite(cond : BOOLEAN; value1 : CHARACTER; value2 : CHARACTER) return CHARACTER is
	begin
		if cond then		return value1;	else	return value2;	end if;
	end function;
	
	-- default fill and string termination character for fixed size strings
	-- ===========================================================================	
	constant C_POC_NUL			: CHARACTER		:= '`';

	-- deferred constant
	-- ===========================================================================	
	constant C_BOARD_STRING_EMPTY						: T_BOARD_STRING					:= (others => C_POC_NUL);
	constant C_BOARD_CONFIG_STRING8_EMPTY		: T_BOARD_CONFIG_STRING8	:= (others => C_POC_NUL);
	constant C_BOARD_CONFIG_STRING16_EMPTY	: T_BOARD_CONFIG_STRING16	:= (others => C_POC_NUL);
	constant C_BOARD_CONFIG_STRING32_EMPTY	: T_BOARD_CONFIG_STRING32	:= (others => C_POC_NUL);
	constant C_DEVICE_STRING_EMPTY					: T_DEVICE_STRING					:= (others => C_POC_NUL);
	

	-- chr_is* function
	function chr_isDigit(chr : CHARACTER) return boolean is
	begin
		return ((CHARACTER'pos('0') <= CHARACTER'pos(chr)) and (CHARACTER'pos(chr) <= CHARACTER'pos('9')));
	end function;

	function chr_isAlpha(chr : character) return boolean is
	begin
		return (((CHARACTER'pos('a') <= CHARACTER'pos(chr)) and (CHARACTER'pos(chr) <= CHARACTER'pos('z'))) or
						((CHARACTER'pos('A') <= CHARACTER'pos(chr)) and (CHARACTER'pos(chr) <= CHARACTER'pos('Z'))));
	end function;

	function str_length(str : STRING) return NATURAL is
	begin
		for i in str'range loop
			if (str(i) = C_POC_NUL) then
				return i - str'low;
			end if;
		end loop;
		return str'length;
	end function;

	function str_trim(str : STRING) return STRING is
	begin
		return str(str'low to str'low + str_length(str) - 1);
	end function;

	function str_imatch(str1 : STRING; str2 : STRING) return BOOLEAN is
		constant len	: NATURAL 		:= imin(str1'length, str2'length);
		variable chr1	: CHARACTER;
		variable chr2	: CHARACTER;
	begin
		-- if both strings are empty
		if ((str1'length = 0 ) and (str2'length = 0)) then		return TRUE;	end if;
		-- compare char by char
		for i in str1'low to str1'low + len - 1 loop
			chr1	:= str1(i);
			chr2	:= str2(str2'low + (i - str1'low ));
			if (CHARACTER'pos('A') <= CHARACTER'pos(chr1)) and (CHARACTER'pos(chr1) <= CHARACTER'pos('Z')) then
				chr1	:= CHARACTER'val(CHARACTER'pos(chr1) - CHARACTER'pos('A') + CHARACTER'pos('a'));
			end if;
			if (CHARACTER'pos('A') <= CHARACTER'pos(chr2)) and (CHARACTER'pos(chr2) <= CHARACTER'pos('Z')) then
				chr2	:= CHARACTER'val(CHARACTER'pos(chr2) - CHARACTER'pos('A') + CHARACTER'pos('a'));
			end if;
			if (chr1 /= chr2) then
				return FALSE;
			elsif ((chr1 = C_POC_NUL) xor (chr2 = C_POC_NUL)) then
				return FALSE;
			elsif ((chr1 = C_POC_NUL) and (chr2 = C_POC_NUL)) then
				return TRUE;
			end if;
		end loop;
		-- check special cases, 
		return (((str1'length = len) and (str2'length = len)) or									-- both strings are fully consumed and equal
						((str1'length > len) and (str1(str1'low + len) = C_POC_NUL)) or		-- str1 is longer, but str_length equals len
						((str2'length > len) and (str2(str2'low + len) = C_POC_NUL)));		-- str2 is longer, but str_length equals len
	end function;

	function str_find(str : STRING; pattern : STRING; start : NATURAL := 0) return BOOLEAN is
	begin
		for i in imax(str'low, start) to (str'high - pattern'length + 1) loop
			exit when (str(i) = C_POC_NUL);
			if (str(i to i + pattern'length - 1) = pattern) then
				return TRUE;
			end if;
		end loop;
		return FALSE;
	end function;


	-- helper function to create configuration strings
	-- ===========================================================================	
	function getLocalDeviceString(DeviceString : STRING) return STRING is
		function ite(cond : BOOLEAN; value1 : STRING; value2 : STRING) return STRING is begin
			if cond then	return value1;	else	return value2;	end if;
		end function;
		
		constant ConstNUL				: STRING(1 to 1)				:= (others => C_POC_NUL);
		constant MY_DEVICE_STR	: STRING								:= BOARD_DEVICE;		
		variable Result					: STRING(1 to T_DEVICE_STRING'length);
	begin
		Result := (others => C_POC_NUL);
		-- report DeviceString for debugging
		if (POC_VERBOSE = TRUE) then
			report "getLocalDeviceString: DeviceString='" & str_trim(DeviceString) & "' MY_DEVICE='" & str_trim(MY_DEVICE) & "' MY_DEVICE_STR='" & str_trim(MY_DEVICE_STR) & "'"  severity NOTE;
		end if;
		-- if DeviceString is populated
		if ((str_length(DeviceString) /= 0) and (str_imatch(DeviceString, "None") = FALSE)) then
			Result(1 to imin(T_DEVICE_STRING'length, imax(1, DeviceString'length)))		:= ite((DeviceString'length > 0), DeviceString(1 to imin(T_DEVICE_STRING'length, DeviceString'length)), ConstNUL);
		-- if MY_DEVICE is set, prefer it
		elsif ((str_length(MY_DEVICE) /= 0) and (str_imatch(MY_DEVICE, "None") = FALSE)) then
			Result(1 to imin(T_DEVICE_STRING'length, imax(1, MY_DEVICE'length)))			:= ite((MY_DEVICE'length > 0), MY_DEVICE(1 to imin(T_DEVICE_STRING'length, MY_DEVICE'length)), ConstNUL);
		-- otherwise use MY_BOARD
		else
			Result(1 to imin(T_DEVICE_STRING'length, imax(1, MY_DEVICE_STR'length)))	:= ite((MY_DEVICE_STR'length > 0), MY_DEVICE_STR(1 to imin(T_DEVICE_STRING'length, MY_DEVICE_STR'length)), ConstNUL);
		end if;
		return Result;
	end function;

	-- helper function to create configuration strings
	-- ===========================================================================
	function conf(str : string; Size : POSITIVE) return STRING is
		constant ConstNUL		: STRING(1 to 1)				:= (others => C_POC_NUL);
		variable Result			: STRING(1 to Size);
		-- inlined function from PoC.utils, to break dependency
		function ite(cond : BOOLEAN; value1 : STRING; value2 : STRING) return STRING is begin
			if cond then	return value1;	else	return value2;	end if;
		end function;
		function imin(arg1 : integer; arg2 : integer) return integer is begin
			if arg1 < arg2 then return arg1;	else	return arg2;	end if;
		end function;
		function imax(arg1 : integer; arg2 : integer) return integer is begin
			if arg1 > arg2 then return arg1;	else	return arg2;	end if;
		end function;
	begin
		Result := (others => C_POC_NUL);
		if (str'length > 0) then
			Result(1 to imin(Size, imax(1, str'length))) := ite((str'length > 0), str(1 to imin(Size, str'length)), ConstNUL);
		end if;
		return Result;
	end function;
	
	function conf8(str : string) return T_BOARD_CONFIG_STRING8 is
	begin
		return conf(str, 8);
	end function;

	function conf16(str : string) return T_BOARD_CONFIG_STRING16 is
	begin
		return conf(str, 16);
	end function;
	
	function conf32(str : string) return T_BOARD_CONFIG_STRING32 is
	begin
		return conf(str, 32);
	end function;
	
--	function conf64(str : string) return T_BOARD_CONFIG_STRING64 is
--	begin
--		return conf(str, 64);
--	end function;
	
	function extractFirstNumber(str : STRING) return NATURAL is
		variable low			: integer;
		variable high			: integer;
		variable Result		: NATURAL;
		variable Digit		: INTEGER;
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
		if (low = -1) then		return 0; end if;
		
		for i in (low + 1) to str'high loop
			if chr_isAlpha(str(i)) then
				high := i - 1;
				exit;
			end if;
		end loop;
		
		if (high = -1) then		return 0; end if;
		-- return INTEGER'value(str(low to high));			-- 'value(...) is not supported by Vivado Synth 2014.1
		
		-- convert substring to a number
		for i in low to high loop
			if (chr_isDigit(str(i)) = FALSE) then
				return -1;
			end if;
			Result	:= (Result * 10) + (character'pos(str(i)) - character'pos('0'));
		end loop;
		return Result;
	end function;

	-- predefined UART descriptions
	function brd_CreateUART(IsDTE : BOOLEAN; FlowControl : STRING; BaudRate : STRING; BaudRate_Max : STRING := "") return T_BOARD_UART_DESC is
		variable Result			: T_BOARD_UART_DESC;
	begin
		Result.IsDTE				:= IsDTE;
		Result.FlowControl	:= conf16(FlowControl);
		Result.BaudRate			:= conf16(BaudRate);
		Result.BaudRate_Max	:= conf16(BaudRate_Max);
		return Result;
	end function;

	constant C_BOARD_UART_EMPTY							: T_BOARD_UART_DESC	:= brd_CreateUART(TRUE,		"NONE",				"0 Bd");
	constant C_BOARD_UART_DTE_115200_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(TRUE,		"NONE",				"115.2 kBd");
	constant C_BOARD_UART_DCE_115200_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"NONE",				"115.2 kBd");
	constant C_BOARD_UART_DCE_115200_HWCTS	: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"HW_CTS_RTS",	"115.2 kBd");
	constant C_BOARD_UART_DTE_460800_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"NONE",				"460.8 kBd");
	constant C_BOARD_UART_DTE_921600_NONE		: T_BOARD_UART_DESC	:= brd_CreateUART(FALSE,	"NONE",				"921.6 kBd");

	function brd_CreateEthernet(IPStyle : STRING; RS_DataInt : STRING; PHY_Device : STRING; PHY_DevAddress : STD_LOGIC_VECTOR(7 downto 0); PHY_DataInt : STRING; PHY_MgntInt : STRING) return T_BOARD_ETHERNET_DESC is
		variable Result		: T_BOARD_ETHERNET_DESC;
	begin
		Result.IPStyle									:= conf8(IPStyle);
		Result.RS_DataInterface					:= conf8(RS_DataInt);
		Result.PHY_Device								:= conf16(PHY_Device);
		Result.PHY_DeviceAddress				:= PHY_DevAddress;
		Result.PHY_DataInterface				:= conf8(PHY_DataInt);
		Result.PHY_ManagementInterface	:= conf16(PHY_MgntInt);
		return Result;
	end function;

	constant C_BOARD_ETH_EMPTY							: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("", "", "", x"00", "", "");
	constant C_BOARD_ETH_SOFT_GMII_88E1111	: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"07", "GMII", "MDIO");
	constant C_BOARD_ETH_HARD_GMII_88E1111	: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("HARD", "GMII", "MARVEL_88E1111", x"07", "GMII", "MDIO");
	constant C_BOARD_ETH_SOFT_SGMII_88E1111	: T_BOARD_ETHERNET_DESC		:= brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"07", "SGMII", "MDIO_OVER_IIC");
	
	constant C_BOARD_ETH_NONE		: T_BOARD_ETHERNET_DESC_VECTOR(T_BOARD_ETHERNET_DESC_INDEX)	:= (others => C_BOARD_ETH_EMPTY);


	-- board description
	-- ===========================================================================
	CONSTANT C_BOARD_INFO_LIST	: T_BOARD_INFO_VECTOR	:= (
		-- Xilinx boards
		-- =========================================================================
		BOARD_S3SK200 => (
			FPGADevice =>			conf32("XC3S200FT256"),								-- XC2S200FT256
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		BOARD_S3SK1000 => (
			FPGADevice =>			conf32("XC3S1000FT256"),							-- XC2S200FT256
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		BOARD_S3ESK500 => (
			FPGADevice =>			conf32("XC3S500EFT256"),							-- XC2S200FT256
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		BOARD_S3ESK1600 => (
			FPGADevice =>			conf32("XC3S1600EFT256"),							-- XC2S200FT256
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		BOARD_ATLYS => (
			FPGADevice =>			conf32("XC6SLX45-3CSG324"),						-- XC6SLX45-3CSG324
			UART =>						C_BOARD_UART_DTE_460800_NONE,
			Ethernet =>	(
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),
		BOARD_KC705 => (
			FPGADevice =>			conf32("XC7K325T-2FFG900C"),					-- XC7K325T-2FFG900C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_SOFT_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),
		BOARD_ML505 => (
			FPGADevice =>			conf32("XC5VLX50T-1FF1136"),					-- XC5VLX50T-1FF1136
			UART =>						C_BOARD_UART_DCE_115200_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount => 1
		),
		BOARD_ML605 => (
			FPGADevice =>			conf32("XC6VLX240T-1FF1156"),					-- XC6VLX240T-1FF1156
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet => (
				0 =>			C_BOARD_ETH_HARD_GMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount => 1
		),
		BOARD_VC707 => (
			FPGADevice =>			conf32("XC7VX485T-2FFG1761C"),				-- XC7VX485T-2FFG1761C
			UART =>						C_BOARD_UART_DTE_921600_NONE,
			Ethernet => (
				0 =>			C_BOARD_ETH_SOFT_SGMII_88E1111,
				others =>	C_BOARD_ETH_EMPTY),
			EthernetCount =>	1
		),
		BOARD_ZEDBOARD => (
			FPGADevice =>			conf32("XC7Z020-1CLG484"),						-- XC7Z020-1CLG484
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		-- Altera boards
		-- =========================================================================
		BOARD_DE0 => (
			FPGADevice =>			conf32("EP3C16F484"),									-- EP3C16F484
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		BOARD_S2GXAV => (
			FPGADevice =>			conf32("EP2SGX90FF1508C3"),						-- EP2SGX90FF1508C3
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		BOARD_DE4 => (
			FPGADevice =>			conf32("EP4SGX230KF40C2"),						-- EP4SGX230KF40C2
			UART =>						C_BOARD_UART_DTE_460800_NONE,
			Ethernet => (
				0 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"00", "RGMII", "MDIO"),
				1 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"01", "RGMII", "MDIO"),
				2 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"02", "RGMII", "MDIO"),
				3 => brd_CreateEthernet("SOFT", "GMII", "MARVEL_88E1111", x"03", "RGMII", "MDIO"),
				others => C_BOARD_ETH_EMPTY
			),
			EthernetCount => 4
		),
		BOARD_DE5 => (
			FPGADevice =>			conf32("EP5SGXEA7N2F45C2"),						-- EP5SGXEA7N2F45C2
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		),
		
		-- custom board / dummy entry
		BOARD_CUSTOM => (
			FPGADevice =>			conf32("Device is unknown for a custom board"),
			UART =>						C_BOARD_UART_EMPTY,
			Ethernet =>				C_BOARD_ETH_NONE,
			EthernetCount =>	0
		)
	);
	
	-- Public functions
	-- ===========================================================================
	-- TODO: comment
	function BOARD(BoardConfig : string := C_BOARD_STRING_EMPTY) return T_BOARD is
		-- inlined function from PoC.utils, to break dependency
		function ite(cond : BOOLEAN; value1 : STRING; value2 : STRING) return STRING is begin
			if cond then	return value1;	else	return value2;	end if;
		end function;
	
		constant MY_BRD			: T_BOARD_STRING	:= ite((BoardConfig /= C_BOARD_STRING_EMPTY), conf(BoardConfig, T_BOARD_STRING'length), conf(MY_BOARD, T_BOARD_STRING'length));
	begin
		if (POC_VERBOSE = TRUE) then
			report "PoC configuration: Used board is '" & str_trim(MY_BRD) & "'" severity NOTE;
		end if;
		for i in T_BOARD loop
			if str_imatch(T_BOARD'image(i), "BOARD_" & str_trim(MY_BRD)) then
				return  i;
			end if;
		end loop;

		report "Unknown board name in MY_BOARD = " & MY_BRD & "." severity failure;
		return BOARD_CUSTOM;
	end function;
	
	function BOARD_INFO(BoardConfig : STRING := C_BOARD_STRING_EMPTY) return T_BOARD_INFO is
		constant BRD	: T_BOARD := BOARD(BoardConfig);
  begin
		return  C_BOARD_INFO_LIST(BRD);
	end function;

	-- TODO: comment
	function BOARD_DEVICE(BoardConfig : STRING := C_BOARD_STRING_EMPTY) return STRING is
		constant BRD	: T_BOARD := BOARD(BoardConfig);
  begin
		return str_trim(C_BOARD_INFO_LIST(BRD).FPGADevice);
	end function;
	
	function BOARD_UART_BAUDRATE(BoardConfig : STRING := C_BOARD_STRING_EMPTY) return STRING is
		constant BRD	: T_BOARD := BOARD(BoardConfig);
  begin
		return str_trim(C_BOARD_INFO_LIST(BRD).UART.BaudRate);
	end function;
	
	-- purpose: extract vendor from MY_DEVICE
	function VENDOR(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_VENDOR is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN_STR	: string(1 to 2)  := MY_DEV(1 to 2);
	begin
		case VEN_STR is
			when "XC" =>		return VENDOR_XILINX;
			when "EP" =>		return VENDOR_ALTERA;
			when others =>	report "Unknown vendor in MY_DEVICE = '" & MY_DEV & "'" severity failure;
										 -- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;
	
	function SYNTHESIS_TOOL(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_SYNTHESIS_TOOL is
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
	begin
		case VEN is
			when VENDOR_ALTERA =>
				return SYNTHESIS_TOOL_ALTERA_QUARTUS2;
			when VENDOR_LATTICE =>
				return SYNTHESIS_TOOL_SYNOPSIS;
			when VENDOR_XILINX =>
				if (1 fs /= 1 us) then
					return SYNTHESIS_TOOL_XILINX_XST;
				else
					return SYNTHESIS_TOOL_XILINX_VIVADO;
				end if;
		end case;
	end function;

	-- purpose: extract device from MY_DEVICE
	function DEVICE(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
		constant DEV_STR	: string(3 to  4)	:= MY_DEV(3 to 4);
	begin
		case VEN is
			when VENDOR_ALTERA =>
				case DEV_STR is
					when "1C"	 => return DEVICE_CYCLONE1;
					when "2C"	 => return DEVICE_CYCLONE2;
					when "3C"	 => return DEVICE_CYCLONE3;
					when "1S"	 => return DEVICE_STRATIX1;
					when "2S"	 => return DEVICE_STRATIX2;
					when "4S"	 => return DEVICE_STRATIX4;
					when "5S"	 => return DEVICE_STRATIX5;
					when others => report "Unknown Altera device in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when VENDOR_XILINX =>
				case DEV_STR is
					when "7A"	 => return DEVICE_ARTIX7;
					when "7K"	 => return DEVICE_KINTEX7;
					when "3S"	 => return DEVICE_SPARTAN3;
					when "6S"	 => return DEVICE_SPARTAN6;
					when "5V"	 => return DEVICE_VIRTEX5;
					when "6V"	 => return DEVICE_VIRTEX6;
					when "7V"	 => return DEVICE_VIRTEX7;
					when "7Z"	 => return DEVICE_ZYNQ7;
					when others => report "Unknown Xilinx device in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;
				
			when others => report "Unknown vendor in MY_DEVICE = " & MY_DEV & "." severity failure;
										 -- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;

	-- purpose: extract device from MY_DEVICE
	function DEVICE_FAMILY(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE_FAMILY is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
		constant FAM_CHAR	: character				:= MY_DEV(4);
	begin
		case VEN is
			when VENDOR_ALTERA =>
				case FAM_CHAR is
					when 'C' =>		return DEVICE_FAMILY_CYCLONE;
					when 'S' =>		return DEVICE_FAMILY_STRATIX;
					when others =>	report "Unknown Altera device family in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;

			when VENDOR_XILINX =>
				case FAM_CHAR is
					when 'A' =>		return DEVICE_FAMILY_ARTIX;
					when 'K' =>		return DEVICE_FAMILY_KINTEX;
					when 'S' =>		return DEVICE_FAMILY_SPARTAN;
					when 'V' =>		return DEVICE_FAMILY_VIRTEX;
					when 'Z' =>		return DEVICE_FAMILY_ZYNQ;
					when others => report "Unknown Xilinx device family in MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end case;
				
			when others => report "Unknown vendor in MY_DEVICE = '" & MY_DEV & "'" severity failure;
										 -- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;

	function DEVICE_SERIES(DeviceString : string := C_DEVICE_STRING_EMPTY) return natural is
		constant MY_DEV	: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant DEV		: T_DEVICE				:= DEVICE(DeviceString);
	begin
		case DEV is
			when DEVICE_ARTIX7 | DEVICE_KINTEX7 | DEVICE_VIRTEX7 | DEVICE_ZYNQ7 =>	return 7;		-- all Xilinx ****7 devices share some common features: e.g. XADC
			when others =>																													return 0;
		end case;
	end function;

	function DEVICE_NUMBER(DeviceString : string := C_DEVICE_STRING_EMPTY) return natural is
		constant MY_DEV		: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant VEN			: T_VENDOR				:= VENDOR(DeviceString);
	begin
		case VEN is
			when VENDOR_ALTERA =>		return extractFirstNumber(MY_DEV(5 to MY_DEV'high));
			when VENDOR_XILINX =>		return extractFirstNumber(MY_DEV(5 to MY_DEV'high));
			when others =>					report "Unknown vendor in MY_DEVICE = '" & MY_DEV & "'" severity failure;
															-- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;
	
	function DEVICE_SUBTYPE(DeviceString : string := C_DEVICE_STRING_EMPTY) return t_device_subtype is
		constant MY_DEV				: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant DEV					: T_DEVICE				:= DEVICE(MY_DEV);
		constant DEV_SUB_STR	: string(1 to 2)	:= MY_DEV(5 to 6);																-- work around for GHDL
	begin
		case DEV is
			when DEVICE_CYCLONE1 | DEVICE_CYCLONE2 | DEVICE_CYCLONE3 =>				return DEVICE_SUBTYPE_NONE;		-- Altera Cyclon I, II, III devices have no subtype

			when DEVICE_STRATIX2 =>
				if		chr_isDigit(DEV_SUB_STR(1)) then																						return DEVICE_SUBTYPE_NONE;
				elsif	(DEV_SUB_STR = "GX") then																										return DEVICE_SUBTYPE_GX;
				else	report "Unknown Stratix II subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_STRATIX4 =>
				if		(DEV_SUB_STR(1) = 'E') then																									return DEVICE_SUBTYPE_E;
				elsif	(DEV_SUB_STR = "GX") then																										return DEVICE_SUBTYPE_GX;
--				elsif	(DEV_SUB_STR = "GT") then																										return DEVICE_SUBTYPE_GT;
				else	report "Unknown Stratix II subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_SPARTAN3 => report "TODO: parse Spartan3 / Spartan3E / Spartan3AN device subtype." severity failure;

			when DEVICE_SPARTAN6 =>
				if		((DEV_SUB_STR = "LX") and (not	str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LX;
				elsif	((DEV_SUB_STR = "LX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LXT;
				else	report "Unknown Virtex-5 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;
			
			when DEVICE_VIRTEX5 =>
				if		((DEV_SUB_STR = "LX") and (not	str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LX;
				elsif	((DEV_SUB_STR = "LX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LXT;
				elsif	((DEV_SUB_STR = "SX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_SXT;
				elsif	((DEV_SUB_STR = "TX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_TXT;
				elsif	((DEV_SUB_STR = "FX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_FXT;
				else	report "Unknown Virtex-5 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_VIRTEX6 =>
				if		((DEV_SUB_STR = "LX") and (not	str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LX;
				elsif	((DEV_SUB_STR = "LX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_LXT;
				elsif	((DEV_SUB_STR = "SX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_SXT;
				elsif	((DEV_SUB_STR = "CX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_CXT;
				elsif	((DEV_SUB_STR = "HX") and (			str_find(MY_DEV(7 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_HXT;
				else	report "Unknown Virtex-6 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when DEVICE_ARTIX7 =>
				if		(													(			str_find(MY_DEV(5 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_T;
				else	report "Unknown Artix-7 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;
				
			when DEVICE_KINTEX7 =>
				if		(													(			str_find(MY_DEV(5 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_T;
				else	report "Unknown Kintex-7 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;
				
			when DEVICE_VIRTEX7 =>
				if		(														(		str_find(MY_DEV(5 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_T;
				elsif	((DEV_SUB_STR(1) = 'X') and (		str_find(MY_DEV(6 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_XT;
				elsif	((DEV_SUB_STR(1) = 'H') and (		str_find(MY_DEV(6 TO MY_DEV'high), "T"))) then	return DEVICE_SUBTYPE_HT;
				else	report "Unknown Virtex-7 subtype: MY_DEVICE = '" & MY_DEV & "'" severity failure;
				end if;

			when others => report "Transceiver type is unknown for the given device." severity failure;
									-- return statement is explicitly missing otherwise XST won't stop
		end case;

	end function;

	function LUT_FANIN(DeviceString : string := C_DEVICE_STRING_EMPTY) return positive is
		constant MY_DEV	: string(1 to 32)	:= getLocalDeviceString(DeviceString);
		constant DEV		: T_DEVICE				:= DEVICE(DeviceString);
	begin
		case DEV is
			when DEVICE_CYCLONE1 | DEVICE_CYCLONE2 | DEVICE_CYCLONE3 =>			return 4;
			when DEVICE_STRATIX1 | DEVICE_STRATIX2 =>												return 4;
			when DEVICE_STRATIX4 | DEVICE_STRATIX5 =>												return 6;

			when DEVICE_SPARTAN3 =>																					return 4;
			when DEVICE_SPARTAN6 =>																					return 6;
			when DEVICE_ARTIX7 =>																						return 6;
			when DEVICE_KINTEX7 =>																					return 6;
			when DEVICE_VIRTEX5 | DEVICE_VIRTEX6 | DEVICE_VIRTEX7 => 				return 6;
			when DEVICE_ZYNQ7 =>																						return 6;

			when others => report "LUT fan-in is unknown for the given device." severity failure;
									-- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;

	function TRANSCEIVER_TYPE(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_TRANSCEIVER is
		constant MY_DEV		: string(1 to 32)		:= getLocalDeviceString(DeviceString);
		constant DEV			: T_DEVICE					:= DEVICE(DeviceString);
		constant DEV_NUM	: natural						:= DEVICE_NUMBER(DeviceString);
		constant DEV_SUB	: t_device_subtype	:= DEVICE_SUBTYPE(DeviceString);
	begin
		case DEV is
			when DEVICE_CYCLONE1 | DEVICE_CYCLONE2 | DEVICE_CYCLONE3 =>				return TRANSCEIVER_NONE;		-- Altera Cyclon I, II, III devices have no transceivers

			when DEVICE_SPARTAN3 =>						return TRANSCEIVER_NONE;		-- Xilinx Spartan3 devices have no transceivers

			when DEVICE_SPARTAN6 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_LX =>			return TRANSCEIVER_NONE;
					when DEVICE_SUBTYPE_LXT =>		return TRANSCEIVER_GTPE1;
					when others =>								report "Unknown Spartan-6 subtype: " & t_device_subtype'image(DEV_SUB) severity failure;
				end case;
			
			when DEVICE_VIRTEX5 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_LX =>			return TRANSCEIVER_NONE;
					when DEVICE_SUBTYPE_SXT =>		return TRANSCEIVER_GTP_DUAL;
					when DEVICE_SUBTYPE_LXT =>		return TRANSCEIVER_GTP_DUAL;
					when DEVICE_SUBTYPE_TXT =>		return TRANSCEIVER_GTX;
					when DEVICE_SUBTYPE_FXT =>		return TRANSCEIVER_GTX;
					when others =>								report "Unknown Virtex-5 subtype: " & t_device_subtype'image(DEV_SUB) severity failure;
				end case;

			when DEVICE_VIRTEX6 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_LX =>			return TRANSCEIVER_NONE;
					when DEVICE_SUBTYPE_SXT =>		return TRANSCEIVER_GTXE1;
					when DEVICE_SUBTYPE_LXT =>		return TRANSCEIVER_GTXE1;
					when DEVICE_SUBTYPE_HXT =>		return TRANSCEIVER_GTXE1;
					when others =>								report "Unknown Virtex-6 subtype: " & t_device_subtype'image(DEV_SUB) severity failure;
				end case;
				
			when DEVICE_ARTIX7 =>							return TRANSCEIVER_GTPE2;
			when DEVICE_KINTEX7 =>						return TRANSCEIVER_GTXE2;
			when DEVICE_VIRTEX7 =>
				case DEV_SUB is
					when DEVICE_SUBTYPE_T =>			return TRANSCEIVER_GTXE2;
					when DEVICE_SUBTYPE_XT =>
						if (DEV_NUM = 485) then			return TRANSCEIVER_GTXE2;
						else												return TRANSCEIVER_GTHE2;
						end if;
					when DEVICE_SUBTYPE_HT =>			return TRANSCEIVER_GTHE2;
					when others =>								report "Unknown Virtex-7 subtype: " & t_device_subtype'image(DEV_SUB) severity failure;
				end case;
				
			when DEVICE_STRATIX2 => return TRANSCEIVER_GXB;
			when DEVICE_STRATIX4 => return TRANSCEIVER_GXB;
				
			when others => report "Unknown device." severity failure;
									-- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;

	-- purpose: extract architecture properties from DEVICE
	function DEVICE_INFO(DeviceString : string := C_DEVICE_STRING_EMPTY) return T_DEVICE_INFO is
		variable Result					: T_DEVICE_INFO;
	begin
		Result.Vendor						:= VENDOR(DeviceString);
		Result.Device						:= DEVICE(DeviceString);
		Result.DevFamily				:= DEVICE_FAMILY(DeviceString);
		Result.DevNumber				:= DEVICE_NUMBER(DeviceString);
		Result.DevSubType				:= DEVICE_SUBTYPE(DeviceString);
		Result.DevSeries				:= DEVICE_SERIES(DeviceString);
		Result.TransceiverType	:= TRANSCEIVER_TYPE(DeviceString);
		Result.LUT_FanIn				:= LUT_FANIN(DeviceString);
		
		return Result;
	end function;
	
	function ARCH_PROPS return archprops_t is
		variable result : archprops_t;
	begin
		result.LUT_K					:= LUT_FANIN;

		return	result;
	end function;

	-- force FSM to predefined encoding in debug mode
	function getFSMEncoding_gray(debug : BOOLEAN) return STRING is
	begin
		if (debug = true) then
			return "gray";
		else
			case VENDOR is
				when VENDOR_XILINX =>		return "auto";
				when VENDOR_ALTERA =>		return "default";
				when others =>					report "Unknown vendor." severity failure;
																-- return statement is explicitly missing otherwise XST won't stop
			end case;
		end if;
	end function;
end package body;
