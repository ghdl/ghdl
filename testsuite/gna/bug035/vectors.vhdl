-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Package:					Common functions and types
--
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Description:
-- ------------------------------------
--		For detailed documentation see below.
--
-- License:
-- ============================================================================
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
-- ============================================================================

library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library	PoC;
use			PoC.utils.all;
use			PoC.strings.all;


package vectors is
	-- ==========================================================================
	-- Type declarations
	-- ==========================================================================
	-- STD_LOGIC_VECTORs
	subtype T_SLV_2							is STD_LOGIC_VECTOR(1 downto 0);
	subtype T_SLV_3							is STD_LOGIC_VECTOR(2 downto 0);
	subtype T_SLV_4							is STD_LOGIC_VECTOR(3 downto 0);
	subtype T_SLV_8							is STD_LOGIC_VECTOR(7 downto 0);
	subtype T_SLV_12						is STD_LOGIC_VECTOR(11 downto 0);
	subtype T_SLV_16						is STD_LOGIC_VECTOR(15 downto 0);
	subtype T_SLV_24						is STD_LOGIC_VECTOR(23 downto 0);
	subtype T_SLV_32						is STD_LOGIC_VECTOR(31 downto 0);
	subtype T_SLV_48						is STD_LOGIC_VECTOR(47 downto 0);
	subtype T_SLV_64						is STD_LOGIC_VECTOR(63 downto 0);
	subtype T_SLV_96						is STD_LOGIC_VECTOR(95 downto 0);
	subtype T_SLV_128						is STD_LOGIC_VECTOR(127 downto 0);
	subtype T_SLV_256						is STD_LOGIC_VECTOR(255 downto 0);
	subtype T_SLV_512						is STD_LOGIC_VECTOR(511 downto 0);
	
	-- STD_LOGIC_VECTOR_VECTORs
	--	type		T_SLVV							is array(NATURAL range <>) of STD_LOGIC_VECTOR;					-- VHDL 2008 syntax - not yet supported by Xilinx
	type		T_SLVV_2						is array(NATURAL range <>) of T_SLV_2;
	type		T_SLVV_3						is array(NATURAL range <>) of T_SLV_3;
	type		T_SLVV_4						is array(NATURAL range <>) of T_SLV_4;
	type		T_SLVV_8						is array(NATURAL range <>) of T_SLV_8;
	type		T_SLVV_12						is array(NATURAL range <>) of T_SLV_12;
	type		T_SLVV_16						is array(NATURAL range <>) of T_SLV_16;
	type		T_SLVV_24						is array(NATURAL range <>) of T_SLV_24;
	type		T_SLVV_32						is array(NATURAL range <>) of T_SLV_32;
	type		T_SLVV_48						is array(NATURAL range <>) of T_SLV_48;
	type		T_SLVV_64						is array(NATURAL range <>) of T_SLV_64;
	type		T_SLVV_128					is array(NATURAL range <>) of T_SLV_128;
	type		T_SLVV_256					is array(NATURAL range <>) of T_SLV_256;
	type		T_SLVV_512					is array(NATURAL range <>) of T_SLV_512;

	-- STD_LOGIC_MATRIXs
	type		T_SLM								is array(NATURAL range <>, NATURAL range <>) of STD_LOGIC;
	-- ATTENTION:
	-- 1.	you MUST initialize your matrix signal with 'Z' to get correct simulation results (iSIM, vSIM, ghdl/gtkwave)
	--		Example: signal myMatrix	: T_SLM(3 downto 0, 7 downto 0)			:= (others => (others => 'Z'));
	-- 2.	Xilinx iSIM work-around: DON'T use myMatrix'range(n) for n >= 2
	--		because: myMatrix'range(2) returns always myMatrix'range(1);	tested with ISE/iSIM 14.2
	-- USAGE NOTES:
	--	dimmension 1 => rows			- e.g. Words
	--	dimmension 2 => columns		- e.g. Bits/Bytes in a word


  -- ==========================================================================
  -- Function declarations
  -- ==========================================================================
  -- slicing boundary calulations
  function low (lenvec : T_POSVEC; index : NATURAL) return NATURAL;
  function high(lenvec : T_POSVEC; index : NATURAL) return NATURAL;

	-- Assign procedures: assign_*
	procedure assign_row(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant RowIndex : NATURAL);																	-- assign vector to complete row
	procedure assign_row(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant RowIndex : NATURAL; Position : NATURAL);							-- assign short vector to row starting at position
	procedure assign_row(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant RowIndex : NATURAL; High : NATURAL; Low : NATURAL);		-- assign short vector to row in range high:low
	procedure assign_col(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant ColIndex : NATURAL);																	-- assign vector to complete column
	-- ATTENTION:	see T_SLM definition for further details and work-arounds

	-- Matrix to matrix conversion: slm_slice*
	function slm_slice(slm : T_SLM; RowIndex : NATURAL; ColIndex : NATURAL; Height : NATURAL; Width : NATURAL) return T_SLM;						-- get submatrix in boundingbox RowIndex,ColIndex,Height,Width
	function slm_slice_rows(slm : T_SLM; High : NATURAL; Low : NATURAL) return T_SLM;																										-- get submatrix / all rows in RowIndex range high:low
	function slm_slice_cols(slm : T_SLM; High : NATURAL; Low : NATURAL) return T_SLM;																										-- get submatrix / all columns in ColIndex range high:low

	-- Matrix concatenation: slm_merge_*
	function slm_merge_rows(slm1 : T_SLM; slm2 : T_SLM) return T_SLM;
	function slm_merge_cols(slm1 : T_SLM; slm2 : T_SLM) return T_SLM;
	
	-- Matrix to vector conversion: get_*
	function get_col(slm : T_SLM; ColIndex : NATURAL) return STD_LOGIC_VECTOR;																	-- get a matrix column
	function get_row(slm : T_SLM; RowIndex : NATURAL)	return STD_LOGIC_VECTOR;																	-- get a matrix row
	function get_row(slm : T_SLM; RowIndex : NATURAL; Length : POSITIVE)	return STD_LOGIC_VECTOR;							-- get a matrix row of defined length [length - 1 downto 0]
	function get_row(slm : T_SLM; RowIndex : NATURAL; High : NATURAL; Low : NATURAL) return STD_LOGIC_VECTOR;		-- get a sub vector of a matrix row at high:low

	-- Convert to vector: to_slv
	function to_slv(slvv : T_SLVV_2)							return STD_LOGIC_VECTOR;								-- convert vector-vector to flatten vector
	function to_slv(slvv : T_SLVV_4)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_8)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_12)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_16)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_24)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_32)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_64)							return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slvv : T_SLVV_128)						return STD_LOGIC_VECTOR;								-- ...
	function to_slv(slm : T_SLM)									return STD_LOGIC_VECTOR;								-- convert matrix to flatten vector
	
	-- Convert flat vector to avector-vector: to_slvv_*
	function to_slvv_4(slv : STD_LOGIC_VECTOR)		return T_SLVV_4;												-- 
	function to_slvv_8(slv : STD_LOGIC_VECTOR)		return T_SLVV_8;												-- 
	function to_slvv_12(slv : STD_LOGIC_VECTOR)		return T_SLVV_12;												-- 
	function to_slvv_16(slv : STD_LOGIC_VECTOR)		return T_SLVV_16;												-- 
	function to_slvv_32(slv : STD_LOGIC_VECTOR)		return T_SLVV_32;												-- 
	function to_slvv_64(slv : STD_LOGIC_VECTOR)		return T_SLVV_64;												-- 
	function to_slvv_128(slv : STD_LOGIC_VECTOR)	return T_SLVV_128;											-- 
	function to_slvv_256(slv : STD_LOGIC_VECTOR)	return T_SLVV_256;											-- 
	function to_slvv_512(slv : STD_LOGIC_VECTOR)	return T_SLVV_512;											-- 

	-- Convert matrix to avector-vector: to_slvv_*
	function to_slvv_4(slm : T_SLM)		return T_SLVV_4;																		-- 
	function to_slvv_8(slm : T_SLM)		return T_SLVV_8;																		-- 
	function to_slvv_12(slm : T_SLM)	return T_SLVV_12;																		-- 
	function to_slvv_16(slm : T_SLM)	return T_SLVV_16;																		-- 
	function to_slvv_32(slm : T_SLM)	return T_SLVV_32;																		-- 
	function to_slvv_64(slm : T_SLM)	return T_SLVV_64;																		-- 
	function to_slvv_128(slm : T_SLM)	return T_SLVV_128;																	-- 
	function to_slvv_256(slm : T_SLM)	return T_SLVV_256;																	-- 
	function to_slvv_512(slm : T_SLM)	return T_SLVV_512;																	-- 
	
	-- Convert vector-vector to matrix: to_slm
	function to_slm(slv : STD_LOGIC_VECTOR; ROWS : POSITIVE; COLS : POSITIVE) return T_SLM;	-- create matrix from vector
	function to_slm(slvv : T_SLVV_4) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_8) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_12) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_16) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_32) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_48) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_64) return T_SLM;																					-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_128) return T_SLM;																				-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_256) return T_SLM;																				-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_512) return T_SLM;																				-- create matrix from vector-vector

	-- Change vector direction
	function dir(slvv : T_SLVV_8)			return T_SLVV_8;
	
	-- Reverse vector elements
	function rev(slvv : T_SLVV_4)			return T_SLVV_4;
	function rev(slvv : T_SLVV_8)			return T_SLVV_8;
	function rev(slvv : T_SLVV_12)		return T_SLVV_12;
	function rev(slvv : T_SLVV_16)		return T_SLVV_16;
	function rev(slvv : T_SLVV_32)		return T_SLVV_32;
	function rev(slvv : T_SLVV_64)		return T_SLVV_64;
	function rev(slvv : T_SLVV_128)		return T_SLVV_128;
	function rev(slvv : T_SLVV_256)		return T_SLVV_256;
	function rev(slvv : T_SLVV_512)		return T_SLVV_512;
	
	-- TODO:
	function resize(slm : T_SLM; size : POSITIVE) return T_SLM;

	-- to_string
	function to_string(slvv : T_SLVV_8; sep : CHARACTER := ':') return STRING;
	function to_string(slm : T_SLM; groups : POSITIVE := 4; format : CHARACTER := 'b') return STRING;
end package vectors;


package body vectors is
	-- slicing boundary calulations
	-- ==========================================================================
	function low(lenvec : T_POSVEC; index : NATURAL) return NATURAL is
		variable pos		: NATURAL		:= 0;
	begin
		for i in lenvec'low to index - 1 loop
			pos := pos + lenvec(i);
		end loop;
		return pos;
	end function;
	
	function high(lenvec : T_POSVEC; index : NATURAL) return NATURAL is
		variable pos		: NATURAL		:= 0;
	begin
		for i in lenvec'low to index loop
			pos := pos + lenvec(i);
		end loop;
		return pos - 1;
	end function;

	-- Assign procedures: assign_*
	-- ==========================================================================
	procedure assign_row(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant RowIndex : NATURAL) is
		variable temp : STD_LOGIC_VECTOR(slm'high(2) downto slm'low(2));					-- Xilinx iSIM work-around, because 'range(2) evaluates to 'range(1); tested with ISE/iSIM 14.2
	begin
		temp := slv;
		for i in temp'range loop
			slm(RowIndex, i)  <= temp(i);
		end loop;
	end procedure;
	
	procedure assign_row(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant RowIndex : NATURAL; Position : NATURAL) is
		variable temp : STD_LOGIC_VECTOR(Position + slv'length - 1 downto Position);
	begin
		temp := slv;
		for i in temp'range loop
			slm(RowIndex, i)  <= temp(i);
		end loop;
	end procedure;
	
	procedure assign_row(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant RowIndex : NATURAL; High : NATURAL; Low : NATURAL) is
		variable temp : STD_LOGIC_VECTOR(High downto Low);
	begin
		temp := slv;
		for i in temp'range loop
			slm(RowIndex, i)  <= temp(i);
		end loop;
	end procedure;
	
	procedure assign_col(signal slm : out T_SLM; slv : STD_LOGIC_VECTOR; constant ColIndex : NATURAL) is
		variable temp : STD_LOGIC_VECTOR(slm'range(1));
	begin
		temp := slv;
		for i in temp'range loop
			slm(i, ColIndex)  <= temp(i);
		end loop;
	end procedure;

	-- Matrix to matrix conversion: slm_slice*
	-- ==========================================================================
	function slm_slice(slm : T_SLM; RowIndex : NATURAL; ColIndex : NATURAL; Height : NATURAL; Width : NATURAL) return T_SLM is
		variable Result		: T_SLM(Height - 1 downto 0, Width - 1 downto 0)		:= (others => (others => '0'));
	begin
		for i in 0 to Height - 1 loop
			for j in 0 to Width - 1 loop
				Result(i, j)		:= slm(RowIndex + i, ColIndex + j);
			end loop;
		end loop;
		return Result;
	end function;

	function slm_slice_rows(slm : T_SLM; High : NATURAL; Low : NATURAL) return T_SLM is
		variable Result		: T_SLM(High - Low downto 0, slm'length(2) - 1 downto 0)		:= (others => (others => '0'));
	begin
		for i in 0 to High - Low loop
			for j in 0 to slm'length(2) - 1 loop
				Result(i, j)		:= slm(Low + i, slm'low(2) + j);
			end loop;
		end loop;
		return Result;
	end function;
	
	function slm_slice_cols(slm : T_SLM; High : NATURAL; Low : NATURAL) return T_SLM is
		variable Result		: T_SLM(slm'length(1) - 1 downto 0, High - Low downto 0)		:= (others => (others => '0'));
	begin
		for i in 0 to slm'length(1) - 1 loop
			for j in 0 to High - Low loop
				Result(i, j)		:= slm(slm'low(1) + i, Low + j);
			end loop;
		end loop;
		return Result;
	end function;

	-- Matrix concatenation: slm_merge_*
	function slm_merge_rows(slm1 : T_SLM; slm2 : T_SLM) return T_SLM is
		constant ROWS			: POSITIVE		:= slm1'length(1) + slm2'length(1);
		constant COLUMNS	: POSITIVE		:= slm1'length(2);
		variable slm			: T_SLM(ROWS - 1 downto 0, COLUMNS - 1 downto 0);
	begin
		for i in slm1'range(1) loop
			for j in slm1'low(2) to slm1'high(2) loop
				slm(i, j)		:= slm1(i, j);
			end loop;
		end loop;
		for i in slm2'range(1) loop
			for j in slm2'low(2) to slm2'high(2) loop
				slm(slm1'length(1) + i, j)		:= slm2(i, j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function slm_merge_cols(slm1 : T_SLM; slm2 : T_SLM) return T_SLM is
		constant ROWS			: POSITIVE		:= slm1'length(1);
		constant COLUMNS	: POSITIVE		:= slm1'length(2) + slm2'length(2);
		variable slm			: T_SLM(ROWS - 1 downto 0, COLUMNS - 1 downto 0);
	begin
		for i in slm1'range(1) loop
			for j in slm1'low(2) to slm1'high(2) loop
				slm(i, j)		:= slm1(i, j);
			end loop;
			for j in slm2'low(2) to slm2'high(2) loop
				slm(i, slm1'length(2) + j)		:= slm2(i, j);
			end loop;
		end loop;
		return slm;
	end function;
	
	
	-- Matrix to vector conversion: get_*
	-- ==========================================================================
	-- get a matrix column
	function get_col(slm : T_SLM; ColIndex : NATURAL) return STD_LOGIC_VECTOR is
		variable slv		: STD_LOGIC_VECTOR(slm'range(1));
	begin
		for i in slm'range(1) loop
			slv(i)	:= slm(i, ColIndex);
		end loop;
		return slv;
	end function;
	
	-- get a matrix row
	function get_row(slm : T_SLM; RowIndex : NATURAL) return STD_LOGIC_VECTOR is
		variable slv		: STD_LOGIC_VECTOR(slm'high(2) downto slm'low(2));			-- Xilinx iSIM work-around, because 'range(2) = 'range(1); tested with ISE/iSIM 14.2
	begin
		for i in slv'range loop
			slv(i)	:= slm(RowIndex, i);
		end loop;
		return slv;
	end function;
	
	-- get a matrix row of defined length [length - 1 downto 0]
	function get_row(slm : T_SLM; RowIndex : NATURAL; Length : POSITIVE) return STD_LOGIC_VECTOR is
	begin
		return get_row(slm, RowIndex, (Length - 1), 0);
	end function;

	-- get a sub vector of a matrix row at high:low
	function get_row(slm : T_SLM; RowIndex : NATURAL; High : NATURAL; Low : NATURAL) return STD_LOGIC_VECTOR is
		variable slv		: STD_LOGIC_VECTOR(High downto Low);			-- Xilinx iSIM work-around, because 'range(2) = 'range(1); tested with ISE/iSIM 14.2
	begin
		for i in slv'range loop
			slv(i)	:= slm(RowIndex, i);
		end loop;
		return slv;
	end function;

	-- Convert to vector: to_slv
	-- ==========================================================================
	-- convert vector-vector to flatten vector
	function to_slv(slvv : T_SLVV_2) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 2) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 2) + 1 downto (i * 2))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_4) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 4) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 4) + 3 downto (i * 4))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_8) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 8) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 8) + 7 downto (i * 8))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_12) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 12) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 12) + 11 downto (i * 12))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_16) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 16) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 16) + 15 downto (i * 16))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_24) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 24) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 24) + 23 downto (i * 24))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_32) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 32) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 32) + 31 downto (i * 32))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_64) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 64) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 64) + 63 downto (i * 64))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	function to_slv(slvv : T_SLVV_128) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slvv'length * 128) - 1 downto 0);
	begin
		for i in slvv'range loop
			slv((i * 128) + 127 downto (i * 128))		:= slvv(i);
		end loop;
		return slv;
	end function;
	
	-- convert matrix to flatten vector
	function to_slv(slm : T_SLM) return STD_LOGIC_VECTOR is
		variable slv			: STD_LOGIC_VECTOR((slm'length(1) * slm'length(2)) - 1 downto 0);
	begin
		for i in slm'range(1) loop
			for j in slm'high(2) downto slm'low(2) loop				-- Xilinx iSIM work-around, because 'range(2) evaluates to 'range(1); tested with ISE/iSIM 14.2
				slv((i * slm'length(2)) + j)		:= slm(i, j);
			end loop;
		end loop;
		return slv;
	end function;
	
	
	-- Convert flat vector to a vector-vector: to_slvv_*
	-- ==========================================================================
	-- create vector-vector from vector (4 bit)
	function to_slvv_4(slv : STD_LOGIC_VECTOR) return T_SLVV_4 is
		variable Result		: T_SLVV_4((slv'length / 4) - 1 downto 0);
	begin
		if ((slv'length mod 4) /= 0) then	report "to_slvv_4: width mismatch - slv'length is no multiple of 4 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 4) + 3 downto (i * 4));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (8 bit)
	function to_slvv_8(slv : STD_LOGIC_VECTOR) return T_SLVV_8 is
		variable Result		: T_SLVV_8((slv'length / 8) - 1 downto 0);
	begin
		if ((slv'length mod 8) /= 0) then	report "to_slvv_8: width mismatch - slv'length is no multiple of 8 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 8) + 7 downto (i * 8));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (12 bit)
	function to_slvv_12(slv : STD_LOGIC_VECTOR) return T_SLVV_12 is
		variable Result		: T_SLVV_12((slv'length / 12) - 1 downto 0);
	begin
		if ((slv'length mod 12) /= 0) then	report "to_slvv_12: width mismatch - slv'length is no multiple of 12 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 12) + 11 downto (i * 12));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (16 bit)
	function to_slvv_16(slv : STD_LOGIC_VECTOR) return T_SLVV_16 is
		variable Result		: T_SLVV_16((slv'length / 16) - 1 downto 0);
	begin
		if ((slv'length mod 16) /= 0) then	report "to_slvv_16: width mismatch - slv'length is no multiple of 16 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 16) + 15 downto (i * 16));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (32 bit)
	function to_slvv_32(slv : STD_LOGIC_VECTOR) return T_SLVV_32 is
		variable Result		: T_SLVV_32((slv'length / 32) - 1 downto 0);
	begin
		if ((slv'length mod 32) /= 0) then	report "to_slvv_32: width mismatch - slv'length is no multiple of 32 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 32) + 31 downto (i * 32));
		end loop;
		return Result;
	end function;

	-- create vector-vector from vector (64 bit)
	function to_slvv_64(slv : STD_LOGIC_VECTOR) return T_SLVV_64 is
		variable Result		: T_SLVV_64((slv'length / 64) - 1 downto 0);
	begin
		if ((slv'length mod 64) /= 0) then	report "to_slvv_64: width mismatch - slv'length is no multiple of 64 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 64) + 63 downto (i * 64));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (128 bit)
	function to_slvv_128(slv : STD_LOGIC_VECTOR) return T_SLVV_128 is
		variable Result		: T_SLVV_128((slv'length / 128) - 1 downto 0);
	begin
		if ((slv'length mod 128) /= 0) then	report "to_slvv_128: width mismatch - slv'length is no multiple of 128 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 128) + 127 downto (i * 128));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (256 bit)
	function to_slvv_256(slv : STD_LOGIC_VECTOR) return T_SLVV_256 is
		variable Result		: T_SLVV_256((slv'length / 256) - 1 downto 0);
	begin
		if ((slv'length mod 256) /= 0) then	report "to_slvv_256: width mismatch - slv'length is no multiple of 256 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 256) + 255 downto (i * 256));
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from vector (512 bit)
	function to_slvv_512(slv : STD_LOGIC_VECTOR) return T_SLVV_512 is
		variable Result		: T_SLVV_512((slv'length / 512) - 1 downto 0);
	begin
		if ((slv'length mod 512) /= 0) then	report "to_slvv_512: width mismatch - slv'length is no multiple of 512 (slv'length=" & INTEGER'image(slv'length) & ")" severity FAILURE;	end if;
		
		for i in Result'range loop
			Result(i)	:= slv((i * 512) + 511 downto (i * 512));
		end loop;
		return Result;
	end function;

	-- Convert matrix to avector-vector: to_slvv_*
	-- ==========================================================================
	-- create vector-vector from matrix (4 bit)
	function to_slvv_4(slm : T_SLM) return T_SLVV_4 is
		variable Result		: T_SLVV_4(slm'range(1));
	begin
		if (slm'length(2) /= 4) then	report "to_slvv_4: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (8 bit)
	function to_slvv_8(slm : T_SLM) return T_SLVV_8 is
		variable Result		: T_SLVV_8(slm'range(1));
	begin
		if (slm'length(2) /= 8) then	report "to_slvv_8: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (12 bit)
	function to_slvv_12(slm : T_SLM) return T_SLVV_12 is
		variable Result		: T_SLVV_12(slm'range(1));
	begin
		if (slm'length(2) /= 12) then	report "to_slvv_12: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (16 bit)
	function to_slvv_16(slm : T_SLM) return T_SLVV_16 is
		variable Result		: T_SLVV_16(slm'range(1));
	begin
		if (slm'length(2) /= 16) then	report "to_slvv_16: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (32 bit)
	function to_slvv_32(slm : T_SLM) return T_SLVV_32 is
		variable Result		: T_SLVV_32(slm'range(1));
	begin
		if (slm'length(2) /= 32) then	report "to_slvv_32: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;

	-- create vector-vector from matrix (64 bit)
	function to_slvv_64(slm : T_SLM) return T_SLVV_64 is
		variable Result		: T_SLVV_64(slm'range(1));
	begin
		if (slm'length(2) /= 64) then	report "to_slvv_64: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (128 bit)
	function to_slvv_128(slm : T_SLM) return T_SLVV_128 is
		variable Result		: T_SLVV_128(slm'range(1));
	begin
		if (slm'length(2) /= 128) then	report "to_slvv_128: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (256 bit)
	function to_slvv_256(slm : T_SLM) return T_SLVV_256 is
		variable Result		: T_SLVV_256(slm'range);
	begin
		if (slm'length(2) /= 256) then	report "to_slvv_256: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- create vector-vector from matrix (512 bit)
	function to_slvv_512(slm : T_SLM) return T_SLVV_512 is
		variable Result		: T_SLVV_512(slm'range(1));
	begin
		if (slm'length(2) /= 512) then	report "to_slvv_512: type mismatch - slm'length(2)=" & INTEGER'image(slm'length(2)) severity FAILURE;	end if;
		
		for i in slm'range(1) loop
			Result(i)	:= get_row(slm, i);
		end loop;
		return Result;
	end function;
	
	-- Convert vector-vector to matrix: to_slm
	-- ==========================================================================
	-- create matrix from vector
	function to_slm(slv : STD_LOGIC_VECTOR; ROWS : POSITIVE; COLS : POSITIVE) return T_SLM is
		variable slm		: T_SLM(ROWS - 1 downto 0, COLS - 1 downto 0);
	begin
		for i in 0 to ROWS - 1 loop
			for j in 0 to COLS - 1 loop
				slm(i, j)	:= slv((i * COLS) + j);
			end loop;
		end loop;
		return slm;
	end function;
	
	-- create matrix from vector-vector
	function to_slm(slvv : T_SLVV_4) return T_SLM is
		variable slm		: T_SLM(slvv'range, 3 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_4'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_8) return T_SLM is
--		variable test		: STD_LOGIC_VECTOR(T_SLV_8'range);
--		variable slm		: T_SLM(slvv'range, test'range);				-- BUG: iSIM 14.5 cascaded 'range accesses let iSIM break down 
--		variable slm		: T_SLM(slvv'range, T_SLV_8'range);			-- BUG: iSIM 14.5 allocates 9 bits in dimmension 2
		variable slm		: T_SLM(slvv'range, 7 downto 0);
	begin
--		report "slvv:    slvv.length=" & INTEGER'image(slvv'length) &			"  slm.dim0.length=" & INTEGER'image(slm'length(1)) & "  slm.dim1.length=" & INTEGER'image(slm'length(2)) severity NOTE;
--		report "T_SLV_8:     .length=" & INTEGER'image(T_SLV_8'length) &	"  .high=" & INTEGER'image(T_SLV_8'high) &	"  .low=" & INTEGER'image(T_SLV_8'low)	severity NOTE;
--		report "test:    test.length=" & INTEGER'image(test'length) &			"  .high=" & INTEGER'image(test'high) &			"  .low=" & INTEGER'image(test'low)			severity NOTE;
		for i in slvv'range loop
			for j in T_SLV_8'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_12) return T_SLM is
		variable slm		: T_SLM(slvv'range, 11 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_12'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_16) return T_SLM is
		variable slm		: T_SLM(slvv'range, 15 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_16'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_32) return T_SLM is
		variable slm		: T_SLM(slvv'range, 31 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_32'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_48) return T_SLM is
		variable slm		: T_SLM(slvv'range, 47 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_48'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_64) return T_SLM is
		variable slm		: T_SLM(slvv'range, 63 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_64'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_128) return T_SLM is
		variable slm		: T_SLM(slvv'range, 127 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_128'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_256) return T_SLM is
		variable slm		: T_SLM(slvv'range, 255 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_256'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;
	
	function to_slm(slvv : T_SLVV_512) return T_SLM is
		variable slm		: T_SLM(slvv'range, 511 downto 0);
	begin
		for i in slvv'range loop
			for j in T_SLV_512'range loop
				slm(i, j)		:= slvv(i)(j);
			end loop;
		end loop;
		return slm;
	end function;

	-- Change vector direction
	-- ==========================================================================
	function dir(slvv : T_SLVV_8) return T_SLVV_8 is
		variable Result : T_SLVV_8(slvv'reverse_range);
	begin
		Result := slvv;
		return Result;
	end function;
	
	-- Reverse vector elements
	function rev(slvv : T_SLVV_4) return T_SLVV_4 is
		variable Result : T_SLVV_4(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;

	function rev(slvv : T_SLVV_8) return T_SLVV_8 is
		variable Result : T_SLVV_8(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_12) return T_SLVV_12 is
		variable Result : T_SLVV_12(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_16) return T_SLVV_16 is
		variable Result : T_SLVV_16(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_32) return T_SLVV_32 is
		variable Result : T_SLVV_32(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_64) return T_SLVV_64 is
		variable Result : T_SLVV_64(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_128) return T_SLVV_128 is
		variable Result : T_SLVV_128(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_256) return T_SLVV_256 is
		variable Result : T_SLVV_256(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;
	
	function rev(slvv : T_SLVV_512) return T_SLVV_512 is
		variable Result : T_SLVV_512(slvv'range);
	begin
		for i in slvv'low to slvv'high loop
			Result(slvv'high - i) := slvv(i);
		end loop;
		return Result;
	end function;

	-- Resize functions
	-- ==========================================================================
	-- Resizes the vector to the specified length. Input vectors larger than the specified size are truncated from the left side. Smaller input
	-- vectors are extended on the left by the provided fill value (default: '0'). Use the resize functions of the numeric_std package for
	-- value-preserving resizes of the signed and unsigned data types.
	function resize(slm : T_SLM; size : POSITIVE) return T_SLM is
		variable Result		: T_SLM(size - 1 downto 0, slm'high(2) downto slm'low(2))		:= (others => (others => '0'));
	begin
		for i in slm'range(1) loop
			for j in slm'high(2) downto slm'low(2) loop
				Result(i, j)	:= slm(i, j);
			end loop;
		end loop;
		return Result;
	end function;
	
	function to_string(slvv : T_SLVV_8; sep : CHARACTER := ':') return STRING is
		constant hex_len			: POSITIVE								:= ite((sep = C_POC_NUL), (slvv'length * 2), (slvv'length * 3) - 1);
		variable Result				: STRING(1 to hex_len)		:= (others => sep);
		variable pos					: POSITIVE								:= 1;
	begin
		for i in slvv'range loop
			Result(pos to pos + 1)	:= to_string(slvv(i), 'h');
			pos											:= pos + ite((sep = C_POC_NUL), 2, 3);
		end loop;
		return Result;
	end function;
	
	function to_string_bin(slm : T_SLM; groups : POSITIVE := 4; format : CHARACTER := 'h') return STRING is
		variable PerLineOverheader	: POSITIVE	:= div_ceil(slm'length(2), groups);
		variable Result							: STRING(1 to (slm'length(1) * (slm'length(2) + PerLineOverheader)) + 10);
		variable Writer							: POSITIVE;
		variable GroupCounter				: NATURAL;
	begin
		Result				:= (others => C_POC_NUL);
		Result(1)			:= LF;
		Writer				:= 2;
		GroupCounter	:= 0;
		for i in slm'low(1) to slm'high(1) loop
			for j in slm'high(2) downto slm'low(2) loop
				Result(Writer)		:= to_char(slm(i, j));
				Writer						:= Writer + 1;
				GroupCounter			:= GroupCounter + 1;
				if (GroupCounter = groups) then
					Result(Writer)	:= ' ';
					Writer					:= Writer + 1;
					GroupCounter		:= 0;
				end if;
			end loop;
			Result(Writer - 1)	:= LF;
			GroupCounter				:= 0;
		end loop;
		return str_trim(Result);
	end function;
	
	function to_string(slm : T_SLM; groups : POSITIVE := 4; format : CHARACTER := 'b') return STRING is
	begin
		if (format = 'b') then
			return to_string_bin(slm, groups);
		else
			return "Format not supported.";
		end if;
	end function;
end package body;
