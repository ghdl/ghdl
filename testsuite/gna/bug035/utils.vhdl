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
-- ============================================================================

library	IEEE;

use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library	PoC;
use			PoC.my_config.all;


package utils is
	-- PoC settings
	-- ==========================================================================
	constant POC_VERBOSE					: BOOLEAN			:= MY_VERBOSE;	

  -- Environment
	-- ==========================================================================
  -- Distinguishes simulation from synthesis
	constant SIMULATION					: BOOLEAN;				-- deferred constant declaration
	
	-- Type declarations
	-- ==========================================================================
	
  --+ Vectors of primitive standard types +++++++++++++++++++++++++++++++++++++
	type		T_BOOLVEC						is array(NATURAL range <>) of BOOLEAN;
	type		T_INTVEC						is array(NATURAL range <>) of INTEGER;
	type		T_NATVEC						is array(NATURAL range <>) of NATURAL;
	type		T_POSVEC						is array(NATURAL range <>) of POSITIVE;
	type		T_REALVEC						is array(NATURAL range <>) of REAL;
	
	--+ Integer subranges sometimes useful for speeding up simulation ++++++++++
	subtype T_INT_8							is INTEGER range -128 to 127;
	subtype T_INT_16						is INTEGER range -32768 to 32767;
	subtype T_UINT_8						is INTEGER range 0 to 255;
	subtype T_UINT_16						is INTEGER range 0 to 65535;

	--+ Enums ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	-- Intellectual Property (IP) type
	type T_IPSTYLE				is (IPSTYLE_HARD, IPSTYLE_SOFT);
	
	-- Bit Order
	type T_BIT_ORDER			is (LSB_FIRST, MSB_FIRST);
  
	-- Byte Order (Endian)
	type T_BYTE_ORDER			is (LITTLE_ENDIAN, BIG_ENDIAN);
	
	-- rounding style
	type T_ROUNDING_STYLE	is (ROUND_TO_NEAREST, ROUND_TO_ZERO, ROUND_TO_INF, ROUND_UP, ROUND_DOWN);

	type T_BCD				is array(3 downto 0) of std_logic;
	type T_BCD_VECTOR	is array(NATURAL range <>) of T_BCD;
	constant C_BCD_MINUS	: T_BCD		:= "1010";
	constant C_BCD_OFF		: T_BCD		:= "1011";
	
	
	-- Function declarations
	-- ==========================================================================

  --+ Division ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  -- Calculates: ceil(a / b)
	function div_ceil(a : NATURAL; b : POSITIVE) return NATURAL;
	
  --+ Power +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  -- is input a power of 2?
	function is_pow2(int : NATURAL)			return BOOLEAN;
  -- round to next power of 2
	function ceil_pow2(int : NATURAL)		return POSITIVE;
  -- round to previous power of 2
	function floor_pow2(int : NATURAL)	return NATURAL;

  --+ Logarithm ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  -- Calculates: ceil(ld(arg))
  function log2ceil(arg : positive) return natural;
  -- Calculates: max(1, ceil(ld(arg)))
  function log2ceilnz(arg : positive) return positive;
  -- Calculates: ceil(lg(arg))
	function log10ceil(arg		: POSITIVE)	return NATURAL;
  -- Calculates: max(1, ceil(lg(arg)))
	function log10ceilnz(arg	: POSITIVE)	return POSITIVE;
	
	--+ if-then-else (ite) +++++++++++++++++++++++++++++++++++++++++++++++++++++
	function ite(cond : BOOLEAN; value1 : BOOLEAN; value2 : BOOLEAN) return BOOLEAN;
	function ite(cond : BOOLEAN; value1 : INTEGER; value2 : INTEGER) return INTEGER;
	function ite(cond : BOOLEAN; value1 : REAL;	value2 : REAL) return REAL;
	function ite(cond : BOOLEAN; value1 : STD_LOGIC; value2 : STD_LOGIC) return STD_LOGIC;
	function ite(cond : BOOLEAN; value1 : STD_LOGIC_VECTOR; value2 : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
	function ite(cond : BOOLEAN; value1 : BIT_VECTOR; value2 : BIT_VECTOR) return BIT_VECTOR;
	function ite(cond : BOOLEAN; value1 : UNSIGNED; value2 : UNSIGNED) return UNSIGNED;
	function ite(cond : BOOLEAN; value1 : CHARACTER; value2 : CHARACTER) return CHARACTER;
	function ite(cond : BOOLEAN; value1 : STRING; value2 : STRING) return STRING;

  --+ Max / Min / Sum ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	function imin(arg1 : integer; arg2 : integer) return integer;		-- Calculates: min(arg1, arg2) for integers
	function rmin(arg1 : real; arg2 : real) return real;						-- Calculates: min(arg1, arg2) for reals
	
	function imin(vec : T_INTVEC) return INTEGER;										-- Calculates: min(vec) for a integer vector
	function imin(vec : T_NATVEC) return NATURAL;										-- Calculates: min(vec) for a natural vector
	function imin(vec : T_POSVEC) return POSITIVE;									-- Calculates: min(vec) for a positive vector
	function rmin(vec : T_REALVEC) return real;	       							-- Calculates: min(vec) of real vector

	function imax(arg1 : integer; arg2 : integer) return integer;		-- Calculates: max(arg1, arg2) for integers
	function rmax(arg1 : real; arg2 : real) return real;						-- Calculates: max(arg1, arg2) for reals
	
	function imax(vec : T_INTVEC) return INTEGER;										-- Calculates: max(vec) for a integer vector
	function imax(vec : T_NATVEC) return NATURAL;										-- Calculates: max(vec) for a natural vector
	function imax(vec : T_POSVEC) return POSITIVE;									-- Calculates: max(vec) for a positive vector
	function rmax(vec : T_REALVEC) return real;	       							-- Calculates: max(vec) of real vector

	function isum(vec : T_NATVEC) return NATURAL;										-- Calculates: sum(vec) for a natural vector
	function isum(vec : T_POSVEC) return natural;										-- Calculates: sum(vec) for a positive vector
	function isum(vec : T_INTVEC) return integer; 									-- Calculates: sum(vec) of integer vector
	function rsum(vec : T_REALVEC) return real;	       							-- Calculates: sum(vec) of real vector

	--+ Conversions ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	-- to integer: to_int
	function to_int(bool : BOOLEAN; zero : INTEGER := 0; one : INTEGER := 1)		return INTEGER;
	function to_int(sl : STD_LOGIC; zero : INTEGER := 0; one : INTEGER := 1)		return INTEGER;
	
	-- to std_logic: to_sl
	function to_sl(Value : BOOLEAN)		return STD_LOGIC;
	function to_sl(Value : CHARACTER) return STD_LOGIC;

	-- to std_logic_vector: to_slv
	function to_slv(Value : NATURAL; Size : POSITIVE)		return STD_LOGIC_VECTOR;					-- short for std_logic_vector(to_unsigned(Value, Size))
	
	-- TODO: comment
	function to_index(slv : UNSIGNED; max : NATURAL := 0) return INTEGER;
	function to_index(slv : STD_LOGIC_VECTOR; max : NATURAL := 0) return INTEGER;
	
	-- is_*
	function is_sl(c : CHARACTER) return BOOLEAN;

	--+ Basic Vector Utilities +++++++++++++++++++++++++++++++++++++++++++++++++

  -- Aggregate functions
  function slv_or  (vec : STD_LOGIC_VECTOR) return STD_LOGIC;
  function slv_nor (vec : STD_LOGIC_VECTOR) return STD_LOGIC;
  function slv_and (vec : STD_LOGIC_VECTOR) return STD_LOGIC;
  function slv_nand(vec : STD_LOGIC_VECTOR) return STD_LOGIC;
  function slv_xor (vec : std_logic_vector) return std_logic;
	-- NO slv_xnor! This operation would not be well-defined as
	-- not xor(vec) /= vec_{n-1} xnor ... xnor vec_1 xnor vec_0 iff n is odd.

  -- Reverses the elements of the passed Vector.
  --
  -- @synthesis supported
  --
	function reverse(vec : std_logic_vector) return std_logic_vector;
	function reverse(vec : bit_vector) return bit_vector;
	function reverse(vec : unsigned) return unsigned;
	
  -- Resizes the vector to the specified length. The adjustment is make on
  -- on the 'high end of the vector. The 'low index remains as in the argument.
  -- If the result vector is larger, the extension uses the provided fill value
  -- (default: '0').
	-- Use the resize functions of the numeric_std package for value-preserving
	-- resizes of the signed and unsigned data types.
	--
  -- @synthesis supported
  --
  function resize(vec : bit_vector; length : natural; fill : bit := '0')
    return bit_vector;
  function resize(vec : std_logic_vector; length : natural; fill : std_logic := '0')
    return std_logic_vector;

	-- Shift the index range of a vector by the specified offset.
	function move(vec : std_logic_vector; ofs : integer) return std_logic_vector;

	-- Shift the index range of a vector making vec'low = 0.
	function movez(vec : std_logic_vector) return std_logic_vector;

  function ascend(vec : std_logic_vector) return std_logic_vector;
  function descend(vec : std_logic_vector) return std_logic_vector;
	
  -- Least-Significant Set Bit (lssb):
  -- Computes a vector of the same length as the argument with
  -- at most one bit set at the rightmost '1' found in arg.
  --
  -- @synthesis supported
  --
  function lssb(arg : std_logic_vector) return std_logic_vector;
  function lssb(arg : bit_vector) return bit_vector;

  -- Returns the index of the least-significant set bit.
  --
  -- @synthesis supported
  --
  function lssb_idx(arg : std_logic_vector) return integer;
  function lssb_idx(arg : bit_vector) return integer;

	-- Most-Significant Set Bit (mssb): computes a vector of the same length
	-- with at most one bit set at the leftmost '1' found in arg.
	function mssb(arg : std_logic_vector) return std_logic_vector;
  function mssb(arg : bit_vector) return bit_vector;
	function mssb_idx(arg : std_logic_vector) return integer;
  function mssb_idx(arg : bit_vector) return integer;

	-- Swap sub vectors in vector (endian reversal)
	function swap(slv : STD_LOGIC_VECTOR; Size : POSITIVE) return STD_LOGIC_VECTOR;

	-- generate bit masks
	function genmask_high(Bits : NATURAL; MaskLength : POSITIVE) return STD_LOGIC_VECTOR;
	function genmask_low(Bits : NATURAL; MaskLength : POSITIVE) return STD_LOGIC_VECTOR;

	--+ Encodings ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  -- One-Hot-Code to Binary-Code.
	function onehot2bin(onehot : std_logic_vector) return unsigned;

  -- Converts Gray-Code into Binary-Code.
  --
  -- @synthesis supported
  --
  function gray2bin (gray_val : std_logic_vector) return std_logic_vector;
	
	-- Binary-Code to One-Hot-Code
	function bin2onehot(value : std_logic_vector) return std_logic_vector;
	
	-- Binary-Code to Gray-Code
	function bin2gray(value : std_logic_vector) return std_logic_vector;
	
end package;


package body utils is

	-- Environment
	-- ==========================================================================
	function is_simulation return boolean is
		variable ret : boolean;
	begin
		ret := false;
		--synthesis translate_off
		if Is_X('X') then ret := true; end if;
		--synthesis translate_on
		return	ret;
	end function;

	-- deferred constant assignment
	constant SIMULATION	: BOOLEAN		:= is_simulation;

	-- Divisions: div_*
	function div_ceil(a : NATURAL; b : POSITIVE) return NATURAL is	-- calculates: ceil(a / b)
	begin
		return (a + (b - 1)) / b;
	end function;

	-- Power functions: *_pow2
	-- ==========================================================================
	-- is input a power of 2?
	function is_pow2(int : NATURAL) return BOOLEAN is
	begin
		return ceil_pow2(int) = int;
	end function;
	
	-- round to next power of 2
	function ceil_pow2(int : NATURAL) return POSITIVE is
	begin
		return 2 ** log2ceil(int);
	end function;
	
	-- round to previous power of 2
	function floor_pow2(int : NATURAL) return NATURAL is
		variable temp : UNSIGNED(30 downto 0);
	begin
		temp	:= to_unsigned(int, 31);
		for i in temp'range loop
			if (temp(i) = '1') then
				return 2 ** i;
			end if;
		end loop;
		return 0;
	end function;

	-- Logarithms: log*ceil*
	-- ==========================================================================
	function log2ceil(arg : positive) return natural is
		variable tmp : positive;
		variable log : natural;
	begin
		if arg = 1 then	return 0; end if;
		tmp := 1;
		log := 0;
		while arg > tmp loop
			tmp := tmp * 2;
			log := log + 1;
		end loop;
		return log;
	end function;

	function log2ceilnz(arg : positive) return positive is
	begin
		return imax(1, log2ceil(arg));
	end function;

	function log10ceil(arg : positive) return natural is
		variable tmp : positive;
		variable log : natural;
	begin
		if arg = 1 then	return 0; end if;
		tmp := 1;
		log := 0;
		while arg > tmp loop
			tmp := tmp * 10;
			log := log + 1;
		end loop;
		return log;
	end function;

	function log10ceilnz(arg : positive) return positive is
	begin
		return imax(1, log10ceil(arg));
	end function;

	-- if-then-else (ite)
	-- ==========================================================================
	function ite(cond : BOOLEAN; value1 : BOOLEAN; value2 : BOOLEAN) return BOOLEAN is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	function ite(cond : BOOLEAN; value1 : INTEGER; value2 : INTEGER) return INTEGER is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : BOOLEAN; value1 : REAL; value2 : REAL) return REAL is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : BOOLEAN; value1 : STD_LOGIC; value2 : STD_LOGIC) return STD_LOGIC is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : BOOLEAN; value1 : STD_LOGIC_VECTOR; value2 : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	function ite(cond : BOOLEAN; value1 : BIT_VECTOR; value2 : BIT_VECTOR) return BIT_VECTOR is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : BOOLEAN; value1 : UNSIGNED; value2 : UNSIGNED) return UNSIGNED is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : BOOLEAN; value1 : CHARACTER; value2 : CHARACTER) return CHARACTER is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	function ite(cond : BOOLEAN; value1 : STRING; value2 : STRING) return STRING is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	-- *min / *max / *sum
	-- ==========================================================================
	function imin(arg1 : integer; arg2 : integer) return integer is
	begin
		if arg1 < arg2 then return arg1; end if;
		return arg2;
	end function;

	function rmin(arg1 : real; arg2 : real) return real is
	begin
		if arg1 < arg2 then return arg1; end if;
		return arg2;
	end function;
	
	function imin(vec : T_INTVEC) return INTEGER is
		variable Result		: INTEGER;
	begin
		Result	:= INTEGER'high;
		for i in vec'range loop
			if (vec(I) < Result) then
				Result	:= vec(I);
			end if;
		end loop;
		return Result;
	end function;
	
	function imin(vec : T_NATVEC) return NATURAL is
		variable Result		: NATURAL;
	begin
		Result	:= NATURAL'high;
		for i in vec'range loop
			if (vec(I) < Result) then
				Result	:= vec(I);
			end if;
		end loop;
		return Result;
	end function;
	
	function imin(vec : T_POSVEC) return POSITIVE is
		variable Result		: POSITIVE;
	begin
		Result	:= POSITIVE'high;
		for i in vec'range loop
			if (vec(I) < Result) then
				Result	:= vec(I);
			end if;
		end loop;
		return Result;
	end function;

	function rmin(vec : T_REALVEC) return REAL is
		variable  Result : REAL;
	begin
		Result	:= REAL'high;
		for i in vec'range loop
			if vec(i) < Result then
				Result := vec(i);
			end if;
		end loop;
		return  Result;
	end function;

	function imax(arg1 : integer; arg2 : integer) return integer is
	begin
		if arg1 > arg2 then return arg1; end if;
		return arg2;
	end function;

	function rmax(arg1 : real; arg2 : real) return real is
	begin
		if arg1 > arg2 then return arg1; end if;
		return arg2;
	end function;
	
	function imax(vec : T_INTVEC) return INTEGER is
		variable Result		: INTEGER;
	begin
		Result		:= INTEGER'low;
		for i in vec'range loop
			if (vec(I) > Result) then
				Result	:= vec(I);
			end if;
		end loop;
		return Result;
	end function;
	
	function imax(vec : T_NATVEC) return NATURAL is
		variable Result		: NATURAL;
	begin
		Result		:= NATURAL'low;
		for i in vec'range loop
			if (vec(I) > Result) then
				Result	:= vec(I);
			end if;
		end loop;
		return Result;
	end function;
	
	function imax(vec : T_POSVEC) return POSITIVE is
		variable Result		: POSITIVE;
	begin
		Result		:= POSITIVE'low;
		for i in vec'range loop
			if (vec(I) > Result) then
				Result	:= vec(I);
			end if;
		end loop;
		return Result;
	end function;

	function rmax(vec : T_REALVEC) return REAL is
		variable  Result : REAL;
	begin
		Result		:= REAL'low;
		for i in vec'range loop
			if vec(i) > Result then
				Result := vec(i);
			end if;
		end loop;
		return  Result;
	end function;

	function isum(vec : T_INTVEC) return INTEGER is
		variable  Result : INTEGER;
	begin
		Result		:= 0;
		for i in vec'range loop
			Result	:= Result + vec(i);
		end loop;
		return  Result;
	end function;
	
	function isum(vec : T_NATVEC) return NATURAL is
		variable Result		: NATURAL;
	begin
		Result		:= 0;
		for i in vec'range loop
			Result	:= Result + vec(I);
		end loop;
		return Result;
	end function;
	
	function isum(vec : T_POSVEC) return natural is
		variable Result : natural;
	begin
		Result := 0;
		for i in vec'range loop
			Result := Result + vec(I);
		end loop;
		return Result;
	end function;

	function rsum(vec : T_REALVEC) return REAL is
		variable  Result : REAL;
	begin
		Result		:= 0.0;
		for i in vec'range loop
			Result	:= Result + vec(i);
		end loop;
		return  Result;
	end function;

	-- Vector aggregate functions: slv_*
	-- ==========================================================================
	function slv_or(vec : STD_LOGIC_VECTOR) return STD_LOGIC is
		variable Result : STD_LOGIC;
	begin
		Result		:= '0';
		for i in vec'range loop
			Result	:= Result or vec(i);
		end loop;
		return Result;
	end function;

	function slv_nor(vec : STD_LOGIC_VECTOR) return STD_LOGIC is
	begin
		return not slv_or(vec);
	end function;

	function slv_and(vec : STD_LOGIC_VECTOR) return STD_LOGIC is
		variable Result : STD_LOGIC;
	begin
		Result		:= '1';
		for i in vec'range loop
			Result	:= Result and vec(i);
		end loop;
		return Result;
	end function;

	function slv_nand(vec : STD_LOGIC_VECTOR) return STD_LOGIC is
	begin
		return not slv_and(vec);
	end function;

	function slv_xor(vec : std_logic_vector) return std_logic is
		variable  res : std_logic;
	begin
		res := '0';
		for i in vec'range loop
			res := res xor vec(i);
		end loop;
		return  res;
	end slv_xor;

	-- Convert to integer: to_int
	function to_int(bool : BOOLEAN; zero : INTEGER := 0; one : INTEGER := 1) return INTEGER is
	begin
		return ite(bool, one, zero);
	end function;
	
	function to_int(sl : STD_LOGIC; zero : INTEGER := 0; one : INTEGER := 1) return INTEGER is
	begin
		if (sl = '1') then
			return one;
		end if;
		return zero;
	end function;
	
	-- Convert to bit: to_sl
	-- ==========================================================================
	function to_sl(Value : BOOLEAN) return STD_LOGIC is
	begin
		return ite(Value, '1', '0');
	end function;

	function to_sl(Value : CHARACTER) return STD_LOGIC is
	begin
		case Value is
			when 'U' =>			return 'U';
			when '0' =>			return '0';
			when '1' =>			return '1';
			when 'Z' =>			return 'Z';
			when 'W' =>			return 'W';
			when 'L' =>			return 'L';
			when 'H' =>			return 'H';
			when '-' =>			return '-';
			when OTHERS =>	return 'X';
		end case;
	end function;

	-- Convert to vector: to_slv
	-- ==========================================================================
	-- short for std_logic_vector(to_unsigned(Value, Size))
	-- the return value is guaranteed to have the range (Size-1 downto 0)
	function to_slv(Value : NATURAL; Size : POSITIVE) return STD_LOGIC_VECTOR is
	  constant  res : std_logic_vector(Size-1 downto 0) := std_logic_vector(to_unsigned(Value, Size));
	begin
		return  res;
	end function;

	function to_index(slv : UNSIGNED; max : NATURAL := 0) return INTEGER is
		variable  res : integer;
	begin
		if (slv'length = 0) then	return 0;	end if;
	
		res := to_integer(slv);
		if SIMULATION and max > 0 then
			res := imin(res, max);
		end if;
		return  res;
	end function;

	function to_index(slv : STD_LOGIC_VECTOR; max : NATURAL := 0) return INTEGER is
	begin
		return to_index(unsigned(slv), max);
	end function;
	
  -- is_*
  -- ==========================================================================
  function is_sl(c : CHARACTER) return BOOLEAN is
  begin
    case c is
      when 'U'|'X'|'0'|'1'|'Z'|'W'|'L'|'H'|'-' => return  true;
      when OTHERS                              => return  false;
    end case;
  end function;

	
	-- Reverse vector elements
	function reverse(vec : std_logic_vector) return std_logic_vector is
		variable res : std_logic_vector(vec'range);
	begin
		for i in vec'low to vec'high loop
			res(vec'low + (vec'high-i)) := vec(i);
		end loop;
		return	res;
	end function;
	
	function reverse(vec : bit_vector) return bit_vector is
		variable res : bit_vector(vec'range);
	begin
    res := to_bitvector(reverse(to_stdlogicvector(vec)));
    return  res;
	end reverse;
	
	function reverse(vec : unsigned) return unsigned is
	begin
		return unsigned(reverse(std_logic_vector(vec)));
	end function;

	
	-- Swap sub vectors in vector
	-- ==========================================================================
	function swap(slv : STD_LOGIC_VECTOR; Size : POSITIVE) return STD_LOGIC_VECTOR IS
		CONSTANT SegmentCount	: NATURAL													:= slv'length / Size;
		variable FromH				: NATURAL;
		variable FromL				: NATURAL;
		variable ToH					: NATURAL;
		variable ToL					: NATURAL;
		variable Result : STD_LOGIC_VECTOR(slv'length - 1 DOWNTO 0);
	begin
		for i in 0 TO SegmentCount - 1 loop
			FromH		:= ((I + 1) * Size) - 1;
			FromL		:= I * Size;
			ToH			:= ((SegmentCount - I) * Size) - 1;
			ToL			:= (SegmentCount - I - 1) * Size;
			Result(ToH DOWNTO ToL)	:= slv(FromH DOWNTO FromL);
		end loop;
		return Result;
	end function;

	-- generate bit masks
	-- ==========================================================================
	function genmask_high(Bits : NATURAL; MaskLength : POSITIVE) return STD_LOGIC_VECTOR IS
	begin
		if (Bits = 0) then
			return (MaskLength - 1 DOWNTO 0 => '0');
		else	
			return (MaskLength - 1 DOWNTO MaskLength - Bits + 1 => '1') & (MaskLength - Bits DOWNTO 0 => '0');
		end if;
	end function;

	function genmask_low(Bits : NATURAL; MaskLength : POSITIVE) return STD_LOGIC_VECTOR is
	begin
		if (Bits = 0) then
			return (MaskLength - 1 DOWNTO 0 => '0');
		else	
			return (MaskLength - 1 DOWNTO Bits => '0') & (Bits - 1 DOWNTO 0 => '1');
		end if;
	end function;

	-- binary encoding conversion functions
	-- ==========================================================================
	-- One-Hot-Code to Binary-Code
  function onehot2bin(onehot : std_logic_vector) return unsigned is
		variable res : unsigned(log2ceilnz(onehot'high+1)-1 downto 0);
		variable chk : natural;
	begin
		res := (others => '0');
		chk := 0;
		for i in onehot'range loop
			if onehot(i) = '1' then
				res := res or to_unsigned(i, res'length);
				chk := chk + 1;
			end if;
		end loop;
		if SIMULATION and chk /= 1 then
			report "Broken 1-Hot-Code with "&integer'image(chk)&" bits set."
				severity error;
		end if;
		return	res;
	end onehot2bin;

	-- Gray-Code to Binary-Code
	function gray2bin(gray_val : std_logic_vector) return std_logic_vector is
		variable res : std_logic_vector(gray_val'range);
	begin	-- gray2bin
		res(res'left) := gray_val(gray_val'left);
		for i in res'left-1 downto res'right loop
			res(i) := res(i+1) xor gray_val(i);
		end loop;
		return res;
	end gray2bin;
	
	-- Binary-Code to One-Hot-Code
	function bin2onehot(value : std_logic_vector) return std_logic_vector is
		variable result		: std_logic_vector(2**value'length - 1 downto 0);
	begin
		result	:= (others => '0');
		result(to_index(value, 0)) := '1';
		return result;
	end function;
	
	-- Binary-Code to Gray-Code
	function bin2gray(value : std_logic_vector) return std_logic_vector is
		variable result		: std_logic_vector(value'range);
	begin
		result(result'left)	:= value(value'left);
		for i in (result'left - 1) downto result'right loop
			result(i) := value(i) xor value(i + 1);
		end loop;
		return result;
	end function;

	-- bit searching / bit indices
	-- ==========================================================================
	-- Least-Significant Set Bit (lssb): computes a vector of the same length with at most one bit set at the rightmost '1' found in arg.
	function lssb(arg : std_logic_vector) return std_logic_vector is
    variable  res : std_logic_vector(arg'range);
	begin
		res := arg and std_logic_vector(unsigned(not arg)+1);
    return  res;
	end function;
	
  function lssb(arg : bit_vector) return bit_vector is
    variable  res : bit_vector(arg'range);
  begin
    res := to_bitvector(lssb(to_stdlogicvector(arg)));
    return  res;
  end lssb;

	-- Most-Significant Set Bit (mssb): computes a vector of the same length with at most one bit set at the leftmost '1' found in arg.
	function mssb(arg : std_logic_vector) return std_logic_vector is
	begin
		return	reverse(lssb(reverse(arg)));
	end function;
	
  function mssb(arg : bit_vector) return bit_vector is
  begin
    return  reverse(lssb(reverse(arg)));
  end mssb;

	-- Index of lssb
	function lssb_idx(arg : std_logic_vector) return integer is
	begin
		return  to_integer(onehot2bin(lssb(arg)));
	end function;
	
	function lssb_idx(arg : bit_vector) return integer is
    variable  slv : std_logic_vector(arg'range);
	begin
    slv := to_stdlogicvector(arg);
		return  lssb_idx(slv);
	end lssb_idx;

	-- Index of mssb
	function mssb_idx(arg : std_logic_vector) return integer is
	begin
		return  to_integer(onehot2bin(mssb(arg)));
	end function;
	
	function mssb_idx(arg : bit_vector) return integer is
    variable  slv : std_logic_vector(arg'range);
	begin
    slv := to_stdlogicvector(arg);
		return  mssb_idx(slv);
	end mssb_idx;

	function resize(vec : bit_vector; length : natural; fill : bit := '0') return bit_vector is
    constant  high2b : natural := vec'low+length-1;
		constant  highcp : natural := imin(vec'high, high2b);
    variable  res_up : bit_vector(vec'low to high2b);
    variable  res_dn : bit_vector(high2b downto vec'low);
	begin
    if vec'ascending then
      res_up := (others => fill);
      res_up(vec'low to highcp) := vec(vec'low to highcp);
      return  res_up;
    else
      res_dn := (others => fill);
      res_dn(highcp downto vec'low) := vec(highcp downto vec'low);
      return  res_dn;
		end if;
	end resize;

	function resize(vec : std_logic_vector; length : natural; fill : std_logic := '0') return std_logic_vector is
    constant  high2b : natural := vec'low+length-1;
		constant  highcp : natural := imin(vec'high, high2b);
    variable  res_up : std_logic_vector(vec'low to high2b);
    variable  res_dn : std_logic_vector(high2b downto vec'low);
	begin
    if vec'ascending then
      res_up := (others => fill);
      res_up(vec'low to highcp) := vec(vec'low to highcp);
      return  res_up;
    else
      res_dn := (others => fill);
      res_dn(highcp downto vec'low) := vec(highcp downto vec'low);
      return  res_dn;
		end if;
	end resize;

	-- Move vector boundaries
	-- ==========================================================================
  function move(vec : std_logic_vector; ofs : integer) return std_logic_vector is
    variable res_up : std_logic_vector(vec'low +ofs to     vec'high+ofs);
    variable res_dn : std_logic_vector(vec'high+ofs downto vec'low +ofs);
  begin
    if vec'ascending then
      res_up := vec;
      return  res_up;
    else
      res_dn := vec;
      return  res_dn;
    end if;
  end move;

	function movez(vec : std_logic_vector) return std_logic_vector is
  begin
    return  move(vec, -vec'low);
  end movez;

  function ascend(vec : std_logic_vector)	return std_logic_vector is
		variable  res : std_logic_vector(vec'low to vec'high);
	begin
		res := vec;
		return  res;
	end ascend;

  function descend(vec : std_logic_vector)	return std_logic_vector is
		variable  res : std_logic_vector(vec'high downto vec'low);
	begin
		res := vec;
		return  res;
	end descend;
end package body;
