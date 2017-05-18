-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Package:					String related functions and types
--
-- Description:
-- -------------------------------------
--		For detailed documentation see below.
--
-- License:
-- =============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
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
use			IEEE.math_real.all;

library	PoC;
use			PoC.config.all;
use			PoC.utils.all;
--use			PoC.FileIO.all;


package strings is
	-- default fill and string termination character for fixed size strings
	-- ===========================================================================
	-- WORKAROUND: for Altera Quartus-II
	--	Version:	15.0
	--	Issue:
	--		character 0 (NUL) causes Quartus-II to crash, if uses to pad STRINGs
	--		characters < 32 (control characters) are not supported in Quartus-II
	--		characters > 127 are not supported in VHDL files (strict ASCII files)
	--		character 255 craches ISE log window (created by 'CHARACTER'val(255)')
	--	Solution:
	--		PoC uses backtick "`" as a fill and termination symbol, if a Quartus-II
	--		synthesis environment is detected.
	constant C_POC_NUL			: character		:= ite((SYNTHESIS_TOOL /= SYNTHESIS_TOOL_ALTERA_QUARTUS2), NUL, '`');

	-- Type declarations
	-- ===========================================================================
	subtype T_RAWCHAR				is std_logic_vector(7 downto 0);
	type		T_RAWSTRING			is array (natural range <>) of T_RAWCHAR;

	-- testing area:
	-- ===========================================================================
	function to_IPStyle(str : string)			return T_IPSTYLE;

	-- to_char
	function to_char(Value : std_logic)		return character;
	function to_char(rawchar : T_RAWCHAR) return character;

	function to_HexChar(Value : natural)	return character;
	function to_HexChar(Value : unsigned)	return character;

	-- chr_is* function
	function chr_isDigit(chr : character)					return boolean;
	function chr_isLowerHexDigit(chr : character)	return boolean;
	function chr_isUpperHexDigit(chr : character)	return boolean;
	function chr_isHexDigit(chr : character)			return boolean;
	function chr_isLower(chr : character)					return boolean;
	function chr_isLowerAlpha(chr : character)		return boolean;
	function chr_isUpper(chr : character)					return boolean;
	function chr_isUpperAlpha(chr : character)		return boolean;
	function chr_isAlpha(chr : character)					return boolean;

	-- raw_format_* functions
	function raw_format_bool_bin(Value : boolean)				return string;
	function raw_format_bool_chr(Value : boolean)				return string;
	function raw_format_bool_str(Value : boolean)				return string;
	function raw_format_slv_bin(slv : std_logic_vector)	return string;
	function raw_format_slv_oct(slv : std_logic_vector)	return string;
	function raw_format_slv_dec(slv : std_logic_vector) return string;
	function raw_format_slv_hex(slv : std_logic_vector)	return string;
	function raw_format_nat_bin(Value : natural)				return string;
	function raw_format_nat_oct(Value : natural)				return string;
	function raw_format_nat_dec(Value : natural)				return string;
	function raw_format_nat_hex(Value : natural)				return string;

	-- str_format_* functions
	function str_format(Value : REAL; precision : natural := 3) return string;

	-- to_string
	function to_string(Value : boolean) return string;
	function to_string(Value : integer; base : positive := 10) return string;
	function to_string(slv : std_logic_vector; format : character; Length : natural := 0; fill : character := '0') return string;
	function to_string(rawstring : T_RAWSTRING) return string;
	function to_string(Value : T_BCD_VECTOR) return string;

	-- to_slv
	function to_slv(rawstring : T_RAWSTRING) return std_logic_vector;

	-- digit subtypes incl. error Value (-1)
	subtype T_DIGIT_BIN	is integer range -1 to 1;
	subtype T_DIGIT_OCT	is integer range -1 to 7;
	subtype T_DIGIT_DEC	is integer range -1 to 9;
	subtype T_DIGIT_HEX	is integer range -1 to 15;

	-- to_digit*
	function to_digit_bin(chr : character) return T_DIGIT_BIN;
	function to_digit_oct(chr : character) return T_DIGIT_OCT;
	function to_digit_dec(chr : character) return T_DIGIT_DEC;
	function to_digit_hex(chr : character) return T_DIGIT_HEX;
	function to_digit(chr : character; base : character := 'd') return integer;

	-- to_natural*
	function to_natural_bin(str : string) return integer;
	function to_natural_oct(str : string) return integer;
	function to_natural_dec(str : string) return integer;
	function to_natural_hex(str : string) return integer;
	function to_natural(str : string; base : character := 'd') return integer;

	-- to_raw*
	function to_RawChar(char : character) return T_RAWCHAR;
	function to_RawString(str : string)		return T_RAWSTRING;

	-- resize
	function resize(str : string; size : positive; FillChar : character := C_POC_NUL)			return string;
--	function resize(rawstr : T_RAWSTRING; size : POSITIVE; FillChar : T_RAWCHAR := x"00")	return T_RAWSTRING;

	-- Character functions
	function chr_toLower(chr : character) return character;
	function chr_toUpper(chr : character) return character;

	-- String functions
	function str_length(str : string)										return natural;
	function str_equal(str1 : string; str2 : string)		return boolean;
	function str_match(str1 : string; str2 : string)		return boolean;
	function str_imatch(str1 : string; str2 : string)		return boolean;
	function str_pos(str : string; chr : character; start : natural := 0)		return integer;
	function str_pos(str : string; pattern : string; start : natural := 0)	return integer;
	function str_ipos(str : string; chr : character; start : natural := 0)	return integer;
	function str_ipos(str : string; pattern : string; start : natural := 0)	return integer;
	function str_find(str : string; chr : character)		return boolean;
	function str_find(str : string; pattern : string)		return boolean;
	function str_ifind(str : string; chr : character)		return boolean;
	function str_ifind(str : string; pattern : string)	return boolean;
	function str_replace(str : string; pattern : string; replace : string)	return string;
	function str_substr(str : string; start : integer := 0; Length : integer := 0) return string;
	function str_ltrim(str : string; char : character := ' ')	return string;
	function str_rtrim(str : string; char : character := ' ')	return string;
	function str_trim(str : string)														return string;
	function str_calign(str : string; Length : natural; FillChar : character := ' ') return string;
	function str_lalign(str : string; Length : natural; FillChar : character := ' ') return string;
	function str_ralign(str : string; Length : natural; FillChar : character := ' ') return string;
	function str_toLower(str : string)												return string;
	function str_toUpper(str : string)												return string;
end package;


package body strings is
	--
	function to_IPStyle(str : string) return T_IPSTYLE is
	begin
		for i in T_IPSTYLE'pos(T_IPSTYLE'low) to T_IPSTYLE'pos(T_IPSTYLE'high) loop
			if str_imatch(str, T_IPSTYLE'image(T_IPSTYLE'val(i))) then
				return T_IPSTYLE'val(i);
			end if;
		end loop;

		report "Unknown IPStyle: '" & str & "'" severity FAILURE;
		return IPSTYLE_UNKNOWN;
	end function;

	-- to_char
	-- ===========================================================================
	function to_char(Value : std_logic) return character is
	begin
		case Value is
			when 'U' =>			return 'U';
			when 'X' =>			return 'X';
			when '0' =>			return '0';
			when '1' =>			return '1';
			when 'Z' =>			return 'Z';
			when 'W' =>			return 'W';
			when 'L' =>			return 'L';
			when 'H' =>			return 'H';
			when '-' =>			return '-';
			when others =>	return 'X';
		end case;
	end function;

	function to_char(rawchar : T_RAWCHAR) return character is
	begin
		return character'val(to_integer(unsigned(rawchar)));
	end function;

	--
	function to_HexChar(Value : natural) return character is
	  constant HEX : string := "0123456789ABCDEF";
	begin
		return ite(Value < 16, HEX(Value+1), 'X');
	end function;

	function to_HexChar(Value : unsigned) return character is
	begin
		return to_HexChar(to_integer(Value));
	end function;

	-- chr_is* function
	function chr_isDigit(chr : character) return boolean is
	begin
		return (character'pos('0') <= character'pos(chr)) and (character'pos(chr) <= character'pos('9'));
	end function;

	function chr_isLowerHexDigit(chr : character) return boolean is
	begin
		return (character'pos('a') <= character'pos(chr)) and (character'pos(chr) <= character'pos('f'));
	end function;

	function chr_isUpperHexDigit(chr : character) return boolean is
	begin
		return (character'pos('A') <= character'pos(chr)) and (character'pos(chr) <= character'pos('F'));
	end function;

	function chr_isHexDigit(chr : character) return boolean is
	begin
		return chr_isDigit(chr) or chr_isLowerHexDigit(chr) or chr_isUpperHexDigit(chr);
	end function;

	function chr_isLower(chr : character) return boolean is
	begin
		return chr_isLowerAlpha(chr);
	end function;

	function chr_isLowerAlpha(chr : character) return boolean is
	begin
		return (character'pos('a') <= character'pos(chr)) and (character'pos(chr) <= character'pos('z'));
	end function;

	function chr_isUpper(chr : character) return boolean is
	begin
		return chr_isUpperAlpha(chr);
	end function;

	function chr_isUpperAlpha(chr : character) return boolean is
	begin
		return (character'pos('A') <= character'pos(chr)) and (character'pos(chr) <= character'pos('Z'));
	end function;

	function chr_isAlpha(chr : character) return boolean is
	begin
		return chr_isLowerAlpha(chr) or chr_isUpperAlpha(chr);
	end function;

	-- raw_format_* functions
	-- ===========================================================================
	function raw_format_bool_bin(Value : boolean) return string is
	begin
		return ite(Value, "1", "0");
	end function;

	function raw_format_bool_chr(Value : boolean) return string is
	begin
		return ite(Value, "T", "F");
	end function;

	function raw_format_bool_str(Value : boolean) return string is
	begin
		return str_toUpper(boolean'image(Value));
	end function;

	function raw_format_slv_bin(slv : std_logic_vector) return string is
		variable Value				: std_logic_vector(slv'length - 1 downto 0);
		variable Result				: string(1 to slv'length);
		variable j						: natural;
	begin
		-- convert input slv to a downto ranged vector and normalize range to slv'low = 0
		Value := movez(ite(slv'ascending, descend(slv), slv));
		-- convert each bit to a character
		j := 0;
		for i in Result'reverse_range loop
			Result(i)	:= to_char(Value(j));
			j					:= j + 1;
		end loop;
		return Result;
	end function;

	function raw_format_slv_oct(slv : std_logic_vector) return string is
		variable Value				: std_logic_vector(slv'length - 1 downto 0);
		variable Digit				: std_logic_vector(2 downto 0);
		variable Result				: string(1 to div_ceil(slv'length, 3));
		variable j						: natural;
	begin
		-- convert input slv to a downto ranged vector; normalize range to slv'low = 0 and resize it to a multiple of 3
		Value := resize(movez(ite(slv'ascending, descend(slv), slv)), (Result'length * 3));
		-- convert 3 bit to a character
		j := 0;
		for i in Result'reverse_range loop
			Digit			:= Value((j * 3) + 2 downto (j * 3));
			Result(i)	:= to_HexChar(unsigned(Digit));
			j					:= j + 1;
		end loop;

		return Result;
	end function;

	function raw_format_slv_dec(slv : std_logic_vector) return string is
		variable Value				: std_logic_vector(slv'length - 1 downto 0);
		variable Result				: string(1 to div_ceil(slv'length, 3));

		subtype	TT_BCD				is integer range 0 to 31;
		type		TT_BCD_VECTOR	is array(natural range <>) of TT_BCD;

		variable Temp		: TT_BCD_VECTOR(div_ceil(slv'length, 3) - 1 downto 0);
		variable Carry	: T_UINT_8;

		variable Pos		: natural;
	begin
		Temp	:= (others => 0);
		Pos		:= 0;
		-- convert input slv to a downto ranged vector
		Value := ite(slv'ascending, descend(slv), slv);

		for i in Value'range loop
			Carry		:= to_int(Value(i));
			for j in Temp'reverse_range loop
				Temp(j)	:= Temp(j) * 2 + Carry;
				Carry		:=						to_int(Temp(j) > 9);
				Temp(j)	:= Temp(j) - to_int((Temp(j) > 9), 0, 10);
			end loop;
		end loop;

		for i in Result'range loop
			Result(i)		:= to_HexChar(Temp(Temp'high - i + 1));
			if ((Result(i) /= '0') and (Pos = 0)) then
				Pos	:= i;
			end if;
		end loop;
		-- trim leading zeros, except the last
		return Result(imin(Pos, Result'high) to Result'high);
	end function;

	function raw_format_slv_hex(slv : std_logic_vector) return string is
		variable Value				: std_logic_vector(4*div_ceil(slv'length, 4) - 1 downto 0);
		variable Digit				: std_logic_vector(3 downto 0);
		variable Result				: string(1 to div_ceil(slv'length, 4));
		variable j						: natural;
	begin
		Value := resize(slv, Value'length);
		j			:= 0;
		for i in Result'reverse_range loop
			Digit			:= Value((j * 4) + 3 downto (j * 4));
			Result(i)	:= to_HexChar(unsigned(Digit));
			j					:= j + 1;
		end loop;
		return Result;
	end function;

	function raw_format_nat_bin(Value : natural) return string is
	begin
		return raw_format_slv_bin(to_slv(Value, log2ceilnz(Value+1)));
	end function;

	function raw_format_nat_oct(Value : natural) return string is
	begin
		return raw_format_slv_oct(to_slv(Value, log2ceilnz(Value+1)));
	end function;

	function raw_format_nat_dec(Value : natural) return string is
	begin
		return integer'image(Value);
	end function;

	function raw_format_nat_hex(Value : natural) return string is
	begin
		return raw_format_slv_hex(to_slv(Value, log2ceilnz(Value+1)));
	end function;

	-- str_format_* functions
	-- ===========================================================================
	function str_format(Value : REAL; precision : natural := 3) return string is
		constant s				: REAL		:= sign(Value);
		constant val			: REAL		:= Value * s;
		constant int			: integer	:= integer(floor(val));
		constant frac			: integer	:= integer(round((val - real(int)) * 10.0**precision));
		constant overflow : boolean := frac >= 10**precision;
		constant int2     : integer := ite(overflow, int+1, int);
		constant frac2    : integer := ite(overflow, frac-10**precision, frac);
		constant frac_str	: string	:= integer'image(frac2);
		constant res			: string	:= integer'image(int2) & "." & (2 to (precision - frac_str'length + 1) => '0') & frac_str;
	begin
		return ite ((s < 0.0), "-" & res, res);
	end function;

	-- to_string
	-- ===========================================================================
	function to_string(Value : boolean) return string is
	begin
		return raw_format_bool_str(Value);
	end function;

	-- convert an integer Value to a STRING using an arbitrary base
	function to_string(Value : integer; base : positive := 10) return string is
		constant absValue		: natural								:= abs Value;
		constant len		 		: positive							:= log10ceilnz(absValue);
		variable power			: positive;
		variable Result			: string(1 to len);
	begin
		power		:= 1;

		if base = 10 then
			return integer'image(Value);
		else
			for i in len downto 1 loop
				Result(i)		:= to_HexChar(absValue / power mod base);
				power				:= power * base;
			end loop;

			if Value < 0 then
				return '-' & Result;
			else
				return Result;
			end if;
		end if;
	end function;

	-- QUESTION: rename to slv_format(..) ?
	function to_string(slv : std_logic_vector; format : character; Length : natural := 0; fill : character := '0') return string is
		constant int					: integer				:= ite((slv'length <= 31), to_integer(unsigned(resize(slv, 31))), 0);
		constant str					: string				:= integer'image(int);
		constant bin_len			: positive			:= slv'length;
		constant dec_len			: positive			:= str'length;--log10ceilnz(int);
		constant hex_len			: positive			:= ite(((bin_len mod 4) = 0), (bin_len / 4), (bin_len / 4) + 1);
		constant len					: natural				:= ite((format = 'b'), bin_len,
																						 ite((format = 'd'), dec_len,
																						 ite((format = 'h'), hex_len, 0)));
		variable j						: natural;
		variable Result				: string(1 to ite((Length = 0), len, imax(len, Length)));
	begin
		j				:= 0;
		Result	:= (others => fill);

		if (format = 'b') then
			for i in Result'reverse_range loop
				Result(i)		:= to_char(slv(j));
				j						:= j + 1;
			end loop;
		elsif (format = 'd') then
			-- TODO: enable big integer conversion
--			if (slv'length < 32) then
--				return INTEGER'image(int);
--			else
--				return raw_format_slv_dec(slv);
--			end if;
			Result(Result'length - str'length + 1 to Result'high)	:= str;
		elsif (format = 'h') then
			for i in Result'reverse_range loop
				Result(i)		:= to_HexChar(unsigned(slv((j * 4) + 3 downto (j * 4))));
				j						:= j + 1;
			end loop;
		else
			report "Unknown format character: " & format & "." severity FAILURE;
		end if;

		return Result;
	end function;

	function to_string(rawstring : T_RAWSTRING) return string is
		variable Result		: string(1 to rawstring'length);
	begin
		for i in rawstring'low to rawstring'high loop
			Result(i - rawstring'low + 1)	:= to_char(rawstring(i));
		end loop;
		return Result;
	end function;

	function to_string(Value : T_BCD_VECTOR) return string is
		variable Result		: string(1 to Value'length);
	begin
		for i in Value'range loop
			Result(Result'high - (i - Value'low))	:= to_HexChar(unsigned(Value(i)));
		end loop;
		return Result;
	end function;

	-- to_slv
	-- ===========================================================================
	function to_slv(rawstring : T_RAWSTRING) return std_logic_vector is
		variable Result : std_logic_vector((rawstring'length * 8) - 1 downto 0);
	begin
		for i in rawstring'range loop
			Result(((i - rawstring'low) * 8) + 7 downto (i - rawstring'low) * 8)	:= rawstring(i);
		end loop;
		return Result;
	end function;

	-- to_digit*
	-- ===========================================================================
	-- convert a binary digit given as CHARACTER to a digit returned as NATURAL; return -1 on error
	function to_digit_bin(chr : character) return T_DIGIT_BIN is
	begin
		case chr is
			when '0' =>			return 0;
			when '1' =>			return 1;
			when others =>	return -1;
		end case;
	end function;

	-- convert an octal digit given as CHARACTER to a digit returned as NATURAL; return -1 on error
	function to_digit_oct(chr : character) return T_DIGIT_OCT is
		variable dec : integer;
	begin
		dec := to_digit_dec(chr);
		return ite((dec < 8), dec, -1);
	end function;

	-- convert a adecimal digit given as CHARACTER to a digit returned as NATURAL; return -1 on error
	function to_digit_dec(chr : character) return T_DIGIT_DEC is
	begin
		if chr_isDigit(chr) then
			return character'pos(chr) - CHARACTER'pos('0');
		else
			return -1;
		end if;
	end function;

	-- convert a hexadecimal digit given as CHARACTER to a digit returned as NATURAL; return -1 on error
	function to_digit_hex(chr : character) return T_DIGIT_HEX is
	begin
		if chr_isDigit(chr) then						return character'pos(chr) - CHARACTER'pos('0');
		elsif chr_isLowerHexDigit(chr) then	return character'pos(chr) - CHARACTER'pos('a') + 10;
		elsif chr_isUpperHexDigit(chr) then	return character'pos(chr) - CHARACTER'pos('A') + 10;
		else																return -1;
		end if;
	end function;

	-- convert a digit given as CHARACTER to a digit returned as NATURAL; return -1 on error
	function to_digit(chr : character; base : character := 'd') return integer is
	begin
		case base is
			when 'b' =>			return to_digit_bin(chr);
			when 'o' =>			return to_digit_oct(chr);
			when 'd' =>			return to_digit_dec(chr);
			when 'h' =>			return to_digit_hex(chr);
			when others =>	report "Unknown base character: " & base & "." severity FAILURE;
											return -1;
		end case;
	end function;

	-- to_natural*
	-- ===========================================================================
	-- convert a binary number given as STRING to a NATURAL; return -1 on error
	function to_natural_bin(str : string) return integer is
		variable Result			: natural;
		variable Digit			: integer;
	begin
		for i in str'range loop
			Digit	:= to_digit_bin(str(i));
			if Digit /= -1 then
				Result	:= Result * 2 + Digit;
			else
				return -1;
			end if;
		end loop;
		return Result;
	end function;

	-- convert an octal number given as STRING to a NATURAL; return -1 on error
	function to_natural_oct(str : string) return integer is
		variable Result			: natural;
		variable Digit			: integer;
	begin
		for i in str'range loop
			Digit	:= to_digit_oct(str(i));
			if Digit /= -1 then
				Result	:= Result * 8 + Digit;
			else
				return -1;
			end if;
		end loop;
		return Result;
	end function;

	-- convert a decimal number given as STRING to a NATURAL; return -1 on error
	function to_natural_dec(str : string) return integer is
		variable Result			: natural;
		variable Digit			: integer;
	begin
		-- WORKAROUND: Xilinx Vivado Synth
		--	Version:	2014.1
		--	Issue:
		--		INTEGER'value(...) is not supported by Vivado Synth
		--	Solution:
		--		implement a manual conversion using shift and multiply
		for i in str'range loop
			Digit	:= to_digit_dec(str(i));
			if Digit /= -1 then
				Result	:= Result * 10 + Digit;
			else
				return -1;
			end if;
		end loop;
		return Result;	-- INTEGER'value(str);
	end function;

	-- convert a hexadecimal number given as STRING to a NATURAL; return -1 on error
	function to_natural_hex(str : string) return integer is
		variable Result			: natural;
		variable Digit			: integer;
	begin
		for i in str'range loop
			Digit	:= to_digit_hex(str(i));
			if Digit /= -1 then
				Result	:= Result * 16 + Digit;
			else
				return -1;
			end if;
		end loop;
		return Result;
	end function;

	-- convert a number given as STRING to a NATURAL; return -1 on error
	function to_natural(str : string; base : character := 'd') return integer is
	begin
		case base is
			when 'b' =>			return to_natural_bin(str);
			when 'o' =>			return to_natural_oct(str);
			when 'd' =>			return to_natural_dec(str);
			when 'h' =>			return to_natural_hex(str);
			when others =>	report "Unknown base character: " & base & "." severity FAILURE;
											return -1;
		end case;
	end function;

	-- to_raw*
	-- ===========================================================================
	-- convert a CHARACTER to a RAWCHAR
	function to_RawChar(char : character) return T_RAWCHAR is
	begin
		return std_logic_vector(to_unsigned(character'pos(char), T_RAWCHAR'length));
	end function;

	-- convert a STRING to a RAWSTRING
	function to_RawString(str : string) return T_RAWSTRING is
		variable Result			: T_RAWSTRING(0 to str'length - 1);
	begin
		for i in str'low to str'high loop
			Result(i - str'low)	:= to_RawChar(str(i));
		end loop;
		return Result;
	end function;

	-- resize
	-- ===========================================================================
	function resize(str : string; Size : positive; FillChar : character := C_POC_NUL) return string is
		constant ConstNUL		: string(1 to 1)				:= (others => C_POC_NUL);
		variable Result			: string(1 to Size);
	begin
		Result := (others => FillChar);
		if (str'length > 0) then
			-- WORKAROUND: for Altera Quartus-II
			--	Version:	15.0
			--	Issue:		array bounds are check regardless of the hierarchy and control flow
			Result(1 to bound(Size, 1, str'length)) := ite((str'length > 0), str(1 to imin(Size, str'length)), ConstNUL);
		end if;
		return Result;
	end function;

--	function resize(str : T_RAWSTRING; size : POSITIVE; FillChar : T_RAWCHAR := x"00") return T_RAWSTRING is
--		constant ConstNUL		: T_RAWSTRING(1 to 1)				:= (others => x"00");
--		variable Result			: T_RAWSTRING(1 to size);
--		function ifthenelse(cond : BOOLEAN; value1 : T_RAWSTRING; value2 : T_RAWSTRING) return T_RAWSTRING is
--		begin
--			if cond then
--				return value1;
--			else
--				return value2;
--			end if;
--		end function;
--	begin
--		Result := (others => FillChar);
--		if (str'length > 0) then
--			Result(1 to imin(size, imax(1, str'length))) := ifthenelse((str'length > 0), str(1 to imin(size, str'length)), ConstNUL);
--		end if;
--		return Result;
--	end function;


	-- Character functions
	-- ===========================================================================
	-- convert an upper case CHARACTER into a lower case CHARACTER
	function chr_toLower(chr : character) return character is
	begin
		if chr_isUpperAlpha(chr) then
			return character'val(character'pos(chr) - character'pos('A') + character'pos('a'));
		else
			return chr;
		end if;
	end function;

	-- convert a lower case CHARACTER into an upper case CHARACTER
	function chr_toUpper(chr : character) return character is
	begin
		if chr_isLowerAlpha(chr) then
			return character'val(character'pos(chr) - character'pos('a') + character'pos('A'));
		else
			return chr;
		end if;
	end function;

	-- String functions
	-- ===========================================================================
	-- count the length of a POC_NUL terminated STRING
	function str_length(str : string) return natural is
	begin
		for i in str'range loop
			if str(i) = C_POC_NUL then
				return i - str'low;
			end if;
		end loop;
		return str'length;
	end function;

	-- compare two STRINGs for equality
	-- pre-check the string lengthes to suppress warnings for unqual sized string comparisons.
	-- QUESTION: overload "=" operator?
	function str_equal(str1 : string; str2 : string) return boolean is
	begin
		if str1'length /= str2'length then
			return FALSE;
		else
			return (str1 = str2);
		end if;
	end function;

	-- compare two POC_NUL terminated STRINGs
	function str_match(str1 : string; str2 : string) return boolean is
		constant len	: natural 		:= imin(str1'length, str2'length);
	begin
		-- if both strings are empty
		if ((str1'length = 0 ) and (str2'length = 0)) then		return TRUE;	end if;
		-- compare char by char
		for i in str1'low to str1'low + len - 1 loop
			if (str1(i) /= str2(str2'low + (i - str1'low))) then
				return FALSE;
			elsif ((str1(i) = C_POC_NUL) xor (str2(str2'low + (i - str1'low)) = C_POC_NUL)) then
				return FALSE;
			elsif ((str1(i) = C_POC_NUL) and (str2(str2'low + (i - str1'low)) = C_POC_NUL)) then
				return TRUE;
			end if;
		end loop;
		-- check special cases,
		return (((str1'length = len) and (str2'length = len)) or									-- both strings are fully consumed and equal
						((str1'length > len) and (str1(str1'low + len) = C_POC_NUL)) or		-- str1 is longer, but str_length equals len
						((str2'length > len) and (str2(str2'low + len) = C_POC_NUL)));		-- str2 is longer, but str_length equals len
	end function;

	-- compare two POC_NUL terminated STRINGs; case insentitve
	function str_imatch(str1 : string; str2 : string) return boolean is
	begin
		return str_match(str_toLower(str1), str_toLower(str2));
	end function;

	-- search for chr in a STRING and return the position; return -1 on error
	function str_pos(str : string; chr : character; start : natural := 0) return integer is
	begin
		for i in imax(str'low, start) to str'high loop
			exit when (str(i) = C_POC_NUL);
			if str(i) = chr then
				return i;
			end if;
		end loop;
		return -1;
	end function;

	-- search for pattern in a STRING and return the position; return -1 on error
	-- QUESTION: implement KMP algorithm?
	function str_pos(str : string; pattern : string; start : natural := 0) return integer is
	begin
		for i in imax(str'low, start) to (str'high - pattern'length + 1) loop
			exit when (str(i) = C_POC_NUL);
			if (str(i to i + pattern'length - 1) = pattern) then
				return i;
			end if;
		end loop;
		return -1;
	end function;

	-- search for chr in a STRING and return the position; case insentitve; return -1 on error
	function str_ipos(str : string; chr : character; start : natural := 0) return integer is
	begin
		return str_pos(str_toLower(str), chr_toLower(chr));
	end function;

	-- search for pattern in a STRING and return the position; case insentitve; return -1 on error
	function str_ipos(str : string; pattern : string; start : natural := 0) return integer is
	begin
		return str_pos(str_toLower(str), str_toLower(pattern));
	end function;

--	function str_pos(str1 : STRING; str2 : STRING) return INTEGER is
--		variable PrefixTable	: T_INTVEC(0 to str2'length);
--		variable j						: INTEGER;
--	begin
--		-- construct prefix table for KMP algorithm
--		j								:= -1;
--		PrefixTable(0)	:= -1;
--		for i in str2'range loop
--			while ((j >= 0) and str2(j + 1) /= str2(i)) loop
--				j		:= PrefixTable(j);
--			end loop;
--
--			j										:= j + 1;
--			PrefixTable(i - 1)	:= j + 1;
--		end loop;
--
--		-- search pattern str2 in text str1
--		j := 0;
--		for i in str1'range loop
--			while ((j >= 0) and str1(i) /= str2(j + 1)) loop
--				j		:= PrefixTable(j);
--			end loop;
--
--			j := j + 1;
--			if ((j + 1) = str2'high) then
--				return i - str2'length + 1;
--			end if;
--		end loop;
--
--		return -1;
--	end function;

	-- check if chr exists in STRING str
	function str_find(str : string; chr : character) return boolean is
	begin
		return (str_pos(str, chr) > 0);
	end function;

	-- check if pattern exists in STRING str
	function str_find(str : string; pattern : string) return boolean is
	begin
		return (str_pos(str, pattern) > 0);
	end function;

	-- check if chr exists in STRING str; case insentitve
	function str_ifind(str : string; chr : character) return boolean is
	begin
		return (str_ipos(str, chr) > 0);
	end function;

	-- check if pattern exists in STRING str; case insentitve
	function str_ifind(str : string; pattern : string) return boolean is
	begin
		return (str_ipos(str, pattern) > 0);
	end function;

	-- replace a pattern in a STRING str by the STRING replace
	function str_replace(str : string; pattern : string; replace : string) return string is
		variable pos		: integer;
	begin
		pos := str_pos(str, pattern);
		if pos > 0 then
			if pos = 1 then
				return replace & str(pattern'length + 1 to str'length);
			elsif (pos = str'length - pattern'length + 1) then
				return str(1 to str'length - pattern'length) & replace;
			else
				return str(1 to pos - 1) & replace & str(pos + pattern'length to str'length);
			end if;
		else
			return str;
		end if;
	end function;

	-- return a sub-string of STRING str
	-- EXAMPLES:
	--							  123456789ABC
	-- input string: "Hello World."
	--	low=1; high=12; length=12
	--
	--	str_substr("Hello World.",	0,	0)	=> "Hello World."		- copy all
	--	str_substr("Hello World.",	7,	0)	=> "World."					- copy from pos 7 to end of string
	--	str_substr("Hello World.",	7,	5)	=> "World"					- copy from pos 7 for 5 characters
	--	str_substr("Hello World.",	0, -7)	=> "Hello World."		- copy all until character 8 from right boundary
	function str_substr(str : string; start : integer := 0; Length : integer := 0) return string is
		variable StartOfString		: positive;
		variable EndOfString			: positive;
	begin
		if start < 0 then			-- start is negative -> start substring at right string boundary
			StartOfString		:= str'high + start + 1;
		elsif start = 0 then	-- start is zero -> start substring at left string boundary
			StartOfString		:= str'low;
		else 										-- start is positive -> start substring at left string boundary + offset
			StartOfString		:= start;
		end if;

		if Length < 0 then		-- Length is negative -> end substring at length'th character before right string boundary
			EndOfString			:= str'high + Length;
		elsif Length = 0 then	-- Length is zero -> end substring at right string boundary
			EndOfString			:= str'high;
		else										-- Length is positive -> end substring at StartOfString + Length
			EndOfString			:= StartOfString + Length - 1;
		end if;

		if (StartOfString < str'low) then			report "StartOfString is out of str's range. (str=" & str & ")"	severity FAILURE;		end if;
		if (EndOfString < str'high) then			report "EndOfString is out of str's range. (str=" & str & ")"		severity FAILURE;		end if;

		return str(StartOfString to EndOfString);
	end function;

	-- left-trim the STRING str
	function str_ltrim(str : string; char : character := ' ') return string is
	begin
		for i in str'range loop
			if str(i) /= char then
				return str(i to str'high);
			end if;
		end loop;
		return "";
	end function;

	-- right-trim the STRING str
	function str_rtrim(str : string; char : character := ' ') return string is
	begin
		for i in str'reverse_range loop
			if str(i) /= char then
				return str(str'low to i);
			end if;
		end loop;
		return "";
	end function;

	-- remove POC_NUL string termination characters
	function str_trim(str : string) return string is
	begin
		return str(str'low to str'low + str_length(str) - 1);
	end function;

	-- center-align a STRING str in a FillChar filled STRING of length Length
	function str_calign(str : string; Length : natural; FillChar : character := ' ') return string is
		constant Start		: positive	:= (Length - str'length) / 2;
		variable Result		: string(1 to Length);
	begin
		Result		:= (others => FillChar);
		Result(Start to (Start + str'length))	:= str;
		return Result;
	end function;

	-- left-align a STRING str in a FillChar filled STRING of length Length
	function str_lalign(str : string; Length : natural; FillChar : character := ' ') return string is
		variable Result		: string(1 to Length);
	begin
		Result		:= (others => FillChar);
		Result(1 to str'length)	:= str;
		return Result;
	end function;

	-- right-align a STRING str in a FillChar filled STRING of length Length
	function str_ralign(str : string; Length : natural; FillChar : character := ' ') return string is
		variable Result		: string(1 to Length);
	begin
		Result		:= (others => FillChar);
		Result((Length - str'length + 1) to Length)	:= str;
		return Result;
	end function;

	-- convert an upper case STRING into a lower case STRING
	function str_toLower(str : string) return string is
		variable Result		: string(str'range);
	begin
		for i in str'range loop
			Result(i)	:= chr_toLower(str(i));
		end loop;
		return Result;
	end function;

	-- convert a lower case STRING into an upper case STRING
	function str_toUpper(str : string) return string is
		variable Result		: string(str'range);
	begin
		for i in str'range loop
			Result(i)	:= chr_toUpper(str(i));
		end loop;
		return Result;
	end function;

end package body;
