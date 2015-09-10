-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Package:					String related functions and types
--
-- Description:
-- ------------------------------------
--		For detailed documentation see below.
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
	constant C_POC_NUL			: CHARACTER		:= ite((SYNTHESIS_TOOL /= SYNTHESIS_TOOL_ALTERA_QUARTUS2), NUL, '`');
	-- character 0 causes Quartus to crash, if uses to pad STRINGs
	-- characters < 32 (control characters) are not supported in Quartus
	-- characters > 127 are not supported in VHDL files (strict ASCII files)
	-- character 255 craches ISE log window (created by 'CHARACTER'val(255)')

	-- Type declarations
	-- ===========================================================================
	subtype T_RAWCHAR				is STD_LOGIC_VECTOR(7 downto 0);
	type		T_RAWSTRING			is array (NATURAL range <>) of T_RAWCHAR;
	
	-- testing area:
	-- ===========================================================================
	function to_IPStyle(str : STRING)			return T_IPSTYLE;

	-- to_char
	function to_char(value : STD_LOGIC)		return CHARACTER;
	function to_char(value : NATURAL)			return CHARACTER;
	function to_char(rawchar : T_RAWCHAR) return CHARACTER;	

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
	function raw_format_bool_bin(value : BOOLEAN)				return STRING;
	function raw_format_bool_chr(value : BOOLEAN)				return STRING;
	function raw_format_bool_str(value : BOOLEAN)				return STRING;
	function raw_format_slv_bin(slv : STD_LOGIC_VECTOR)	return STRING;
	function raw_format_slv_oct(slv : STD_LOGIC_VECTOR)	return STRING;
	function raw_format_slv_dec(slv : STD_LOGIC_VECTOR) return STRING;
	function raw_format_slv_hex(slv : STD_LOGIC_VECTOR)	return STRING;
	function raw_format_nat_bin(value : NATURAL)				return STRING;
	function raw_format_nat_oct(value : NATURAL)				return STRING;
	function raw_format_nat_dec(value : NATURAL)				return STRING;
	function raw_format_nat_hex(value : NATURAL)				return STRING;
	
	-- str_format_* functions
	function str_format(value : REAL; precision : NATURAL := 3) return STRING;
	
	-- to_string
	function to_string(value : BOOLEAN) return STRING;	
	function to_string(value : INTEGER; base : POSITIVE := 10) return STRING;
	function to_string(slv : STD_LOGIC_VECTOR; format : CHARACTER; length : NATURAL := 0; fill : CHARACTER := '0') return STRING;
	function to_string(rawstring : T_RAWSTRING) return STRING;

	-- to_slv
	function to_slv(rawstring : T_RAWSTRING) return STD_LOGIC_VECTOR;

	-- to_digit*
	function to_digit_bin(chr : character) return integer;
	function to_digit_oct(chr : character) return integer;
	function to_digit_dec(chr : character) return integer;
	function to_digit_hex(chr : character) return integer;
	function to_digit(chr : character; base : character := 'd') return integer;

	-- to_natural*
	function to_natural_bin(str : STRING) return INTEGER;
	function to_natural_oct(str : STRING) return INTEGER;
	function to_natural_dec(str : STRING) return INTEGER;
	function to_natural_hex(str : STRING) return INTEGER;
	function to_natural(str : STRING; base : CHARACTER := 'd') return INTEGER;
	
	-- to_raw*
	function to_RawChar(char : character) return T_RAWCHAR;
	function to_RawString(str : string)		return T_RAWSTRING;
	
	-- resize
	function resize(str : STRING; size : POSITIVE; FillChar : CHARACTER := C_POC_NUL)			return STRING;
--	function resize(rawstr : T_RAWSTRING; size : POSITIVE; FillChar : T_RAWCHAR := x"00")	return T_RAWSTRING;

	-- Character functions
	function chr_toLower(chr : character) return character;
	function chr_toUpper(chr : character) return character;
	
	-- String functions
	function str_length(str : STRING)										return NATURAL;
	function str_equal(str1 : STRING; str2 : STRING)		return BOOLEAN;
	function str_match(str1 : STRING; str2 : STRING)		return BOOLEAN;
	function str_imatch(str1 : STRING; str2 : STRING)		return BOOLEAN;
	function str_pos(str : STRING; chr : CHARACTER; start : NATURAL := 0)		return INTEGER;
	function str_pos(str : STRING; pattern : STRING; start : NATURAL := 0)	return INTEGER;
	function str_ipos(str : STRING; chr : CHARACTER; start : NATURAL := 0)	return INTEGER;
	function str_ipos(str : STRING; pattern : STRING; start : NATURAL := 0)	return INTEGER;
	function str_find(str : STRING; chr : CHARACTER)		return BOOLEAN;
	function str_find(str : STRING; pattern : STRING)		return BOOLEAN;
	function str_ifind(str : STRING; chr : CHARACTER)		return BOOLEAN;
	function str_ifind(str : STRING; pattern : STRING)	return BOOLEAN;
	function str_replace(str : STRING; pattern : STRING; replace : STRING)	return STRING;
	function str_substr(str : STRING; start : INTEGER := 0; length : INTEGER := 0) return STRING;
	function str_ltrim(str : STRING; char : CHARACTER := ' ')	return STRING;
	function str_rtrim(str : STRING; char : CHARACTER := ' ')	return STRING;
	function str_trim(str : STRING)														return STRING;
	function str_toLower(str : STRING)												return STRING;
	function str_toUpper(str : STRING)												return STRING;

end package;


package body strings is

	-- 
	function to_IPStyle(str : STRING) return T_IPSTYLE is
	begin
		for i in T_IPSTYLE'pos(T_IPSTYLE'low) to T_IPSTYLE'pos(T_IPSTYLE'high) loop
			if str_imatch(str, T_IPSTYLE'image(T_IPSTYLE'val(I))) then
				return T_IPSTYLE'val(i);
			end if;
		end loop;
		
		report "Unknown IPStyle: '" & str & "'" severity FAILURE;
	end function;

	-- to_char
	-- ===========================================================================
	function to_char(value : STD_LOGIC) return CHARACTER is
	begin
		case value IS
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

	-- TODO: rename to to_HexDigit(..) ?
	function to_char(value : natural) return character is
	  constant  HEX : string := "0123456789ABCDEF";
	begin
		return  ite(value < 16, HEX(value+1), 'X');
	end function;

	function to_char(rawchar : T_RAWCHAR) return CHARACTER is
	begin
		return CHARACTER'val(to_integer(unsigned(rawchar)));
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
	function raw_format_bool_bin(value : BOOLEAN) return STRING is
	begin
		return ite(value, "1", "0");
	end function;
	
	function raw_format_bool_chr(value : BOOLEAN) return STRING is
	begin
		return ite(value, "T", "F");
	end function;
	
	function raw_format_bool_str(value : BOOLEAN) return STRING is
	begin
		return str_toUpper(boolean'image(value));
	end function;
	
	function raw_format_slv_bin(slv : STD_LOGIC_VECTOR) return STRING is
		variable Value				: STD_LOGIC_VECTOR(slv'length - 1 downto 0);
		variable Result				: STRING(1 to slv'length);
		variable j						: NATURAL;
	begin
		-- convert input slv to a downto ranged vector and normalize range to slv'low = 0
		Value := movez(ite(slv'ascending, descend(slv), slv));
		
		-- convert each bit to a character
		J				:= 0;
		for i in Result'reverse_range loop
			Result(i)	:= to_char(Value(j));
			j					:= j + 1;
		end loop;
		
		return Result;
	end function;
	
	function raw_format_slv_oct(slv : STD_LOGIC_VECTOR) return STRING is
		variable Value				: STD_LOGIC_VECTOR(slv'length - 1 downto 0);
		variable Digit				: STD_LOGIC_VECTOR(2 downto 0);
		variable Result				: STRING(1 to div_ceil(slv'length, 3));
		variable j						: NATURAL;
	begin
		-- convert input slv to a downto ranged vector; normalize range to slv'low = 0 and resize it to a multiple of 3
		Value := resize(movez(ite(slv'ascending, descend(slv), slv)), (Result'length * 3));
		
		-- convert 3 bit to a character
		j				:= 0;
		for i in Result'reverse_range loop
			Digit			:= Value((j * 3) + 2 downto (j * 3));
			Result(i)	:= to_char(to_integer(unsigned(Digit)));
			j					:= j + 1;
		end loop;
		
		return Result;
	end function;
	
	function raw_format_slv_dec(slv : STD_LOGIC_VECTOR) return STRING is
		variable Value				: STD_LOGIC_VECTOR(slv'length - 1 downto 0);
		variable Result				: STRING(1 to div_ceil(slv'length, 3));
		
		subtype TT_BCD			is INTEGER range 0 to 31;
		type TT_BCD_VECTOR	is array(natural range <>) of TT_BCD;
		
		variable Temp		: TT_BCD_VECTOR(div_ceil(slv'length, 3) - 1 downto 0);
		variable Carry	: T_UINT_8;
		
		variable Pos		: NATURAL;
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
			Result(i)		:= to_char(Temp(Temp'high - i + 1));
			if ((Result(i) /= '0') and (Pos = 0)) then
				Pos	:= i;
			end if;
		end loop;
	
		-- trim leading zeros, except the last
		return Result(imin(Pos, Result'high) to Result'high);
	end function;
	
	function raw_format_slv_hex(slv : STD_LOGIC_VECTOR) return STRING is
		variable Value				: STD_LOGIC_VECTOR(4*div_ceil(slv'length, 4) - 1 downto 0);
		variable Digit				: STD_LOGIC_VECTOR(3 downto 0);
		variable Result				: STRING(1 to div_ceil(slv'length, 4));
		variable j						: NATURAL;
	begin
		Value := resize(slv, Value'length);
		j			:= 0;
		for i in Result'reverse_range loop
			Digit			:= Value((j * 4) + 3 downto (j * 4));
			Result(i)	:= to_char(to_integer(unsigned(Digit)));
			j					:= j + 1;
		end loop;
		
		return Result;
	end function;

	function raw_format_nat_bin(value : NATURAL) return STRING is
	begin
		return raw_format_slv_bin(to_slv(value, log2ceilnz(value+1)));
	end function;
	
	function raw_format_nat_oct(value : NATURAL) return STRING is
	begin
		return raw_format_slv_oct(to_slv(value, log2ceilnz(value+1)));
	end function;
	
	function raw_format_nat_dec(value : NATURAL) return STRING is
	begin
		return INTEGER'image(value);
	end function;
	
	function raw_format_nat_hex(value : NATURAL) return STRING is
	begin
		return raw_format_slv_hex(to_slv(value, log2ceilnz(value+1)));
	end function;
	
	-- str_format_* functions
	-- ===========================================================================
	function str_format(value : REAL; precision : NATURAL := 3) return STRING is
		constant s				: REAL		:= sign(value);
		constant val			: REAL		:= value * s;
		constant int			: INTEGER	:= integer(floor(val));
		constant frac			: INTEGER	:= integer(round((val - real(int)) * 10.0**precision));
		constant frac_str	: STRING	:= INTEGER'image(frac);
		constant res			: STRING	:= INTEGER'image(int) & "." & (1 to (precision - frac_str'length) => '0') & frac_str;
	begin
		return ite ((s < 0.0), "-" & res, res);
	end function;
	
	-- to_string
	-- ===========================================================================
	function to_string(value : boolean) return string is
	begin
		return raw_format_bool_str(value);
	end function;

	function to_string(value : INTEGER; base : POSITIVE := 10) return STRING is
		constant absValue		: NATURAL								:= abs(value);
		constant len		 		: POSITIVE							:= log10ceilnz(absValue);
		variable power			: POSITIVE;
		variable Result			: STRING(1 TO len);

	begin
		power		:= 1;
	
		if (base = 10) then
			return INTEGER'image(value);
		else
			for i in len downto 1 loop
				Result(i)		:= to_char(absValue / power MOD base);
				power				:= power * base;
			end loop;

			if (value < 0) then
				return '-' & Result;
			else
				return Result;
			end if;
		end if;
	end function;

	-- TODO: rename to slv_format(..) ?
	function to_string(slv : STD_LOGIC_VECTOR; format : CHARACTER; length : NATURAL := 0; fill : CHARACTER := '0') return STRING is
		constant int					: INTEGER				:= ite((slv'length <= 31), to_integer(unsigned(resize(slv, 31))), 0);
		constant str					: STRING				:= INTEGER'image(int);
		constant bin_len			: POSITIVE			:= slv'length;
		constant dec_len			: POSITIVE			:= str'length;--log10ceilnz(int);
		constant hex_len			: POSITIVE			:= ite(((bin_len MOD 4) = 0), (bin_len / 4), (bin_len / 4) + 1);
		constant len					: NATURAL				:= ite((format = 'b'), bin_len,
																						 ite((format = 'd'), dec_len,
																						 ite((format = 'h'), hex_len, 0)));
		variable j						: NATURAL;
		variable Result				: STRING(1 to ite((length = 0), len, imax(len, length)));
	begin
		j				:= 0;
		Result	:= (others => fill);
	
		if (format = 'b') then
			for i in Result'reverse_range loop
				Result(i)		:= to_char(slv(j));
				j						:= j + 1;
			end loop;
		elsif (format = 'd') then
--			if (slv'length < 32) then
--				return INTEGER'image(int);
--			else
--				return raw_format_slv_dec(slv);
--			end if;
			Result(Result'length - str'length + 1 to Result'high)	:= str;
		elsif (format = 'h') then
			for i in Result'reverse_range loop
				Result(i)		:= to_char(to_integer(unsigned(slv((j * 4) + 3 downto (j * 4)))));
				j						:= j + 1;
			end loop;
		else
			report "unknown format" severity FAILURE;
		end if;
		
		return Result;
	end function;

	function to_string(rawstring : T_RAWSTRING) return STRING is
		variable str		: STRING(1 to rawstring'length);
	begin
		for i in rawstring'low to rawstring'high loop
			str(I - rawstring'low + 1)	:= to_char(rawstring(I));
		end loop;
	
		return str;
	end function;

	-- to_slv
	-- ===========================================================================
	function to_slv(rawstring : T_RAWSTRING) return STD_LOGIC_VECTOR is
		variable result : STD_LOGIC_VECTOR((rawstring'length * 8) - 1 downto 0);
	begin
		for i in rawstring'range loop
			result(((i - rawstring'low) * 8) + 7 downto (i - rawstring'low) * 8)	:= rawstring(i);
		end loop;
		return result;
	end function;
	
	-- to_*
	-- ===========================================================================
	function to_digit_bin(chr : character) return integer is
	begin
		case chr is
			when '0' =>			return 0;
			when '1' =>			return 1;
			when others =>	return -1;
		end case;
	end function;
	
	function to_digit_oct(chr : character) return integer is
		variable dec : integer;
	begin
		dec := to_digit_dec(chr);
		return ite((dec < 8), dec, -1);
	end function;
	
	function to_digit_dec(chr : character) return integer is
	begin
		if chr_isDigit(chr) then
			return character'pos(chr) - character'pos('0');
		else
			return -1;
		end if;
	end function;
	
	function to_digit_hex(chr : character) return integer is
	begin
		if chr_isDigit(chr) then						return character'pos(chr) - character'pos('0');
		elsif chr_isLowerHexDigit(chr) then	return character'pos(chr) - character'pos('a') + 10;
		elsif chr_isUpperHexDigit(chr) then	return character'pos(chr) - character'pos('A') + 10;
		else																return -1;
		end if;
	end function;
	
	function to_digit(chr : character; base : character := 'd') return integer is
	begin
		case base is
			when 'b' =>			return to_digit_bin(chr);
			when 'o' =>			return to_digit_oct(chr);
			when 'd' =>			return to_digit_dec(chr);
			when 'h' =>			return to_digit_hex(chr);
			when others =>	report "Unknown base character: " & base & "." severity failure;
											-- return statement is explicitly missing otherwise XST won't stop
		end case;
	end function;

	function to_natural_bin(str : STRING) return INTEGER is
		variable Result			: NATURAL;
		variable Digit			: INTEGER;
	begin
		for i in str'range loop
			Digit	:= to_digit_bin(str(I));
			if (Digit /= -1) then
				Result	:= Result * 2 + Digit;
			else
				return -1;
			end if;
		end loop;
				
		return Result;
	end function;

	function to_natural_oct(str : STRING) return INTEGER is
		variable Result			: NATURAL;
		variable Digit			: INTEGER;
	begin
		for i in str'range loop
			Digit	:= to_digit_oct(str(I));
			if (Digit /= -1) then
				Result	:= Result * 8 + Digit;
			else
				return -1;
			end if;
		end loop;
				
		return Result;
	end function;

	function to_natural_dec(str : STRING) return INTEGER is
		variable Result			: NATURAL;
		variable Digit			: INTEGER;
	begin
		for i in str'range loop
			Digit	:= to_digit_dec(str(I));
			if (Digit /= -1) then
				Result	:= Result * 10 + Digit;
			else
				return -1;
			end if;
		end loop;
				
		return Result;
--		return INTEGER'value(str);			-- 'value(...) is not supported by Vivado Synth 2014.1
	end function;

	function to_natural_hex(str : STRING) return INTEGER is
		variable Result			: NATURAL;
		variable Digit			: INTEGER;
	begin
		for i in str'range loop
			Digit	:= to_digit_hex(str(I));
			if (Digit /= -1) then
				Result	:= Result * 16 + Digit;
			else
				return -1;
			end if;
		end loop;
				
		return Result;
	end function;

	function to_natural(str : STRING; base : CHARACTER := 'd') return INTEGER is
	begin
		case base is
			when 'b' =>			return to_natural_bin(str);
			when 'o' =>			return to_natural_oct(str);
			when 'd' =>			return to_natural_dec(str);
			when 'h' =>			return to_natural_hex(str);
			when others =>	report "unknown base" severity ERROR;
		end case;
	end function;

	-- to_raw*
	-- ===========================================================================
	function to_RawChar(char : character) return t_rawchar is
	begin
		return std_logic_vector(to_unsigned(character'pos(char), t_rawchar'length));
	end function;

	function to_RawString(str : STRING) return T_RAWSTRING is
		variable rawstr			: T_RAWSTRING(0 to str'length - 1);
	begin
		for i in str'low to str'high loop
			rawstr(i - str'low)	:= to_RawChar(str(i));
		end loop;
		return rawstr;
	end function;

	-- resize
	-- ===========================================================================
	function resize(str : STRING; size : POSITIVE; FillChar : CHARACTER := C_POC_NUL) return STRING is
		constant ConstNUL		: STRING(1 to 1)				:= (others => C_POC_NUL);
		variable Result			: STRING(1 to size);
	begin
		Result := (others => FillChar);
		if (str'length > 0) then
			Result(1 to imin(size, imax(1, str'length))) := ite((str'length > 0), str(1 to imin(size, str'length)), ConstNUL);
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
	function chr_toLower(chr : character) return character is
	begin
		if chr_isUpperAlpha(chr) then
			return character'val(character'pos(chr) - character'pos('A') + character'pos('a'));
		else
			return chr;
		end if;
	end function;
	
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
	function str_length(str : STRING) return NATURAL is
	begin
		for i in str'range loop
			if (str(i) = C_POC_NUL) then
				return i - str'low;
			end if;
		end loop;
		return str'length;
	end function;
	
	function str_equal(str1 : STRING; str2 : STRING) return BOOLEAN is
	begin
		if str1'length /= str2'length then
			return FALSE;
		else
			return (str1 = str2);
		end if;
	end function;

	function str_match(str1 : STRING; str2 : STRING) return BOOLEAN is
		constant len	: NATURAL 		:= imin(str1'length, str2'length);
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

	function str_imatch(str1 : STRING; str2 : STRING) return BOOLEAN is
	begin
		return str_match(str_toLower(str1), str_toLower(str2));
	end function;
	
	function str_pos(str : STRING; chr : CHARACTER; start : NATURAL := 0) return INTEGER is
	begin
		for i in imax(str'low, start) to str'high loop
			exit when (str(i) = C_POC_NUL);
			if (str(i) = chr) then
				return i;
			end if;
		end loop;
		return -1;
	end function;
	
	function str_pos(str : STRING; pattern : STRING; start : NATURAL := 0) return INTEGER is
	begin
		for i in imax(str'low, start) to (str'high - pattern'length + 1) loop
			exit when (str(i) = C_POC_NUL);
			if (str(i to i + pattern'length - 1) = pattern) then
				return i;
			end if;
		end loop;
		return -1;
	end function;
	
	function str_ipos(str : STRING; chr : CHARACTER; start : NATURAL := 0) return INTEGER is
	begin
		return str_pos(str_toLower(str), chr_toLower(chr));
	end function;
	
	function str_ipos(str : STRING; pattern : STRING; start : NATURAL := 0) return INTEGER is
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
	
	function str_find(str : STRING; chr : CHARACTER) return boolean is
	begin
		return (str_pos(str, chr) > 0);
	end function;
	
	function str_find(str : STRING; pattern : STRING) return boolean is
	begin
		return (str_pos(str, pattern) > 0);
	end function;
	
	function str_ifind(str : STRING; chr : CHARACTER) return boolean is
	begin
		return (str_ipos(str, chr) > 0);
	end function;
	
	function str_ifind(str : STRING; pattern : STRING) return boolean is
	begin
		return (str_ipos(str, pattern) > 0);
	end function;
	
	function str_replace(str : STRING; pattern : STRING; replace : STRING) return STRING is
		variable pos		: INTEGER;
	begin
		pos := str_pos(str, pattern);
		if (pos > 0) then
			if (pos = 1) then
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
	
	-- examples:
	--							  123456789ABC
	-- input string: "Hello World."
	--	low=1; high=12; length=12
	--
	--	str_substr("Hello World.",	0,	0)	=> "Hello World."		- copy all
	--	str_substr("Hello World.",	7,	0)	=> "World."					- copy from pos 7 to end of string
	--	str_substr("Hello World.",	7,	5)	=> "World"					- copy from pos 7 for 5 characters
	--	str_substr("Hello World.",	0, -7)	=> "Hello World."		- copy all until character 8 from right boundary
	function str_substr(str : STRING; start : INTEGER := 0; length : INTEGER := 0) return STRING is
		variable StartOfString		: positive;
		variable EndOfString			: positive;
	begin
		if (start < 0) then			-- start is negative -> start substring at right string boundary
			StartOfString		:= str'high + start + 1;
		elsif (start = 0) then	-- start is zero -> start substring at left string boundary
			StartOfString		:= str'low;
		else 										-- start is positive -> start substring at left string boundary + offset
			StartOfString		:= start;
		end if;

		if (length < 0) then		-- length is negative -> end substring at length'th character before right string boundary
			EndOfString			:= str'high + length;
		elsif (length = 0) then	-- length is zero -> end substring at right string boundary
			EndOfString			:= str'high;
		else										-- length is positive -> end substring at StartOfString + length
			EndOfString			:= StartOfString + length - 1;
		end if;
		
		if (StartOfString < str'low) then			report "StartOfString is out of str's range. (str=" & str & ")" severity error;		end if;
		if (EndOfString < str'high) then			report "EndOfString is out of str's range. (str=" & str & ")" severity error;			end if;
		
		return str(StartOfString to EndOfString);
	end function;
	
	function str_ltrim(str : STRING; char : CHARACTER := ' ') return STRING is
	begin
		for i in str'range loop
			if (str(i) /= char) then
				return str(i to str'high);
			end if;
		end loop;
		return "";
	end function;

	function str_rtrim(str : STRING; char : CHARACTER := ' ') return STRING is
	begin
		for i in str'reverse_range loop
			if (str(i) /= char) then
				return str(str'low to i);
			end if;
		end loop;
		return "";
	end function;
	
	function str_trim(str : STRING) return STRING is
	begin
		return str(str'low to str'low + str_length(str) - 1);
	end function;
	
	function str_toLower(str : STRING) return STRING is
		variable temp		: STRING(str'range);
	begin
		for i in str'range loop
			temp(I)	:= chr_toLower(str(I));
		end loop;
		return temp;
	end function;
	
	function str_toUpper(str : STRING) return STRING is
		variable temp		: STRING(str'range);
	begin
		for i in str'range loop
			temp(I)	:= chr_toUpper(str(I));
		end loop;
		return temp;
	end function;

end package body;
