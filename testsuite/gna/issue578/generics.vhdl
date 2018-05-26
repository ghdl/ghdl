library ieee;
use ieee.std_logic_1164.all;
--use ieee.math_real.all;
use ieee.numeric_std.all;

package generics is
	constant synthesis : boolean := true
									-- synthesis translate_off
									and false
									-- synthesis translate_on
									;

	function sim_cond(cond : boolean) return boolean;

	function istr(v : integer) return string;
	
	function as_std_logic(v : boolean) return std_logic;

	function rot_l(v : std_logic_vector; n : integer := 1) return std_logic_vector;
	function rot_r(v : std_logic_vector; n : integer := 1) return std_logic_vector;
	function sh_l(v : std_logic_vector; n : integer := 1) return std_logic_vector;
	function sh_r(v : std_logic_vector; n : integer := 1) return std_logic_vector;

	function log2_int(v : integer) return integer;
	function log2(v : integer) return integer;
	function is_log2(v : integer) return boolean;
	
	function binary_flatten(v : std_logic_vector; n : integer) return std_logic_vector;
	function min(a : integer; b : integer) return integer;
	function max(a : integer; b : integer) return integer;
	
	function find_first_bit(v : std_logic_vector) return integer;
	function mask_first_bit(v : std_logic_vector) return std_logic_vector;
	function next_multiple(v : integer; m : integer) return integer;
	function reverse_vector(inp : std_logic_vector) return std_logic_vector;
	function reorder_vector(inp : std_logic_vector) return std_logic_vector;
	function htonl(inp : std_logic_vector) return std_logic_vector;
	function ntohl(inp : std_logic_vector) return std_logic_vector;
	function vector_all_valid(i : std_logic_vector) return boolean;
	function to_integer(i : std_logic) return integer;
	
	function integer_reverse_bits(i : integer; bits : integer) return integer;
	
	function dbg_collapse16(d : in std_logic_vector) return std_logic_vector;			

	function bram_we_width(aw : integer) return integer;

	function chr(sl: std_logic) return character;
	function chr(i : integer) return character;
	function chr(b : boolean) return character;
	function vector_to_string(v : std_logic_vector) return string;
	function vector_to_hex_string(v : std_logic_vector) return string;

	
	function to_std_logic_vector(v : integer; size : integer) return std_logic_vector;

	function popcnt(v : std_logic_vector) return integer;
	function popcnt(v : integer; bits : integer) return integer;
	function popcnt_x(v : std_logic_vector) return integer;
	
	function maxlen_lfsr_advance(reg : std_logic_vector) return std_logic_vector;
	function random_vector(len : integer; seed : integer) return std_logic_vector;
	function random(max : integer; seed : integer) return integer;
		
	function reverse_any_vector(a : in std_logic_vector) return std_logic_vector;

	function sel(cond : boolean; if_true : integer; if_false : integer) return integer;
	function sel(cond : boolean; if_true : std_logic_vector; if_false : std_logic_vector) return std_logic_vector;
	function sel(cond : boolean; if_true : std_logic; if_false : std_logic) return std_logic;
	function sel(cond : boolean; if_true : string; if_false : string) return string;
	procedure clkp(signal clk : in std_logic; n : in integer);

	function vector_mux(sel : std_logic_vector; i1 : std_logic_vector; i2 : std_logic_vector) return std_logic_vector;
	function div_ceil(a : integer; b : integer) return integer;

	function int_strlen(vv : integer) return natural;
end generics;

package body generics is
	function chr(b : boolean) return character is
	begin
		if b then
			return 't';
		end if;
		return 'f';
	end function;

	function chr(i : integer) return character is
		variable s : string(1 to 10) := "0123456789";
	begin
		if i < 10 then
			return s(i + 1);
		else
			return 'X';
		end if;
	end function;

	function istr(v : integer) return string is
	begin
		return integer'image(v);
	end function;
	

	function as_std_logic(v : boolean) return std_logic is
	begin
		if v then return '1'; end if;
		return '0';
	end function;

	function int_strlen(vv : integer) return natural is
		variable ret : natural := 0;
		variable v : integer := vv;
	begin
		if v < 0 then
			ret := ret + 1;
			v := -v;
		end if;
		while v >= 10 loop
			v := v / 10;
			ret := ret + 1;
		end loop;
		return ret + 1;
	end function;
	
	function rot_l(v : std_logic_vector; n : integer := 1) return std_logic_vector is
	begin
		return std_logic_vector(rotate_left(unsigned(v), n));
	end function;
	function rot_r(v : std_logic_vector; n : integer := 1) return std_logic_vector is
	begin
		return std_logic_vector(rotate_right(unsigned(v), n));
	end function;
	function sh_l(v : std_logic_vector; n : integer := 1) return std_logic_vector is
	begin
		return std_logic_vector(shift_left(unsigned(v), n));
	end function;
	function sh_r(v : std_logic_vector; n : integer := 1) return std_logic_vector is
	begin
		return std_logic_vector(shift_right(unsigned(v), n));
	end function;
	function log2(v : integer) return integer is
	begin
		return log2_int(v);
	end function;
	function is_log2(v : integer) return boolean is
	begin
		return 2 ** log2(v) = v;
	end function;
	function to_integer(i : std_logic) return integer is
	begin
		if i = '1' then
			return 1;
		elsif i = '0' then
			return 0;
		else
			return -1;
		end if;
	end function;
	function dbg_collapse16(d : in std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(0 to 15);
		variable oi : integer;
	begin
		oi := 0;
		ret := (others => '0');
		for i in d'range loop
			ret(oi) := ret(oi) xor d(i);
			if oi < 15 then
				oi := oi + 1;
			else
				oi := 0;
			end if;
		end loop;
		return ret;
	end function;
	function random(max : integer; seed : integer) return integer is
	begin
		if max = 0 then return 0; end if;
		return to_integer(unsigned(random_vector(log2_int(max) + 1, seed))) mod max;
	end function;
	function sel(cond : boolean; if_true : std_logic; if_false : std_logic) return std_logic is
	begin
		if cond then
			return if_true;
		end if;
		return if_false;
	end function;
	function sel(cond : boolean; if_true : string; if_false : string) return string is
	begin
		if cond then
			return if_true;
		else
			return if_false;
		end if;
	end function;
	function div_ceil(a : integer; b : integer) return integer is
	begin
		return a / b + sel(a mod b /= 0, 1, 0);
	end function;



	function sim_cond(cond : boolean) return boolean is
	begin
		if synthesis then
			return true;
		else
			return cond;
		end if;
	end function;
	
	function integer_reverse_bits(i : integer; bits : integer) return integer is
		variable m : std_logic_vector(0 to bits - 1);
	begin
		m := std_logic_vector(to_unsigned(i, bits));
		m := reverse_any_vector(m);
		return to_integer(unsigned(m));
	end function;

	function vector_mux(sel : std_logic_vector; i1 : std_logic_vector; i2 : std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(0 to sel'length - 1);
	begin
		for i in 0 to sel'length - 1 loop
			if sel(sel'left + i) = '1' then
				ret(i) := i1(i1'left + i);
			else
				ret(i) := i2(i2'left + i);
			end if;
		end loop;
		return ret;
	end function;


	procedure clkp(signal clk : in std_logic; n : in integer) is
	begin
		for i in 1 to n loop
			wait until rising_edge(clk);
		end loop;
		wait for 1 ps;
	end procedure;

	function random_vector(len : integer; seed : integer) return std_logic_vector is
		variable lfsr : std_logic_vector(0 to 7) := std_logic_vector(to_unsigned(seed, 8));
		variable ret : std_logic_vector(0 to len - 1);
	begin
		for i in 0 to len / 8 loop
			lfsr := maxlen_lfsr_advance(lfsr);
			lfsr := maxlen_lfsr_advance(lfsr);
			lfsr := maxlen_lfsr_advance(lfsr);
			lfsr := maxlen_lfsr_advance(lfsr);
			ret(i * 8 to min(ret'length, (i + 1) * 8) - 1) := lfsr(0 to min(8, ret'length - i * 8) - 1);
		end loop;
		return ret;
	end function;
	function sel(cond : boolean; if_true : integer; if_false : integer) return integer is
	begin
		if cond then return if_true; else return if_false; end if;
	end function;
	function sel(cond : boolean; if_true : std_logic_vector; if_false : std_logic_vector) return std_logic_vector is
	begin
		if cond then return if_true; else return if_false; end if;
	end function;

	function popcnt(v : integer; bits : integer) return integer is
	begin
		return popcnt(std_logic_vector(to_unsigned(v, bits)));
	end function;

	function reverse_any_vector (a: in std_logic_vector) return std_logic_vector is
		variable result: std_logic_vector(a'range);
		alias aa: std_logic_vector(a'reverse_range) is a;
	begin
		for i in aa'range loop
			result(i) := aa(i);
		end loop;
		return result;
	end;
	function maxlen_lfsr_advance(reg : std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(reg'range);
	begin
		if ret'left > ret'right then
			ret(reg'left downto 1) := reg(reg'left - 1 downto 0);
		else
			ret(1 to reg'right) := reg(0 to reg'right - 1);
		end if;
		if reg'length = 3 then
			ret(0) := reg(2) xnor reg(1);
		elsif reg'length = 4 then
			ret(0) := reg(3) xnor reg(2);
		elsif reg'length = 8 then
			ret(0) := reg(7) xnor reg(5) xnor reg(4) xnor reg(3);
		elsif reg'length = 57 then
			ret(0) := reg(56) xnor reg(49);
		else
			assert false report "no matching shift register configured for length " & integer'image(reg'length) severity failure;
		end if;
		return ret;
	end function;



	function vector_all_valid(i : std_logic_vector) return boolean is
	begin
		for j in i'range loop
			if i(j) /= '1' and i(j) /= '0' then
				return false;
			end if;
		end loop;
		return true;
	end function;

	function popcnt_x(v : std_logic_vector) return integer is
		variable ret : integer;
	begin
		ret := 0;
		for i in v'range loop
			if v(i) = 'X' then
				ret := ret + 1;
			end if;
		end loop;
		return ret;
	end function;
	
	function popcnt(v : std_logic_vector) return integer is
		variable res : integer;
	begin
		res := 0;
		for i in v'range loop
			if v(i) = '1' then
				res := res + 1;
			end if;
		end loop;
		return res;
	end function;
	function to_std_logic_vector(v : integer; size : integer) return std_logic_vector is
		variable ret : std_logic_vector(size - 1 downto 0);
		variable tmp : unsigned(size - 1 downto 0);
	begin
		tmp := to_unsigned(v, size);
		return std_logic_vector(tmp);
	end function;


	function chr(sl: std_logic) return character is
		variable c: character;
    begin
		case sl is
			when 'U' => c:= 'U';
			when 'X' => c:= 'X';
			when '0' => c:= '0';
			when '1' => c:= '1';
			when 'Z' => c:= 'Z';
			when 'W' => c:= 'W';
			when 'L' => c:= 'L';
			when 'H' => c:= 'H';
			when '-' => c:= '-';
		end case;
		return c;
	end chr;

	function vector_to_hex_string(v : std_logic_vector) return string is
		variable ret : string(1 to (v'length + 3) / 4);
		type hchar_t is array(0 to 15) of character;
		constant hchar : hchar_t := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
		variable off : integer := v'length mod 4;
	begin
		if off /= 0 then
			ret(1) := hchar(to_integer(unsigned(v(0 to off - 1))));
		end if;
		for i in 0 to v'length / 4 - 1 loop
			ret(i + sel(off = 0, 1, 2)) := hchar(to_integer(unsigned(v(off + i * 4 to off + i * 4 + 3))));
		end loop;
		return ret;
	end function;

	function vector_to_string(v : std_logic_vector) return string is
		variable ret : string(1 to v'length);
		variable at : integer;
	begin
		at := 1;
		for i in v'range loop
			ret(at) := chr(v(i));
			at := at + 1;
		end loop;
		return ret;
--		for i in 0 to v'length - 1 loop
--			ret(i + 1) := chr(v(i));
--		end loop;
--		return ret;
	end function;

	function bram_we_width(aw : integer) return integer is
	begin
		if aw > 16 then
			return 4;
		elsif aw > 8 then
			return 2;
		else
			return 1;
		end if;
	end function;

	function htonl(inp : std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(inp'range);
	begin
		ret(31 downto 24) := inp(7 downto 0);
		ret(23 downto 16) := inp(15 downto 8);
		ret(15 downto 8)  := inp(23 downto 16);
		ret(7 downto 0)   := inp(31 downto 24);
		return ret;
	end function;

	function ntohl(inp : std_logic_vector) return std_logic_vector is
	begin
		return htonl(inp);
	end function;

	function reorder_vector(inp : std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(inp'reverse_range);
	begin
		return inp;
		if inp'left < inp'right then
			for i in inp'range loop
				ret(inp'right - i) := inp(i);
			end loop;
		elsif inp'left > inp'right then
			for i in inp'range loop
				ret(inp'left - i) := inp(i);
			end loop;
		else
			ret(inp'left) := inp(inp'left);
		end if;
		return ret;
	end function;

	function reverse_vector(inp : std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(inp'reverse_range);
	begin
		for i in inp'range loop
			ret(i) := inp(i);
		end loop;
		return ret;
	end function;

	function next_multiple(v : integer; m : integer) return integer is
	begin
		if v mod m = 0 then
			return v;
		else
			return v + m - v mod m;
		end if;
	end function;
	function mask_first_bit(v : std_logic_vector) return std_logic_vector is
		variable ret : std_logic_vector(v'range) := (others => '0');
	begin
		for i in 0 to v'length - 1 loop
			if v(i) = '1' then
				ret(i) := '1';
				return ret;
			end if;
		end loop;
		return ret;
	end function;
		
	function find_first_bit(v : std_logic_vector) return integer is
	begin
		for i in 0 to v'length - 1 loop
			if v(i) = '1' then return i; end if;
		end loop;
		return 0;
	end function;

	function min(a : integer; b : integer) return integer is
	begin
		if a < b then
			return a;
		end if;
		return b;
	end function;
	function max(a : integer; b : integer) return integer is
	begin
		if a > b then
			return a;
		end if;
		return b;
	end function;

	function binary_flatten(v : std_logic_vector; n : integer) return std_logic_vector is
		variable res : std_logic_vector(n - 1 downto 0) := (others => '0');
	begin
		for i in 0 to n - 1 loop
			if unsigned(v) = to_unsigned(i, v'length) then
				res(i) := '1';
				return res;
			end if;
		end loop;
		return res;
	end function;

	function log2_int(v : integer) return integer is
		variable vv, ret : integer;
	begin
		vv := v;
		ret := 0;
		while vv > 1 loop
			ret := ret + 1;
			vv  := vv / 2;
		end loop;
		if 2 ** ret = v then
			return ret;
		else
			return ret + 1;
		end if;
		--return integer(ceil(log2(real(v))));
	end function;
end generics;
