library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port (
		clk :  in std_ulogic
	);
end bug;

architecture struct of bug is

	type a_t is record
		value : unsigned;
	end record;

	type a_array_t is array(natural range<>) of a_t;

	type b_t is record
		a : a_array_t;
	end record;

	type b_array_t is array(natural range<>) of b_t;

	type table_t is array (natural range<>, natural range<>) of a_t;

	function make_b return b_array_t is
		variable ret : b_array_t(0 to 0)(a(0 to 31)(value(31 downto 0)));
	begin
		return ret;
	end function;

	function calculate_b return b_array_t is
		function calculate_b(prefix : b_array_t; cur_value : natural) return b_array_t is
		begin
			if cur_value > 0 then
				return prefix;
			else
				return calculate_b(prefix & make_b, cur_value + 1);
			end if;
		end function;

		variable empty : b_array_t(0 to -1)(a(0 to 31)(value(31 downto 0)));
	begin
		return calculate_b(empty, 0);
	end function;

	function calculate_table(b : b_array_t) return table_t is
		variable ret : table_t(0 to b'length-1, 0 to b(0).a'length-1)(value(31 downto 0));
	begin
		return ret;
	end function;

	constant b     : b_array_t := calculate_b;
	constant table : table_t := calculate_table(b);
begin

end architecture;

