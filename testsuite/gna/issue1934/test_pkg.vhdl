library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package test_pkg is

	type inner_t is record
		value : unsigned;
	end record;

	type inner_array_t is array(natural range<>) of inner_t;

	type outer_t is record
		inner : inner_array_t;
	end record;

	function fun return outer_t;

end test_pkg;

package body test_pkg is

	function fun return outer_t is
		variable ret : outer_t(inner(0 to 31)(value(31 downto 0)));
	begin
		return ret;
	end function;

end package body test_pkg;
