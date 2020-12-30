library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

package pack_1 is
	constant const_1 : boolean;

	type matrix is array(natural range <>, natural range <>) of std_logic;

	subtype matrix8x8 is matrix(7 downto 0, 7 downto 0);

	function func1(value : unsigned) return natural;
end package;

package body pack_1 is
	constant const_1 : boolean := true;

	function func1(value : unsigned) return natural is
	begin
		return to_integer(value);
	end function;
end package body;
