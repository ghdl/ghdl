library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity strings_test is
end strings_test;

architecture rtl of strings_test is
	type T_TYPE				is (TYPE_1, TYPE_2, TYPE_3);

	function str_low(str : string) return integer is
	begin
                assert str'low = 1 severity failure;
		return str'low;
	end function;
	
	constant string1 : string := "TYPE_1";
	constant string2 : T_TYPE := TYPE_1;
	
begin

assert false report "str_low of type'image     : i =" & integer'image(str_low(T_TYPE'image(string2))) severity note;
assert false report "tic low of type'image     : i =" & integer'image(T_TYPE'image(string2)'low) severity note;
assert false report "tic low of string         : i =" & integer'image(string1'low) severity note;
assert false report "str_low of string         : i =" & integer'image(str_low(string1)) severity note;

end rtl;
