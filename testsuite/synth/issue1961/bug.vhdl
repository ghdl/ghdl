library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port (
		clk : in std_ulogic
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

	function fun return natural is
		variable b : b_array_t(0 to 1)(a(0 to 31)(value(31 downto 0)));
	begin
		return 0;
	end function;

	constant dummy : natural := fun;
begin
	
end architecture;
