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

	function fun(b : b_array_t) return natural is
		variable c : natural;
	begin
		c := 0;
		for i in b'range loop
			for j in b'element.a'range loop
				c := c+1;
			end loop;
		end loop;
		return c;
	end function;

	constant b : b_array_t(0 to 1)(a(0 to 31)(value(31 downto 0))) := (others => (a => (others => (value => (others => '0' )))));
	
	constant dummy : natural := fun(b);
begin
end architecture;
