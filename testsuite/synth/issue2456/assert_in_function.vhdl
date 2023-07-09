library ieee;
use ieee.std_logic_1164.all;

entity assert_in_function_tester is
	port(
		clk_in: in std_logic;
		a_in: in std_logic_vector(7 downto 0);
		b_out: out std_logic_vector(7 downto 0);
		c_out: out std_logic_vector(7 downto 0)
	);
end;

architecture formal of assert_in_function_tester is
	function test_function(a: std_logic_vector) return std_logic_vector is
	begin
		assert a(0) = '1';
		return a;
	end;
begin
	b_out <= test_function(a_in);

	-- If this process is commented out, no assertin is seen with sby.
	process(all)
	begin
		assert a_in(0) = '1';
		c_out <= a_in;
	end process;

	default clock is rising_edge(clk_in);

	assume {(a_in(0) = '1')[*5]};
end;
