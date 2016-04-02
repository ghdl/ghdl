library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity sub_145 is
	port (
		result : out std_logic_vector(3 downto 0);
		in_a : in  std_logic_vector(3 downto 0);
		in_b : in  std_logic_vector(3 downto 0)
	);
end sub_145;

architecture augh of sub_145 is

	signal carry_inA : std_logic_vector(5 downto 0);
	signal carry_inB : std_logic_vector(5 downto 0);
	signal carry_res : std_logic_vector(5 downto 0);

begin

	-- To handle the CI input, the operation is '0' - CI
	-- If CI is not present, the operation is '0' - '0'
	carry_inA <= '0' & in_a & '0';
	carry_inB <= '0' & in_b & '0';
	-- Compute the result
	carry_res <= std_logic_vector(unsigned(carry_inA) - unsigned(carry_inB));

	-- Set the outputs
	result <= carry_res(4 downto 1);

end architecture;
