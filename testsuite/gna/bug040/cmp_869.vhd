library ieee;
use ieee.std_logic_1164.all;

entity cmp_869 is
	port (
		eq : out std_logic;
		in1 : in  std_logic_vector(7 downto 0);
		in0 : in  std_logic_vector(7 downto 0)
	);
end cmp_869;

architecture augh of cmp_869 is

	signal tmp : std_logic;

begin

	-- Compute the result
	tmp <=
		'0' when in1 /= in0 else
		'1';

	-- Set the outputs
	eq <= tmp;

end architecture;
