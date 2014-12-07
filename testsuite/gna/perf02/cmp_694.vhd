library ieee;
use ieee.std_logic_1164.all;

entity cmp_694 is
	port (
		in1 : in  std_logic_vector(31 downto 0);
		in0 : in  std_logic_vector(31 downto 0);
		eq : out std_logic
	);
end cmp_694;

architecture augh of cmp_694 is

	signal tmp : std_logic;

begin

	-- Compute the result
	tmp <=
		'0' when in1 /= in0 else
		'1';

	-- Set the outputs
	eq <= tmp;

end architecture;
