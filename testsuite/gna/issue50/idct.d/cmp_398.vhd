library ieee;
use ieee.std_logic_1164.all;

entity cmp_398 is
	port (
		eq : out std_logic;
		in0 : in  std_logic_vector(2 downto 0);
		in1 : in  std_logic_vector(2 downto 0)
	);
end cmp_398;

architecture augh of cmp_398 is

	signal tmp : std_logic;

begin

	-- Compute the result
	tmp <=
		'0' when in0 /= in1 else
		'1';

	-- Set the outputs
	eq <= tmp;

end architecture;
