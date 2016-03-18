library ieee;
use ieee.std_logic_1164.all;

entity cmp_790 is
	port (
		ne : out std_logic;
		in1 : in  std_logic_vector(3 downto 0);
		in0 : in  std_logic_vector(3 downto 0)
	);
end cmp_790;

architecture augh of cmp_790 is

	signal tmp : std_logic;

begin

	-- Compute the result
	tmp <=
		'0' when in1 /= in0 else
		'1';

	-- Set the outputs
	ne <= not(tmp);

end architecture;
