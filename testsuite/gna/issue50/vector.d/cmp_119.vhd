library ieee;
use ieee.std_logic_1164.all;

entity cmp_119 is
	port (
		ne : out std_logic;
		in0 : in  std_logic_vector(15 downto 0);
		in1 : in  std_logic_vector(15 downto 0)
	);
end cmp_119;

architecture augh of cmp_119 is

	signal tmp : std_logic;

begin

	-- Compute the result
	tmp <=
		'0' when in0 /= in1 else
		'1';

	-- Set the outputs
	ne <= not(tmp);

end architecture;
