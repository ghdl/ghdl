library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity mul_218 is
	port (
		result : out std_logic_vector(31 downto 0);
		in_a : in  std_logic_vector(31 downto 0);
		in_b : in  std_logic_vector(14 downto 0)
	);
end mul_218;

architecture augh of mul_218 is

	signal tmp_res : signed(46 downto 0);

begin

	-- The actual multiplication
	tmp_res <= signed(in_a) * signed(in_b);

	-- Set the output
	result <= std_logic_vector(tmp_res(31 downto 0));

end architecture;
