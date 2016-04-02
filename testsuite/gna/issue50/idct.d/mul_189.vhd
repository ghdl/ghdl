library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity mul_189 is
	port (
		result : out std_logic_vector(30 downto 0);
		in_a : in  std_logic_vector(30 downto 0);
		in_b : in  std_logic_vector(13 downto 0)
	);
end mul_189;

architecture augh of mul_189 is

	signal tmp_res : signed(44 downto 0);

begin

	-- The actual multiplication
	tmp_res <= signed(in_a) * signed(in_b);

	-- Set the output
	result <= std_logic_vector(tmp_res(30 downto 0));

end architecture;
