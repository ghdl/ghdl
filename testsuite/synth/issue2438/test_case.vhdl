
library ieee;
use ieee.std_logic_1164.all;

entity test_case is
	port (
		clk : in  std_logic
	);
end entity;

architecture arch of test_case is

	signal a : std_logic;
	signal b : std_logic;

begin

	process (clk)
	begin

		if rising_edge(clk) then
			a <= '0';
		elsif falling_edge(clk) then
			b <= '0';
		end if;

	end process;

end architecture;
