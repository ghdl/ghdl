library ieee;
use ieee.std_logic_1164.all;

entity test_case is
	port (
          clk : in  std_logic;
          ia, ib : std_logic
	);
end entity;

architecture arch of test_case is
  signal a, b : std_logic;
begin
	process (clk)
	begin
		if rising_edge(clk) then
			a <= ia;
		elsif falling_edge(clk) then
			b <= ib;
		end if;
	end process;
end architecture;
