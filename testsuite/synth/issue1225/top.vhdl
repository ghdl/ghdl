library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top is
	port (
		clk, en : in std_logic;
		a, b : in std_logic;
		p, q : out std_logic
	);
end entity;

architecture arch of top is
begin
	process (clk, en, a)
		variable tmp : std_logic;
	begin
		if en = '1' then
			tmp := a;
			p <= tmp;
		else
			p <= '0';
		end if;

		if rising_edge(clk) then
			tmp := b;
			q <= tmp;
		end if;
	end process;
end architecture;
