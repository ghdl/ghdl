library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	generic(
		C_HIGH : natural := 5
	);
	port(
		reset_n : in std_ulogic;
		clk     : in std_ulogic;
                res     : out natural
	);
end bug;

architecture behav of bug is
	subtype st is natural range 0 to C_HIGH;
	signal c : st;
begin
	process(clk, reset_n)
	begin
		if reset_n = '0' then
			c <= st'low;
		elsif rising_edge(clk) then
			if c = st'high then
				c <= st'low;
			else
				c <= c + 1;
			end if;
		end if;
	end process;
        res <= c;
end architecture;
