library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity counter is
	generic(
		LEN    : positive := 1
	);
	port(
		clk           : in std_ulogic;
		reset_n       : in std_ulogic
	);
end counter;

architecture behav of counter is
	signal c : integer range 0 to LEN-1;
begin
	process(clk, reset_n)
	begin
		if reset_n = '0' then
			c  <= 0;
		elsif rising_edge(clk) then
			if c = LEN-1 then
				c <= 0;
			else
				c <= c + 1;
			end if;
		end if;
	end process;
end architecture;
