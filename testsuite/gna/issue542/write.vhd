library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity write is
	port(
	clk   : in std_logic;
	reset : in std_logic;
	write : in std_logic;
	ack   : out std_logic
);
end write;

architecture a of write is
begin

	process (clk, reset) is
	begin
		if reset = '1' then
			ack <= '0';
		elsif rising_edge(clk) then
			if write = '1' then
				ack <= '1';
			else
				ack <= '0';
			end if;
		end if;
	end process;

end architecture;
