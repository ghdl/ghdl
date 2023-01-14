library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reset_synchronizer is
	port(
		clk         : in  std_logic;
		async_rst_n : in  std_logic;
		rst_n       : out std_logic
	);
end entity reset_synchronizer;

architecture behavior of reset_synchronizer is
	signal r_ff1 : std_logic;
begin
	SYNCHRONIZE : process(clk, async_rst_n)
	begin
		if async_rst_n = '0' then
			r_ff1 <= '0';
			rst_n <= '0';
		elsif rising_edge(clk) then
			r_ff1 <= '1';
			rst_n <= r_ff1;
		end if;
	end process;
end architecture;
