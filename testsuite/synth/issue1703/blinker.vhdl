library ieee;
use ieee.std_logic_1164.all;

entity Blinker is
	port(
		clk_i : in std_logic;
		rst_i : in std_logic;
		led_o : out std_logic
	);
end Blinker;

architecture RTL of Blinker is
	constant N : natural := 25e6;
	signal count_reg : natural range 0 to N - 1;
begin
	process(rst_i, clk_i)
	begin
		if rst_i = '1' then
			count_reg <= 0;
		elsif rising_edge(clk_i) then
			if count_reg = N - 1 then
				count_reg <= 0;
			else
				count_reg <= count_reg + 1;
			end if;
		end if;
	end process;

	led_o <= '1' when count_reg < N / 2 else '0';
end RTL;
