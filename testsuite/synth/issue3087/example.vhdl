library ieee;
use ieee.std_logic_1164.all;

entity example is
	port (
		csi_clk : in std_logic;
		rsi_rst : in std_logic;

		gpo_dummy : out std_logic
	);
end entity example;

architecture rtl of example is
begin
	p_main : process (csi_clk) is
	begin
		if rising_edge(csi_clk) then
			gpo_dummy <= '1';

			if rsi_rst then
				gpo_dummy <= '0';
			end if;
		end if; -- rising_edge(csi_clk)
	end process p_main;
end architecture rtl;
