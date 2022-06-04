library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

entity tb_swaptest is
end tb_swaptest;

architecture behaviour of tb_swaptest
is
	constant clk_period : time := 10 ns;
	signal clk : std_logic;
	signal d : unsigned(7 downto 0) := X"c5";
	signal q : unsigned(7 downto 0);
begin

	clk_process: process
	begin
	    for i in 1 to 10 loop
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
	    end loop;
	    wait;
	end process;

	st : entity work.swaptest
	port map (
		clk => clk,
		d => d,
		q => q
	);

end architecture;
