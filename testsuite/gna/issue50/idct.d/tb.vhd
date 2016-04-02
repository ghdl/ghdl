--test bench written by Alban Bourge @ TIMA
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.pkg_tb.all;

entity tb is
end tb;

architecture behavior of tb is

	signal clock      : std_logic  := '0';
	signal clock_next : std_logic  := '0';
	signal reset      : std_logic  := '0';
	--TOP signals
	signal n_error    : std_logic;
	signal stopped    : std_logic;

begin

	test : entity work.cp3_test(rtl)
	port map(
						clock   => clock,
						reset   => reset,
						n_error => n_error,
						stopped => stopped
					);

	--clock generator
	clock <= clock_next after 10 ns;
	clock_next <= not clock when stopped = '0' else '0';

	reset_proc : process
	begin
		reset <= '1';
		for i in 1 to 5 loop
			wait until rising_edge(clock);
		end loop;
		reset <= '0';
		wait;
	end process reset_proc;

end behavior;
