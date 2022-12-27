-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

library lib_Utilities;
use     lib_Utilities.Utilities_pkg.all;

use     work.StopWatch_pkg.all;


entity toplevel_tb is
end entity;


architecture tb of toplevel_tb is
	constant CLOCK_PERIOD : time      := 10 ns;

	signal StopSimulation : std_logic := '0';
	signal Clock          : std_logic := '1';
	signal Reset          : std_logic := '1';

	signal StartButton    : std_logic := '0';

begin
	StopSimulation <= '1' after 30 ms;

	Clock <= (Clock xnor StopSimulation) after CLOCK_PERIOD / 2;
	Reset <= '0' after 2 us,
	         '1' after 3 us,
	         '0' after 20 ms,
	         '1' after 20 ms + 2 us;
	StartButton <= '1' after 10 us,
	               '0' after 15 us,
	               '1' after 10 ms,
	               '0' after 10 ms + 1 us,
	               '1' after 12 ms,
	               '0' after 12 ms + 2 us,
	               '1' after 22 ms,
	               '0' after 22 ms + 2 us;

	DUT: entity lib_StopWatch.toplevel
		generic map (
			CLOCK_PERIOD_NS => CLOCK_PERIOD / 1 ns
		)
		port map (
			Clock          => Clock,
			Reset_n        => Reset,

			Button(0)      => StartButton,
			Seg7_Cathode_n => open,
			Seg7_Anode_n   => open
		);

end architecture;
