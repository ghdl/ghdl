-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;

-- Package with stop watch specific types.
package StopWatch_pkg is
	type T_DIGIT_CONFIGURATION is record
		Modulo : positive;
		Dot    : std_logic;
	end record;

	type T_STOPWATCH_CONFIGURATION is array(natural range <>) of T_DIGIT_CONFIGURATION;
end package;
