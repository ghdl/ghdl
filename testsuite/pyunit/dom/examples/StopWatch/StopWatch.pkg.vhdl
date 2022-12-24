-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;


package StopWatch_pkg is
	subtype T_BCD is unsigned(3 downto 0);
	type T_BCD_Vector is array(natural range <>) of T_BCD;

	type T_DIGIT_CONFIGURATION is record
		Modulo : positive;
		Dot    : std_logic;
	end record;

	type T_STOPWATCH_CONFIGURATION is array(natural range <>) of T_DIGIT_CONFIGURATION;
end package;
