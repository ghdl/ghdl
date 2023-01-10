-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

-- Package with stop watch specific types.
package StopWatch_pkg is
	subtype T_BCD is unsigned(3 downto 0);
	type T_BCD_Vector is array(natural range <>) of T_BCD;

	type T_DIGIT_CONFIGURATION is record
		Modulo : positive;
		Dot    : std_logic;
	end record;

	type T_STOPWATCH_CONFIGURATION is array(natural range <>) of T_DIGIT_CONFIGURATION;

	-- Encoder that translates from 4-bit binary (BCD) to 7-segment code.
	--
	-- In addition, an optional dot input is supported.
	component seg7_Encoder is
		port (
			BCDValue  : in  T_BCD;
			Dot       : in  std_logic  := '0';

			Seg7Code  : out std_logic_vector(7 downto 0)
		);
	end component;

	component seg7_Display is
		generic (
			CLOCK_PERIOD  : time := 10 ns;
			REFRESH_RATE  : time := 200 us;
			DIGITS        : positive
		);
		port (
			Clock         : in  std_logic;

			DigitValues   : in  T_BCD_Vector(DIGITS - 1 downto 0);
			DotValues     : in  std_logic_vector(DIGITS - 1 downto 0) := (others => '0');

			Seg7_Segments : out std_logic_vector(7 downto 0);
			Seg7_Selects  : out std_logic_vector(DIGITS - 1 downto 0)
		);
	end component;
end package;
