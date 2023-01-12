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


entity toplevel is
	generic (
		constant CLOCK_PERIOD       : time := 10 ns
	);
	port (
		NexysA7_SystemClock         : in  std_logic;
		NexysA7_GPIO_Button_Reset_n : in  std_logic;

		NexysA7_GPIO_Button         : in  std_logic_vector(0 downto 0);
		NexysA7_GPIO_Seg7_Cathode_n : out std_logic_vector(7 downto 0);
		NexysA7_GPIO_Seg7_Anode_n   : out std_logic_vector(7 downto 0)
	);
end entity;


architecture rtl of toplevel is
	constant STOPWATCH_CONFIGURATION : T_STOPWATCH_CONFIGURATION := (
		0 => (Modulo => 10, Dot => '0'),
		1 => (Modulo => 10, Dot => '0'),
		2 => (Modulo => 10, Dot => '0'),
		3 => (Modulo =>  6, Dot => '0'),
		4 => (Modulo => 10, Dot => '0'),
		5 => (Modulo =>  6, Dot => '0')
	);

	signal Board_Reset  : std_logic;

	signal Deb_Reset    : std_logic;
	signal Deb_Start    : std_logic;
	signal Deb_Start_d  : std_logic := '0';
	signal Deb_Start_re : std_logic;

	signal Reset        : std_logic;
	signal Start        : std_logic;


	signal Digits  : T_BCD_Vector(STOPWATCH_CONFIGURATION'length - 1 downto 0);

	signal Cathode : std_logic_vector(7 downto 0);
	signal Anode   : std_logic_vector(Digits'range);

begin
	-- convert from low-active inputs
	Board_Reset <= not NexysA7_GPIO_Button_Reset_n;

	-- Debounce input signals
	deb: component Debouncer
		generic map (
			CLOCK_PERIOD  => CLOCK_PERIOD,
			BITS          => 2
		)
		port map (
			Clock     => NexysA7_SystemClock,

			Input(0)  => Board_Reset,
			Input(1)  => NexysA7_GPIO_Button(0),
			Output(0) => Deb_Reset,
			Output(1) => Deb_Start
		);

	Reset <= Deb_Reset;

	-- Rising edge detection
	Deb_Start_d  <= Deb_Start when rising_edge(NexysA7_SystemClock);
	Deb_Start_re <= not Deb_Start_d and Deb_Start;

	-- renaming
	Start <= Deb_Start_re;

	-- Stopwatch
	sw: entity work.Stopwatch
		generic map (
			CLOCK_PERIOD  => CLOCK_PERIOD,

			TIMEBASE      => 10 ms,
			CONFIG        => STOPWATCH_CONFIGURATION
		)
		port map (
			Clock  => NexysA7_SystemClock,
			Reset  => Reset,

			Start  => Start,

			Digits => Digits
		);

	-- 7-segment display
	display: /* configuration */ seg7_Display--_cfg
		generic map (
			CLOCK_PERIOD  => CLOCK_PERIOD,
			DIGITS        => Digits'length
		)
		port map (
			Clock         => NexysA7_SystemClock,

			DigitValues   => Digits,

			Seg7_Segments => Cathode,
			Seg7_Selects  => Anode
		);

	-- convert to low-active outputs
	NexysA7_GPIO_Seg7_Cathode_n <= not Cathode;
	NexysA7_GPIO_Seg7_Anode_n   <= not ((NexysA7_GPIO_Seg7_Anode_n'high downto Anode'length => '0') & Anode);
end architecture;
