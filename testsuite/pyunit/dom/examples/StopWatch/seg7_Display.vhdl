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


entity seg7_Display is
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
end entity;


architecture rtl of seg7_Display is
	constant TIMEBASE_COUNTER_MAX : positive := REFRESH_RATE / (CLOCK_PERIOD * ite(IS_SIMULATION, 1_000, 1));

	signal Timebase_Counter : unsigned(log2(TIMEBASE_COUNTER_MAX) - 1 downto 0) := (others => '0');
	signal Timebase_Tick    : std_logic;

	signal Digit_Select     : unsigned(log2(DIGITS) - 1 downto 0) := (others => '0');
	signal Digit_Select_ov  : std_logic;

	signal Digit            : T_BCD;
	signal Dot              : std_logic;
begin
	-- refresh rate
	process(Clock)
	begin
		if rising_edge(Clock) then
			if (Timebase_Tick = '1') then
				Timebase_Counter <= (others => '0');
			else
				Timebase_Counter <= Timebase_Counter + 1;
			end if;
		end if;
	end process;

	Timebase_Tick <= '1' when (Timebase_Counter = TIMEBASE_COUNTER_MAX - 1) else '0';


	-- counter to select digits (time multiplexing)
	process(Clock)
	begin
		if rising_edge(Clock) then
			if (Timebase_Tick = '1') then
				if (Digit_Select_ov = '1') then
					Digit_Select <= (others => '0'); -- to_unsigned(5, Digit_Select'length);
				else
					Digit_Select <= Digit_Select + 1;
				end if;
			end if;
		end if;
	end process;

	Digit_Select_ov <= '1' when (Digit_Select = DIGITS - 1) else '0';

	-- multiplexer
	Digit <= DigitValues(to_index(Digit_Select, DigitValues'high));
	Dot   <= DotValues(to_index(Digit_Select, DotValues'high));

	-- 7-segment encoder
	enc: component seg7_Encoder
		port map (
			BCDValue  => Digit,
			Dot       => Dot,

			Seg7Code  => Seg7_Segments
		);


	Seg7_Selects <= bin2onehot(Digit_Select, DIGITS);
end architecture;
