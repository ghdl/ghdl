-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

context work.Utilities_ctx;


entity Debouncer is
	generic (
		CLOCK_PERIOD   : time := 10 ns;
		DEBOUNCE_TIME  : time := 3 ms;

		BITS           : positive;
		INPUT_SYNC     : boolean := true
	);
	port (
		Clock  : in  std_logic;

		Input  : in  std_logic_vector(BITS - 1 downto 0);
		Output : out std_logic_vector(BITS - 1 downto 0) := (others => '0')
	);
end entity;

architecture rtl of Debouncer is
	constant DEBOUNCE_COUNTER_MAX  : positive := DEBOUNCE_TIME / (CLOCK_PERIOD* ite(IS_SIMULATION, 20, 1));
	constant DEBOUNCE_COUNTER_BITS : positive := log2(DEBOUNCE_COUNTER_MAX);

	signal Input_sync : Input'subtype;
begin
	assert false report "CLOCK_PERIOD:         " & time'image(CLOCK_PERIOD);
	assert false report "DEBOUNCE_TIME:        " & time'image(DEBOUNCE_TIME);
	--assert false report "DEBOUNCE_COUNTER_MAX: " & to_string(10 ns);
	--assert false report "INTEGER'high:         " & integer'image(integer'high);

	genSync: if INPUT_SYNC generate
		sync: entity work.sync_Bits
			generic map (
				BITS => BITS
			)
			port map (
				Clock  => Clock,
				Input  => Input,
				Output => Input_sync
			);
	else generate
		Input_sync <= Input;
	end generate;

	genBits: for i in Input'range generate
		signal DebounceCounter         : signed(DEBOUNCE_COUNTER_BITS downto 0) := to_signed(DEBOUNCE_COUNTER_MAX - 3, DEBOUNCE_COUNTER_BITS + 1);
	begin
		process (Clock)
		begin
			if rising_edge(Clock) then
				-- restart counter, whenever Input(i) was unstable within DEBOUNCE_TIME_MS
				if (Input(i) /= Output(i)) then
					DebounceCounter <= DebounceCounter - 1;
				else
					DebounceCounter <= to_signed(DEBOUNCE_COUNTER_MAX - 3, DebounceCounter'length);
				end if;

				-- latch input bit, if input was stable for DEBOUNCE_TIME_MS
				if (DebounceCounter(DebounceCounter'high) = '1') then
					Output(i) <= Input(i);
				end if;
			end if;
		end process;
	end generate;
end architecture;
