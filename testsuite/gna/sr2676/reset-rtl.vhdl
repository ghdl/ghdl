library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.reset_types.all;

architecture rtl of power_on_reset is
	--signal current_state: reset_state;
	signal current_state: reset_state;
	signal next_state: reset_state;
	signal por_counter: unsigned(31 downto 0);
	signal saved_por_counter: unsigned(31 downto 0);

	constant DELAY_POWER_STABLE_CLOCKS: unsigned := to_unsigned((10 * 1000), 32)/clk_period_ns;
	constant DELAY_PM_READ_ID_CLOCKS: unsigned := 
		DELAY_POWER_STABLE_CLOCKS + to_unsigned((10 * 1000), 32)/clk_period_ns;
	constant DELAY_PM_READ_ID_DONE_CLOCKS: unsigned := 
		DELAY_PM_READ_ID_CLOCKS + to_unsigned((10 * 1000), 32)/clk_period_ns;
	

begin

-- advance the state variable and detect reset 
advance_state: process(clk, reset)
begin
	if (reset = '1') then
		current_state <= RST;
	elsif rising_edge(clk) then
		current_state <= next_state;
	end if;
end process advance_state;

run_counter: process(clk, current_state)
begin
	if (current_state = RST) then
		por_counter <= (others => '0');
	elsif (current_State = MONITOR_RESET) then
		por_counter <= por_counter;
	else
		por_counter <= por_counter + 1;
	end if;
end process run_counter;

-- select the next state
select_state: process(clk)
begin
	sys_clken <= '1';
	core_en <= '1';
	case current_state is
		when RST =>
			core_en <= '0';
			sys_clken <= '0';
			clk_33mhz_en <= '0';
			cfg_drv <= '0';
			rd_pmbrd_rev <= '0';
			next_state <= ENABLE_CORE_POWER;
	
		when ENABLE_CORE_POWER =>
			if (por_counter < DELAY_POWER_STABLE_CLOCKS) then
				-- wait for core power to stabilize
				next_state <= ENABLE_CORE_POWER;
			else
				next_state <= SELECT_PROCESSOR_POR_CONFIG;
			end if;

		when SELECT_PROCESSOR_POR_CONFIG =>
			cfg_drv <= '1';
			next_state <= SAMPLE_HW_CONFIG;

		when SAMPLE_HW_CONFIG =>
			if (por_counter < DELAY_PM_READ_ID_CLOCKS) then
				-- delay until ready to read personality module ID
				next_state <= SAMPLE_HW_CONFIG;
			else
				next_state <= SELECT_PERSONALITY_ID;
			end if;

		when SELECT_PERSONALITY_ID =>
			rd_pmbrd_rev <= '1';
			next_state <= SAMPLE_PERSONALITY_ID;

		when SAMPLE_PERSONALITY_ID =>
			if (por_counter < DELAY_PM_READ_ID_DONE_CLOCKS) then
				next_state <= SAMPLE_PERSONALITY_ID;
			else
				next_state <= DESELECT_PERSONALITY_ID;
			end if;

		when DESELECT_PERSONALITY_ID =>
			rd_pmbrd_rev <= '0';
			next_state <= DEASSERT_HRESET;

		when DEASSERT_HRESET =>
			saved_por_counter <= por_counter;
			if (por_counter < (saved_por_counter + 4)) then
				next_state <= DEASSERT_HRESET;
			else
				next_state <= DESELECT_PROCESSOR_POR_CONFIG;
			end if;

		when DESELECT_PROCESSOR_POR_CONFIG =>
			cfg_drv <= '0';
			next_state <= WAIT_FOR_PROCESSOR;

		when WAIT_FOR_PROCESSOR =>
			next_state <= ENABLE_ALL_DEVICES;

		when ENABLE_ALL_DEVICES =>
			next_state <= MONITOR_RESET;

		when MONITOR_RESET =>
			next_state <= MONITOR_RESET;
	end case;
end process select_state;

-- output state variable
cur_state <= current_state;

end rtl;


