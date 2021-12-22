library IEEE;
use IEEE.std_logic_1164.all;

entity bwc is
	port
	(
		enable, clock, reset : in std_logic;
		button_pressed       : in std_logic;
		open_window, close_window    : out std_logic
	);
end bwc;

architecture behave of bwc is

-- User defined type
type SM_State is (w_closed, w_open);
signal current_state, next_state: SM_State;


begin
	-- State memory process
	State_memory: process (clock)
	begin
		if    (reset  = '0') then current_state <= w_closed;
		
		elsif (enable = '1' and rising_edge(clock)) then 
			current_state <= next_state;
		
		end if;
	end process;
	
	-- Next state logic
	Next_state_logic: process (clock)
	begin
		case (current_state) is
			when w_open =>  if (button_pressed = '1') 
							then next_state <= w_closed;
							else next_state <= w_open;
							end if;

			when w_closed =>if (button_pressed = '1') 
							then next_state <= w_open;
							else next_state <= w_closed;
							end if;
		end case;
	end process;
	
	-- Output logic
	Output_logic: process(clock)
	begin
		case (current_state) is
			when w_open =>  if (button_pressed = '1') 
							then open_window <= '0'; close_window <= '1';
							else open_window <= '0'; close_window <= '0';
							end if;

			when w_closed =>if (button_pressed = '1') 
							then open_window <= '1'; close_window <= '0';
							else open_window <= '0'; close_window <= '0';
							end if;
			when others => open_window <= '0'; close_window <= '0';
		end case;
	end process;
end architecture;
