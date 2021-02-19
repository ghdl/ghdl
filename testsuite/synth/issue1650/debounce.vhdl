library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity debounce is
	generic (
		N_CYCLES: integer := 255
	);
	port
	(
		clk: in std_ulogic;
		reset_n: in std_ulogic;
		debounce_in: in std_ulogic;
		debounced_out: buffer std_ulogic
	);
end debounce;

architecture rtl of debounce is
    signal counter: integer range 0 to N_CYCLES;
    signal previous_debounce_in: std_ulogic := '0';
begin
	process(clk, reset_n)
	begin
        if (reset_n = '0') then
            previous_debounce_in <= '0';
			debounced_out <= '0';
			counter <= 0;
        elsif rising_edge(clk) then

			if debounce_in = previous_debounce_in then
				if counter = N_CYCLES then
					debounced_out <= debounce_in;
				else
					counter <= counter + 1;
				end if;
			else
				counter <= 0;
            end if;

            previous_debounce_in <= debounce_in;
		end if;
	end process;
end rtl;
