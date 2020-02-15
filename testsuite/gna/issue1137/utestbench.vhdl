
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

entity testbench is
end entity;

architecture simu of testbench is

	-- Dummy control
	signal clk : std_ulogic := '0';
	signal simu_clock_enable : std_ulogic := '1';

	-- Dummy source signal
	signal data_src  : std_ulogic_vector(1 downto 0) := "00";
	-- Three destination signals
	signal data_dst1 : std_ulogic_vector(1 downto 0) := "11";
	signal data_dst2 : std_ulogic_vector(1 downto 0) := "11";
	signal data_dst3 : std_ulogic_vector(1 downto 0) := "11";
	signal data_dst4 : std_ulogic_vector(1 downto 0) := "11";

begin

	-- Solution 1
	-- THIS WORKS

	process(all)
		variable idx : integer;
	begin

		for c in 0 to 1 loop

			idx := c;

			data_dst1(idx) <= data_src(idx);

		end loop;

	end process;

	-- Solution 2
	-- FIXME THIS DOES NOT WORK, CREATES XXX

	gen2 : for c in 0 to 1 generate

		process(all)
			variable idx : integer;
		begin

			idx := c;

			data_dst2(idx) <= data_src(idx);

		end process;

	end generate;

	-- Solution 4
	-- THIS WORKS

	gen4 : for c in 0 to 1 generate

		process(all)
			constant idx : integer := c;
		begin

			data_dst4(idx) <= data_src(idx);

		end process;

	end generate;

	-- Solution 3
	-- THIS WORKS

	gen3 : for c in 0 to 1 generate

		constant idx : integer := c;

	begin

		data_dst3(idx) <= data_src(idx);

	end generate;

	-- Dummy clock generation

	clk <= (not clk) and simu_clock_enable after 5 ns;

	-- Main testbench process

	process
		-- To print simulation messages
		variable l : line;
	begin

		wait until rising_edge(clk);
		wait until rising_edge(clk);

		write(l, string'("Result 1 : "));
		write(l, to_string(data_dst1));
		writeline(output, l);

		write(l, string'("Result 2 : "));
		write(l, to_string(data_dst2));
		writeline(output, l);

		write(l, string'("Result 3 : "));
		write(l, to_string(data_dst3));
		writeline(output, l);

		write(l, string'("Result 4 : "));
		write(l, to_string(data_dst4));
		writeline(output, l);

		simu_clock_enable <= '0';

	end process;

end architecture;

