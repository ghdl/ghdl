
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

entity tb is
end tb;

architecture augh of tb is

	constant simu_max_cycles : natural := 100000;
	constant simu_disp_cycles : std_logic := '1';
	constant simu_err_end_in : std_logic := '0';
	constant reset_cycles : natural := 4;

	component top is
		port (
			clock : in  std_logic;
			reset : in  std_logic;
			start : in  std_logic;
			stdin_rdy : out std_logic;
			stdin_ack : in  std_logic;
			stdout_data : out std_logic_vector(31 downto 0);
			stdout_rdy : out std_logic;
			stdout_ack : in  std_logic;
			stdin_data : in  std_logic_vector(31 downto 0)
		);
	end component;

	signal clock : std_logic := '0';
	signal reset : std_logic := '0';
	signal start : std_logic := '0';

	signal clock_next : std_logic := '0';

	-- Access 'clock' model 'clock'

	-- Access 'reset' model 'reset'

	-- Access 'start' model 'start'

	-- Access 'stdin' model 'fifo_in'
	signal stdin_data : std_logic_vector(31 downto 0) := (others => '0');
	signal stdin_rdy : std_logic := '0';
	signal stdin_ack : std_logic := '0';
	signal stdin_vector_idx : natural := 0;
	signal stdin_vector : std_logic_vector(31 downto 0) := (others => '0');
	-- Test vectors
	constant stdin_vectors_nb : natural := 100;
	type stdin_vec_type is array (0 to stdin_vectors_nb-1) of std_logic_vector(31 downto 0);
	constant stdin_vectors : stdin_vec_type := (
		X"00000044", X"00000044", X"00000044", X"00000044", X"00000044", X"00000044", X"00000044", X"00000044",
		X"00000044", X"00000044", X"00000044", X"00000044", X"00000044", X"00000044", X"00000044", X"00000044",
		X"00000044", X"00000043", X"00000043", X"00000043", X"00000043", X"00000043", X"00000043", X"00000043",
		X"00000042", X"00000042", X"00000042", X"00000042", X"00000042", X"00000042", X"00000041", X"00000041",
		X"00000041", X"00000041", X"00000041", X"00000040", X"00000040", X"00000040", X"00000040", X"00000040",
		X"00000040", X"00000040", X"00000040", X"0000003f", X"0000003f", X"0000003f", X"0000003f", X"0000003f",
		X"0000003e", X"0000003e", X"0000003e", X"0000003e", X"0000003e", X"0000003e", X"0000003d", X"0000003d",
		X"0000003d", X"0000003d", X"0000003d", X"0000003d", X"0000003c", X"0000003c", X"0000003c", X"0000003c",
		X"0000003c", X"0000003c", X"0000003c", X"0000003c", X"0000003c", X"0000003b", X"0000003b", X"0000003b",
		X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b",
		X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003b",
		X"0000003b", X"0000003b", X"0000003b", X"0000003b", X"0000003c", X"0000003c", X"0000003c", X"0000003c",
		X"0000003c", X"0000003c", X"0000003c", X"0000003c"
	);

	-- Access 'stdout' model 'fifo_out'
	signal stdout_data : std_logic_vector(31 downto 0) := (others => '0');
	signal stdout_rdy : std_logic := '0';
	signal stdout_ack : std_logic := '0';
	signal stdout_vector_idx : natural := 0;
	signal stdout_vector : std_logic_vector(31 downto 0) := (others => '0');
	-- Test vectors
	constant stdout_vectors_nb : natural := 150;
	type stdout_vec_type is array (0 to stdout_vectors_nb-1) of std_logic_vector(31 downto 0);
	constant stdout_vectors : stdout_vec_type := (
		X"000000fd", X"000000de", X"00000077", X"000000ba", X"000000f4", X"00000092", X"00000020", X"000000a0",
		X"000000ec", X"000000ed", X"000000ee", X"000000f0", X"000000f1", X"000000f1", X"000000f2", X"000000f3",
		X"000000f4", X"000000f4", X"000000f3", X"000000f5", X"000000f5", X"000000f5", X"000000f6", X"000000f6",
		X"000000f6", X"000000f7", X"000000f8", X"000000f6", X"000000f7", X"000000f8", X"000000f7", X"000000f8",
		X"000000f8", X"000000f6", X"000000f8", X"000000f8", X"000000f7", X"000000f9", X"000000f9", X"000000f8",
		X"000000f8", X"000000f8", X"000000f7", X"000000fa", X"000000fb", X"000000fb", X"000000fa", X"000000fb",
		X"000000fb", X"000000fb", X"00000000", X"00000000", X"00000000", X"00000000", X"ffffffff", X"00000000",
		X"00000000", X"ffffffff", X"00000000", X"00000000", X"ffffffff", X"ffffffff", X"00000000", X"00000000",
		X"ffffffff", X"fffffffd", X"fffffffe", X"fffffffd", X"ffffffff", X"fffffffc", X"00000000", X"ffffffff",
		X"ffffffff", X"fffffffb", X"00000000", X"00000000", X"ffffffff", X"00000004", X"0000000b", X"00000009",
		X"0000000b", X"0000000d", X"00000011", X"00000010", X"00000014", X"00000013", X"00000016", X"00000013",
		X"00000016", X"00000017", X"0000001a", X"0000001a", X"0000001d", X"0000001e", X"00000021", X"0000001f",
		X"0000001e", X"0000001a", X"0000001e", X"00000020", X"00000026", X"00000025", X"00000026", X"00000023",
		X"00000025", X"00000024", X"00000027", X"00000025", X"00000028", X"00000028", X"0000002b", X"00000029",
		X"0000002d", X"0000002e", X"0000002f", X"00000028", X"00000027", X"00000027", X"0000002d", X"0000002f",
		X"00000031", X"0000002d", X"0000002d", X"0000002c", X"00000031", X"00000030", X"0000002f", X"00000028",
		X"0000002a", X"0000002d", X"00000033", X"00000030", X"0000002e", X"00000029", X"0000002d", X"00000030",
		X"00000037", X"00000035", X"00000035", X"00000030", X"00000030", X"0000002e", X"00000031", X"0000002e",
		X"0000002f", X"0000002c", X"00000031", X"00000034", X"0000003a", X"0000003a"
	);

	signal clock_counter      : natural := 0;
	signal clock_counter_stop : natural := 0;

	signal errors_nb : natural := 0;

	-- Defined in VHDL 2008, not handled by GHDL
	function to_string(sv: std_logic_vector) return string is
		variable bv: bit_vector(sv'range) := to_bitvector(sv);
		variable lp: line;
	begin
		write(lp, bv);
		return lp.all;
	end;

begin

	-- Instantiation of the main component
	top_i : top port map (
		-- Access 'clock' model 'clock'
		clock => clock,
		-- Access 'reset' model 'reset'
		reset => reset,
		-- Access 'start' model 'start'
		start => start,
		-- Access 'stdin' model 'fifo_in'
		stdin_data => stdin_data,
		stdin_rdy => stdin_rdy,
		stdin_ack => stdin_ack,
		-- Access 'stdout' model 'fifo_out'
		stdout_data => stdout_data,
		stdout_rdy => stdout_rdy,
		stdout_ack => stdout_ack
	);

	-- Functionality for top-level access 'clock' model 'clock'
	-- Generation of clock: 100MHz (note: arbitrary value)
	clock <= clock_next after 5 ns;
	clock_next <= not clock when clock_counter_stop = 0 or clock_counter <= clock_counter_stop else '0';

	-- Clock counter and global messages
	process (clock)
		-- To print simulation messages
		variable l : line;
	begin

		-- Increment clock counter
		if rising_edge(clock) then

			clock_counter <= clock_counter + 1;

			if false and simu_disp_cycles = '1' then
				-- Write simulation message
				write(l, string'("INFO clock cycle "));
				write(l, clock_counter);
				writeline(output, l);
			end if;

		end if;

		-- Messages
		if falling_edge(clock) then

			if clock_counter > simu_max_cycles then
				report "ERROR Too many cycles simulated. Stopping simulation." severity failure;
			end if;

			if clock_counter < reset_cycles then
				report "INFO Reset" severity note;
			end if;

			if clock_counter = reset_cycles then
				report "INFO Start" severity note;
			end if;

		end if;

	end process;

	-- Functionality for top-level access 'reset' model 'reset'
	-- Generation of reset
	reset <= '1' when clock_counter < reset_cycles else '0';

	-- Functionality for top-level access 'start' model 'start'
	-- Generation of start
	start <= '1';

	-- Functionality for top-level access 'stdin' model 'fifo_in'
	-- FIFO stdin
	-- Sending inputs

	stdin_vector <= stdin_vectors(stdin_vector_idx) when stdin_vector_idx < stdin_vectors_nb else (others => '0');
	stdin_data <= stdin_vector(31 downto 0);

	stdin_ack <= '1' when reset = '0' and stdin_vector_idx < stdin_vectors_nb else '0';

	process (clock)
		-- To print simulation messages
		variable l : line;
	begin

		if rising_edge(clock) then

			if stdin_vector_idx < stdin_vectors_nb then

				if stdin_rdy = '1' and stdin_ack = '1' and reset = '0' then

					-- Write simulation message
					write(l, string'("INFO Input vector "));
					write(l, stdin_vector_idx);
					write(l, string'(" at cycle "));
					write(l, clock_counter);
					writeline(output, l);

					if stdin_vector_idx = 0 then
						write(l, string'("INFO First input vector sent at clock cycle "));
						write(l, clock_counter);
						writeline(output, l);
					end if;

					if stdin_vector_idx = stdin_vectors_nb - 1 then
						write(l, string'("INFO Last input vector sent at clock cycle "));
						write(l, clock_counter);
						writeline(output, l);
					end if;

					-- Increase vector index
					stdin_vector_idx <= stdin_vector_idx + 1;

				end if;  -- Handshake

			else

				if stdin_rdy = '1' and reset = '0' then
					if simu_err_end_in = '1' then
						report "ERROR Out of input vectors. Stopping simulation." severity failure;
					end if;
				end if;  -- Handshake

			end if;

		end if;

	end process;

	-- Functionality for top-level access 'stdout' model 'fifo_out'
	-- FIFO stdout
	-- Checking outputs

	-- Always enable output FIFO
	stdout_ack <= '1' when stdout_vector_idx < stdout_vectors_nb and reset = '0' else '0';

	stdout_vector <= stdout_vectors(stdout_vector_idx) when stdout_vector_idx < stdout_vectors_nb else (others => '0');

	-- Check outputs
	process (clock)
		variable l : line;
	begin

		if rising_edge(clock) then

			if stdout_vector_idx < stdout_vectors_nb then

				if stdout_rdy = '1' and stdout_ack = '1' and reset = '0' then

					if stdout_data = stdout_vector(31 downto 0) then
						-- The vector is verified

						write(l, string'("INFO Output nb "));
						write(l, stdout_vector_idx);
						write(l, string'(" at cycle "));
						write(l, clock_counter);
						write(l, string'(" (check OK)"));
						write(l, string'(" Obtained "));
						write(l, to_string(stdout_data));
						writeline(output, l);

					else
						-- An error is detected

						write(l, string'("ERROR Output nb "));
						write(l, stdout_vector_idx);
						write(l, string'(" at cycle "));
						write(l, clock_counter);
						writeline(output, l);

						write(l, string'("  Obtained "));
						write(l, to_string(stdout_data));
						writeline(output, l);
						write(l, string'("  Expected "));
						write(l, to_string(stdout_vector(31 downto 0)));
						writeline(output, l);

						errors_nb <= errors_nb + 1;

						--report "ERROR A simulation error was found." severity failure;

					end if;

					if stdout_vector_idx = stdout_vectors_nb - 1 then

						write(l, string'("INFO Last output vector read at cycle "));
						write(l, clock_counter);
						writeline(output, l);

						report "INFO Stopping simulation." severity note;

						clock_counter_stop <= clock_counter + 3;

					end if;

					-- Increase vector index
					stdout_vector_idx <= stdout_vector_idx + 1;

				end if;  -- FIFO handshake

			else
				-- All vectors have been read

				if errors_nb > 0 then
					write(l, string'("ERROR Number of errors found : "));
					write(l, errors_nb);
					writeline(output, l);
					report "ERROR Simulation errors were found." severity failure;
				end if;

			end if;  -- Check all vectors read

		end if;  -- Clock

	end process;

end augh;

