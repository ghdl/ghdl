library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity fsm_15 is
	port (
		clock : in  std_logic;
		reset : in  std_logic;
		out3 : out std_logic;
		out157 : out std_logic;
		out159 : out std_logic;
		out160 : out std_logic;
		out171 : out std_logic;
		out172 : out std_logic;
		out173 : out std_logic;
		out175 : out std_logic;
		out178 : out std_logic;
		in0 : in  std_logic;
		out0 : out std_logic;
		in5 : in  std_logic;
		in6 : in  std_logic;
		in7 : in  std_logic;
		out35 : out std_logic;
		out39 : out std_logic;
		out40 : out std_logic;
		out41 : out std_logic;
		out44 : out std_logic;
		out46 : out std_logic;
		out140 : out std_logic;
		in8 : in  std_logic;
		in9 : in  std_logic;
		in10 : in  std_logic;
		in11 : in  std_logic;
		in12 : in  std_logic;
		in13 : in  std_logic;
		in14 : in  std_logic;
		out65 : out std_logic;
		in1 : in  std_logic;
		in2 : in  std_logic;
		in3 : in  std_logic;
		in4 : in  std_logic;
		out225 : out std_logic;
		out227 : out std_logic;
		out231 : out std_logic;
		out235 : out std_logic;
		out236 : out std_logic;
		out237 : out std_logic;
		out238 : out std_logic;
		out97 : out std_logic;
		out98 : out std_logic;
		out101 : out std_logic;
		out102 : out std_logic;
		out124 : out std_logic;
		out125 : out std_logic;
		out80 : out std_logic;
		out81 : out std_logic;
		out84 : out std_logic;
		out86 : out std_logic;
		out88 : out std_logic;
		out93 : out std_logic;
		out94 : out std_logic
	);
end fsm_15;

architecture augh of fsm_15 is

	signal state_cur  : std_logic_vector(0 to 21) := (19 => '1', others => '0');
	signal state_next : std_logic_vector(0 to 21) := (19 => '1', others => '0');

	-- Buffers for outputs
	signal out0_buf : std_logic := '0';
	signal out0_bufn : std_logic;
	signal out40_buf : std_logic := '0';
	signal out40_bufn : std_logic;
	signal out101_buf : std_logic := '0';
	signal out101_bufn : std_logic;
	signal out172_buf : std_logic := '0';
	signal out172_bufn : std_logic;

	-- Function calls: return IDs
	signal funccall0 :      unsigned(0 downto 0) := (others => '0');
	signal funccall0_next : unsigned(0 downto 0) := (others => '0');

begin

	-- Sequential process
	-- Set the current state

	process (clock)
	begin
		if rising_edge(clock) then

			-- Next state
			state_cur <= state_next;
			-- Buffers for outputs
			out0_buf <= out0_bufn;
			out40_buf <= out40_bufn;
			out101_buf <= out101_bufn;
			out172_buf <= out172_bufn;
			-- Function calls: return IDs
			funccall0 <= funccall0_next;

		end if;
	end process;

	-- Combinatorial process
	-- Compute the next state
	-- Compute the outputs

	process (
		-- Inputs of the FSM
		reset, in0, in5, in6, in7, in8, in9, in10, in11, in12, in13, in14, in1, in2, in3, in4,
		-- Function calls: return IDs
		funccall0,
		-- Current state
		state_cur
	)
	begin

		-- Reset the next state value

		state_next <= (others => '0');

		-- Default value to the outputs or output buffers

		out46 <= '0';
		out35 <= '0';
		out65 <= '0';
		out39 <= '0';
		out3 <= '0';
		out44 <= '0';
		out0_bufn <= '0';
		out41 <= '0';
		out40_bufn <= '0';
		out80 <= '0';
		out81 <= '0';
		out84 <= '0';
		out86 <= '0';
		out88 <= '0';
		out93 <= '0';
		out94 <= '0';
		out97 <= '0';
		out98 <= '0';
		out101_bufn <= '0';
		out102 <= '0';
		out124 <= '0';
		out125 <= '0';
		out140 <= '0';
		out157 <= '0';
		out159 <= '0';
		out160 <= '0';
		out171 <= '0';
		out172_bufn <= '0';
		out173 <= '0';
		out175 <= '0';
		out178 <= '0';
		out225 <= '0';
		out227 <= '0';
		out231 <= '0';
		out235 <= '0';
		out236 <= '0';
		out237 <= '0';
		out238 <= '0';

		-- Function calls: default values (no change)
		funccall0_next <= funccall0;

		-- For all states, compute the next state bits
		--   And the outputs, and the next value for buffered outputs

		if state_cur(0) = '1' then
			-- Next state
			if ((in4) or (in3) or (in2)) = '1' then
				state_next(17) <= '1';
				-- Next values for buffered outputs
			else
				if (in1) = '1' then
					-- Function call: memextrct_0
					-- Save the origin of the call
					funccall0_next <= to_unsigned(0, 1);
					-- This is where the function call leads
					state_next(4) <= '1';
					-- Next values for buffered outputs
					out0_bufn <= '1';
				else
					if (in0) = '1' then
						-- Function call: memextrct_1
						state_next(2) <= '1';
						-- Next values for buffered outputs
						out0_bufn <= '1';
					else
						state_next(17) <= '1';
						-- Next values for buffered outputs
					end if;
				end if;
			end if;
			-- Assignment of non-buffered outputs
			out3 <= '1';
		end if;

		if state_cur(1) = '1' then
			-- Next state
			if (in5) = '1' then
				state_next(1) <= '1';
				-- Next values for buffered outputs
				out40_bufn <= '1';
			else
				-- Return from function: memextrct_1
				-- Function call: memextrct_0
				-- Save the origin of the call
				funccall0_next <= to_unsigned(1, 1);
				-- This is where the function call leads
				state_next(4) <= '1';
				-- Next values for buffered outputs
				out0_bufn <= '1';
			end if;
			-- Assignment of non-buffered outputs
			out41 <= '1';
			out39 <= '1';
			out35 <= '1';
		end if;

		if state_cur(2) = '1' then
			-- Next state
			state_next(1) <= '1';
			-- Next values for buffered outputs
			out40_bufn <= '1';
			-- Assignment of non-buffered outputs
			out44 <= '1';
		end if;

		if state_cur(3) = '1' then
			-- Next state
			if (in6) = '1' then
				state_next(3) <= '1';
				-- Next values for buffered outputs
				out40_bufn <= '1';
			else
				-- Return from function: memextrct_0
				if funccall0 = 1 then
					state_next(17) <= '1';
					-- Next values for buffered outputs
				end if;
				-- Return from function: memextrct_0
				if funccall0 = 0 then
					state_next(17) <= '1';
					-- Next values for buffered outputs
				end if;
			end if;
			-- Assignment of non-buffered outputs
			out41 <= '1';
			out65 <= '1';
			out46 <= '1';
		end if;

		if state_cur(4) = '1' then
			-- Next state
			state_next(3) <= '1';
			-- Next values for buffered outputs
			out40_bufn <= '1';
			-- Assignment of non-buffered outputs
			out44 <= '1';
		end if;

		if state_cur(5) = '1' then
			-- Next state
			if (in8) = '1' then
				state_next(20) <= '1';
				-- Next values for buffered outputs
			else
				if (not (in7)) = '1' then
					state_next(5) <= '1';
					-- Next values for buffered outputs
				else
					state_next(6) <= '1';
					-- Next values for buffered outputs
				end if;
			end if;
			-- Assignment of non-buffered outputs
			out84 <= '1';
			out81 <= '1';
			out80 <= '1';
		end if;

		if state_cur(6) = '1' then
			-- Next state
			if (in8) = '1' then
				state_next(20) <= '1';
				-- Next values for buffered outputs
			else
				if (not (in9)) = '1' then
					state_next(6) <= '1';
					-- Next values for buffered outputs
				else
					state_next(7) <= '1';
					-- Next values for buffered outputs
				end if;
			end if;
			-- Assignment of non-buffered outputs
			out88 <= '1';
			out86 <= '1';
			out81 <= '1';
		end if;

		if state_cur(7) = '1' then
			-- Next state
			if (in8) = '1' then
				state_next(20) <= '1';
				-- Next values for buffered outputs
			else
				if (not (in9)) = '1' then
					state_next(7) <= '1';
					-- Next values for buffered outputs
				else
					state_next(16) <= '1';
					-- Next values for buffered outputs
				end if;
			end if;
			-- Assignment of non-buffered outputs
			out94 <= '1';
			out93 <= '1';
			out81 <= '1';
		end if;

		if state_cur(8) = '1' then
			-- Next state
			state_next(14) <= '1';
			-- Next values for buffered outputs
			-- Assignment of non-buffered outputs
			out98 <= '1';
			out97 <= '1';
		end if;

		if state_cur(9) = '1' then
			-- Next state
			if (in10) = '1' then
				state_next(10) <= '1';
				-- Next values for buffered outputs
				out101_bufn <= '1';
			else
				state_next(5) <= '1';
				-- Next values for buffered outputs
			end if;
			-- Assignment of non-buffered outputs
		end if;

		if state_cur(10) = '1' then
			-- Next state
			state_next(11) <= '1';
			-- Next values for buffered outputs
			-- Assignment of non-buffered outputs
			out102 <= '1';
		end if;

		if state_cur(11) = '1' then
			-- Next state
			state_next(13) <= '1';
			-- Next values for buffered outputs
			out172_bufn <= '1';
			-- Assignment of non-buffered outputs
			out125 <= '1';
			out124 <= '1';
		end if;

		if state_cur(12) = '1' then
			-- Next state
			state_next(9) <= '1';
			-- Next values for buffered outputs
			-- Assignment of non-buffered outputs
			out159 <= '1';
			out157 <= '1';
			out140 <= '1';
		end if;

		if state_cur(13) = '1' then
			-- Next state
			if (in8) = '1' then
				state_next(20) <= '1';
				-- Next values for buffered outputs
			else
				state_next(9) <= '1';
				-- Next values for buffered outputs
			end if;
			-- Assignment of non-buffered outputs
			out173 <= '1';
			out171 <= '1';
			out160 <= '1';
			out81 <= '1';
		end if;

		if state_cur(14) = '1' then
			-- Next state
			if (in11) = '1' then
				state_next(15) <= '1';
				-- Next values for buffered outputs
				out172_bufn <= '1';
				out101_bufn <= '1';
			else
				state_next(12) <= '1';
				-- Next values for buffered outputs
			end if;
			-- Assignment of non-buffered outputs
		end if;

		if state_cur(15) = '1' then
			-- Next state
			if (in8) = '1' then
				state_next(20) <= '1';
				-- Next values for buffered outputs
			else
				state_next(14) <= '1';
				-- Next values for buffered outputs
			end if;
			-- Assignment of non-buffered outputs
			out173 <= '1';
			out178 <= '1';
			out175 <= '1';
			out81 <= '1';
		end if;

		if state_cur(16) = '1' then
			-- Next state
			state_next(8) <= '1';
			-- Next values for buffered outputs
			-- Assignment of non-buffered outputs
			out159 <= '1';
			out227 <= '1';
			out225 <= '1';
		end if;

		if state_cur(17) = '1' then
			-- Next state
			if (in12) = '1' then
				if (in4) = '1' then
					state_next(5) <= '1';
					-- Next values for buffered outputs
				else
					if (in3) = '1' then
						state_next(6) <= '1';
						-- Next values for buffered outputs
					else
						if (in2) = '1' then
							state_next(7) <= '1';
							-- Next values for buffered outputs
						else
							if (in1) = '1' then
								state_next(13) <= '1';
								-- Next values for buffered outputs
								out172_bufn <= '1';
							else
								state_next(15) <= '1';
								-- Next values for buffered outputs
								out172_bufn <= '1';
								out101_bufn <= '1';
							end if;
						end if;
					end if;
				end if;
			else
				state_next(18) <= '1';
				-- Next values for buffered outputs
			end if;
			-- Assignment of non-buffered outputs
			out231 <= '1';
		end if;

		if state_cur(18) = '1' then
			-- Next state
			state_next(18) <= '1';
			-- Next values for buffered outputs
			-- Assignment of non-buffered outputs
		end if;

		-- Info: This is the init/reset state
		if state_cur(19) = '1' then
			-- Next state
			if (not (in13)) = '1' then
				state_next(19) <= '1';
				-- Next values for buffered outputs
			else
				if (in12) = '1' then
					state_next(20) <= '1';
					-- Next values for buffered outputs
				else
					state_next(6) <= '1';
					-- Next values for buffered outputs
				end if;
			end if;
			-- Assignment of non-buffered outputs
			out236 <= '1';
			out235 <= '1';
		end if;

		if state_cur(20) = '1' then
			-- Next state
			state_next(21) <= '1';
			-- Next values for buffered outputs
			-- Assignment of non-buffered outputs
			out44 <= '1';
		end if;

		if state_cur(21) = '1' then
			-- Next state
			if (in14) = '1' then
				state_next(21) <= '1';
				-- Next values for buffered outputs
			else
				state_next(0) <= '1';
				-- Next values for buffered outputs
				out0_bufn <= '1';
			end if;
			-- Assignment of non-buffered outputs
			out41 <= '1';
			out238 <= '1';
			out237 <= '1';
		end if;

		-- Reset input
		if reset = '1' then
			-- Set the reset state
			state_next <= (19 => '1', others => '0');
			-- Note: Resetting all buffers for outputs here is not necessary.
			-- It would cost hardware. They will be reset at the next clock front.
			-- Reset state: set the buffered outputs
		end if;

	end process;

	-- Assignment of buffered outputs

	out0 <= out0_buf;
	out40 <= out40_buf;
	out101 <= out101_buf;
	out172 <= out172_buf;

end architecture;

