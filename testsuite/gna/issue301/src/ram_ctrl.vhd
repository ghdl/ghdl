--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Viterbi decoder RAM control
--! @author Markus Fehrenz
--! @date   2011/12/13
--!
--! @details Manage RAM behavior. Write and read data.
--! The decisions are sent to the traceback units
--! It is signaled if the data belongs to acquisition or window phase.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;
use dec_viterbi.pkg_types.all;
use dec_viterbi.pkg_components.all;


entity ram_ctrl is
	port(
	clk       : in std_logic;
	rst       : in std_logic;


	--
	-- Slave data signals, delivers the LLR parity values.
	--
	s_axis_input_tvalid : in  std_logic;
	s_axis_input_tdata  : in  std_logic_vector(NUMBER_TRELLIS_STATES - 1 downto 0);
	s_axis_input_tlast  : in  std_logic;
	s_axis_input_tready : out std_logic;


	--
	-- Master data signals for traceback units, delivers the decision vectors.
	--
	m_axis_output_tvalid       : out std_logic_vector(1 downto 0);
	m_axis_output_tdata        : out t_ram_rd_data;
	m_axis_output_tlast        : out std_logic_vector(1 downto 0);
	m_axis_output_tready       : in  std_logic_vector(1 downto 0);

	-- Signals the traceback unit when the decision bits do not belong to an acquisition.
	m_axis_output_window_tuser : out std_logic_vector(1 downto 0);

	-- Signals whether this is the last decision vector of the window.
	m_axis_output_last_tuser   : out std_logic_vector(1 downto 0);


	--
	-- Slave configuration signals, delivering the configuration data.
	--

	s_axis_ctrl_tvalid : in  std_logic;
	s_axis_ctrl_tdata  : in  std_logic_vector(31 downto 0);
	s_axis_ctrl_tready : out std_logic
);
end entity ram_ctrl;


architecture rtl of ram_ctrl is

	------------------------
	-- Type definition
	------------------------

	--
	-- Record contains runtime configuration.
	-- The input configuration is stored in a register.
	-- It is received from a AXI4-Stream interface from the top entity.
	--
	type trec_runtime_param is record
		window_length           : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
		acquisition_length      : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
	end record trec_runtime_param;

	-- Types for finite state machines
	type t_write_ram_fsm is (CONFIGURE, START, RUN, WAIT_FOR_TRACEBACK, WAIT_FOR_LAST_TRACEBACK);
	type t_read_ram_fsm is (WAIT_FOR_WINDOW, TRACEBACK, WAIT_FOR_RAM, FINISH);
	type t_read_ram_fsm_array is array (0 to 1) of t_read_ram_fsm;

	-- RAM controling types
	type t_ram_data    is array (3 downto 0) of std_logic_vector(NUMBER_TRELLIS_STATES - 1 downto 0);
	type t_ram_addr    is array (3 downto 0) of unsigned(BW_MAX_WINDOW_LENGTH - 1  downto 0);
	type t_ram_rd_addr is array (1 downto 0) of unsigned(BW_MAX_WINDOW_LENGTH - 1  downto 0);
	type t_ram_ptr     is array (1 downto 0) of unsigned(1 downto 0);
	type t_ram_ptr_int is array (1 downto 0) of integer range 3 downto 0;

	type t_ram_data_cnt is array (1 downto 0) of integer range 2 * MAX_WINDOW_LENGTH downto 0;


	------------------------
	-- Signal declaration
	------------------------

	signal ram_buffer   : t_ram_rd_data;
	signal ram_buffer_full   : std_logic_vector(1 downto 0);

	signal config          : trec_runtime_param;
	signal write_ram_fsm   : t_write_ram_fsm;
	signal read_ram_fsm    : t_read_ram_fsm_array;
	signal wen_ram         : std_logic_vector(3 downto 0);
	signal addr            : t_ram_addr;
	signal q_reg           : t_ram_data;

	-- ram addess, number and data pointer
	signal write_ram_ptr  : unsigned(1 downto 0);
	signal read_ram_ptr   : t_ram_ptr;
	signal read_ram_ptr_d : t_ram_ptr;
	signal write_addr_ptr : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
	signal read_addr_ptr  : t_ram_rd_addr;

	-- internal signals of outputs
	signal m_axis_output_tvalid_int       : std_logic_vector(1 downto 0);
	signal m_axis_output_tlast_int       : std_logic_vector(1 downto 0);
	signal m_axis_output_window_tuser_int : std_logic_vector(1 downto 0);
	signal m_axis_output_last_tuser_int   : std_logic_vector(1 downto 0);
	signal s_axis_input_tready_int        : std_logic;
	signal s_axis_ctrl_tready_int         : std_logic;

	signal next_traceback : std_logic_vector(1 downto 0);
	signal write_window_complete : std_logic;
	signal write_last_window_complete : std_logic;
	signal last_of_block : std_logic;
	signal read_last_addr_ptr : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
begin

	m_axis_output_tvalid       <= m_axis_output_tvalid_int;
	m_axis_output_tlast        <= m_axis_output_tlast_int;
	m_axis_output_window_tuser <= m_axis_output_window_tuser_int;
	m_axis_output_last_tuser   <= m_axis_output_last_tuser_int;
	m_axis_output_tdata(0)     <= q_reg(to_integer(read_ram_ptr_d(0))) when ram_buffer_full(0) = '0' else ram_buffer(0);
	m_axis_output_tdata(1)     <= q_reg(to_integer(read_ram_ptr_d(1))) when ram_buffer_full(1) = '0' else ram_buffer(1);


	--
	-- When the output port is not ready to read the output of the RAM immediately
	-- we have to remember the output value of the RAM in an extra register.
	-- When the output is ready to read, we first use the ouput of the register
	-- and only then the output of the RAM again.
	--
	pr_buf_ram_output: process(clk) is
	begin
	if rising_edge(clk) then
		if rst = '1' then
			ram_buffer <= (others => (others => '0'));
			ram_buffer_full <= (others => '0');
		else

			for i in 0 to 1 loop
				if m_axis_output_tvalid_int(i) = '1' and m_axis_output_tready(i) = '0' and ram_buffer_full(i) = '0' then
					ram_buffer(i) <=  q_reg(to_integer(read_ram_ptr_d(i)));
					ram_buffer_full(i) <= '1';
				end if;

				if m_axis_output_tvalid_int(i) = '1' and m_axis_output_tready(i) = '1' and ram_buffer_full(i) = '1' then
					ram_buffer_full(i) <= '0';
				end if;
			end loop;

		end if;
	end if;
	end process pr_buf_ram_output;

	-----------------------------
	-- Manage writing from ACS --
	-----------------------------
	s_axis_input_tready_int <= '0' when (write_ram_fsm = CONFIGURE) or
	                           (write_ram_ptr = read_ram_ptr(0) and read_ram_fsm(0) /= WAIT_FOR_WINDOW) or
	                           (write_ram_ptr = read_ram_ptr(1) and read_ram_fsm(1) /= WAIT_FOR_WINDOW) or
	                            write_ram_fsm = WAIT_FOR_TRACEBACK or write_ram_fsm = WAIT_FOR_LAST_TRACEBACK else
	                           '1';
	s_axis_input_tready <= s_axis_input_tready_int;

	s_axis_ctrl_tready_int <= '1' when (read_ram_fsm(0) = WAIT_FOR_WINDOW and read_ram_fsm(1) = WAIT_FOR_WINDOW and write_ram_fsm = CONFIGURE) else
	                          '0';
	s_axis_ctrl_tready <= s_axis_ctrl_tready_int;

	-- Process for writing to the RAM
	pr_write_ram: process(clk) is
		variable v_window_length      : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
		variable v_acquisition_length : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
	begin
	if rising_edge(clk) then
		if rst = '1' then
			write_ram_fsm         <= CONFIGURE;
			write_addr_ptr        <= (others => '0');
			write_ram_ptr         <= (others => '0');
			wen_ram               <= (others => '0');
			write_window_complete <= '0';
			write_last_window_complete <= '0';
			read_last_addr_ptr    <= (others => '0');
		else

			case write_ram_fsm is

			--
			-- It is necessary to configure the decoder before each block
			--
			when CONFIGURE =>
				write_window_complete <= '0';
				write_last_window_complete <= '0';
				if s_axis_ctrl_tvalid = '1' and s_axis_ctrl_tready_int = '1' then
					v_window_length           := unsigned(s_axis_ctrl_tdata(BW_MAX_WINDOW_LENGTH - 1 + 16 downto 16));
					v_acquisition_length      := unsigned(s_axis_ctrl_tdata(BW_MAX_WINDOW_LENGTH - 1      downto  0));
					write_addr_ptr            <= v_window_length - v_acquisition_length;
					config.window_length      <= v_window_length;
					config.acquisition_length <= v_acquisition_length;
					write_ram_fsm             <= START;

					wen_ram(to_integer(write_ram_ptr)) <= '1';
				end if;


			--
			-- After the decoder is configured, the decoder is waiting for a new block.
			-- When the AXIS handshake is there the packet transmission begins.
			-- The first write is a special case, since writing data starts at the acquisition length.
			-- There is no complete window available afterwards.
			--
			when START =>
				if s_axis_input_tvalid = '1' and s_axis_input_tready_int = '1' then

					if write_addr_ptr = config.window_length - 1 then

						-- When we switch to the next RAM, we reset the write addr.
						write_addr_ptr <= (others => '0');

						-- Switch to the next RAM.
						write_ram_ptr                          <= write_ram_ptr + 1;
						wen_ram(to_integer(write_ram_ptr))     <= '0';
						wen_ram(to_integer(write_ram_ptr + 1)) <= '1';

						write_ram_fsm  <= RUN;
					else
						write_addr_ptr <= write_addr_ptr + 1;
					end if;
				end if;


			--
			-- The decoder is receiving data from the ACS.
			--
			when RUN =>
				write_window_complete <= '0';
				write_last_window_complete <= '0';

				if s_axis_input_tvalid = '1' and s_axis_input_tready_int = '1' then
					write_addr_ptr <= write_addr_ptr + 1;

					if write_addr_ptr = config.window_length - 1 then

						-- When we switch to the next RAM, we reset the write addr.
						write_addr_ptr <= (others => '0');

						-- Switch to the next RAM.
						write_ram_ptr                          <= write_ram_ptr + 1;
						wen_ram(to_integer(write_ram_ptr))     <= '0';
						wen_ram(to_integer(write_ram_ptr + 1)) <= '1';

						-- Indicate, that a complete window is within the RAM and traceback may start.
						write_window_complete <= '1';

						if read_ram_fsm(0) /= WAIT_FOR_WINDOW and read_ram_fsm(1) /= WAIT_FOR_WINDOW then
							write_ram_fsm <= WAIT_FOR_TRACEBACK;
						end if;

					else
						write_addr_ptr <= write_addr_ptr + 1;
					end if;

					if s_axis_input_tlast = '1' then
						write_ram_fsm <= CONFIGURE;
						wen_ram       <= (others => '0');

						write_last_window_complete <= '1';
						if (read_ram_fsm(0) /= WAIT_FOR_WINDOW and read_ram_fsm(1) /= WAIT_FOR_WINDOW) or write_window_complete = '1' then
							write_ram_fsm <= WAIT_FOR_LAST_TRACEBACK;
						end if;
						read_last_addr_ptr <= write_addr_ptr;

						write_addr_ptr <= (others => '0');
						write_ram_ptr                          <= write_ram_ptr + 1;
					end if;
				end if;

			when WAIT_FOR_TRACEBACK =>
				if read_ram_fsm(0) = WAIT_FOR_WINDOW or read_ram_fsm(1) = WAIT_FOR_WINDOW then
					write_ram_fsm <= RUN;
					write_window_complete <= '0';
				end if;

			when WAIT_FOR_LAST_TRACEBACK =>
				if read_ram_fsm(0) = WAIT_FOR_WINDOW or read_ram_fsm(1) = WAIT_FOR_WINDOW then
					write_ram_fsm <= CONFIGURE;
					write_last_window_complete <= '0';
				end if;

			end case;
		end if;
	end if;
	end process pr_write_ram;


	-------------------------------------------
	-- Manage reading from RAM for traceback --
	-------------------------------------------

	gen_read_ram: for i in 0 to 1 generate
		pr_read_ram: process(clk) is
		begin
		if rising_edge(clk) then
			if rst = '1' then
				read_addr_ptr(i) <= (others => '0');
				read_ram_fsm(i)  <= WAIT_FOR_WINDOW;
				m_axis_output_tvalid_int(i)       <= '0';
				m_axis_output_tlast_int(i)        <= '0';
				m_axis_output_window_tuser_int(i) <= '0';
				m_axis_output_last_tuser_int(i)   <= '0';
				read_ram_ptr(i)                   <= (others => '0');
				read_ram_ptr_d(i)                 <= (others => '0');
			else

				read_ram_ptr_d(i) <= read_ram_ptr(i);
				case read_ram_fsm(i) is

				-- Wait for the next window to be ready within the RAM.
				when WAIT_FOR_WINDOW =>
					read_addr_ptr(i) <= config.window_length - 1;
					m_axis_output_tlast_int(i) <= '0';
					m_axis_output_tvalid_int(i) <= '0';
					m_axis_output_last_tuser_int(i) <= '0';
					m_axis_output_window_tuser_int(i) <= '0';
					read_ram_ptr(i)   <= write_ram_ptr;

					-- We always start from the RAM, which was written last.
					if write_window_complete = '1' and next_traceback(i) = '1' then
						read_ram_ptr(i)   <= write_ram_ptr - 1;
						read_addr_ptr(i)            <= read_addr_ptr(i) - 1;
						read_ram_fsm(i)             <= TRACEBACK;
						m_axis_output_tvalid_int(i) <= '1';
					end if;
					if write_last_window_complete = '1' and next_traceback(i) = '1' then
						read_ram_ptr(i)   <= write_ram_ptr - 1;
						read_addr_ptr(i)            <= read_last_addr_ptr;
						read_ram_fsm(i)             <= TRACEBACK;
						m_axis_output_window_tuser_int(i) <= '1';
					end if;

				-- Perform the Traceback on the RAM data of the first RAM we need for acquisition and traceback.
				when TRACEBACK =>
					m_axis_output_tlast_int(i) <= '0';
					m_axis_output_last_tuser_int(i) <= '0';
					m_axis_output_tvalid_int(i) <= '1';

					if m_axis_output_tready(i) = '1' then
						if read_addr_ptr(i) = 0 then
							if read_ram_fsm(1 - i) = TRACEBACK and read_ram_ptr(1 - i) = read_ram_ptr(i) - 1 then
								read_ram_fsm(i) <= WAIT_FOR_RAM;
							else
								read_addr_ptr(i) <= config.window_length - 1;
								read_ram_ptr(i)  <= read_ram_ptr(i) - 1;
								read_ram_fsm(i)  <= FINISH;
							end if;
						else
							read_addr_ptr(i) <= read_addr_ptr(i) - 1;
						end if;

						-- Signal the traceback unit, acquisition is over.
						if read_addr_ptr(i) = config.window_length - config.acquisition_length - 1 then
							m_axis_output_window_tuser_int(i) <= '1';
						end if;
					end if;

				when WAIT_FOR_RAM =>
					m_axis_output_tvalid_int(i) <= '0';
					if read_ram_fsm(1 - i) /= TRACEBACK or read_ram_ptr(1 - i) /= read_ram_ptr(i) - 1 then
						read_addr_ptr(i) <= config.window_length - 1;
						read_ram_ptr(i)  <= read_ram_ptr(i) - 1;
						read_ram_fsm(i)  <= FINISH;
					end if;

				-- Get the remaining values from the second RAM we need for traceback (no acquisition values in this RAM)
				when FINISH =>
					if m_axis_output_tvalid_int(i) <= '0' then
						m_axis_output_tvalid_int(i) <= '1';
						read_addr_ptr(i) <= read_addr_ptr(i) - 1;
					end if;
					if m_axis_output_tready(i) = '1' then

						if read_addr_ptr(i) = config.window_length - config.acquisition_length then
							m_axis_output_last_tuser_int(i) <= '1';
							read_addr_ptr(i)        <= config.window_length - 1;
							read_ram_fsm(i)         <= WAIT_FOR_WINDOW;

							-- Check if the other read process finished processing.
							if read_ram_fsm((i+1) mod 2) = WAIT_FOR_WINDOW and last_of_block = '1' then
								m_axis_output_tlast_int(i) <= '1';
							end if;

						else
							read_addr_ptr(i) <= read_addr_ptr(i) - 1;
						end if;
					end if;
				end case;
			end if;
		end if;
		end process pr_read_ram;
	end generate gen_read_ram;

	-- This process decides which traceback unit is the next one to use.
	pr_next_traceback: process(clk) is
	begin
	if rising_edge(clk) then
		if rst = '1' then
			next_traceback <= "01";
			last_of_block  <= '0';
		else
			if write_window_complete = '1' then
				if next_traceback(0) = '1' then
					next_traceback(0) <= '0';
					next_traceback(1) <= '1';
				else
					next_traceback(0) <= '1';
					next_traceback(1) <= '0';
				end if;
			end if;

			if s_axis_input_tlast = '1' then
				last_of_block <= '1';
			end if;

		end if;
	end if;
	end process pr_next_traceback;

	------------------------------
	--- Portmapping components ---
	------------------------------

	gen_generic_sp_ram : for i in 0 to 3 generate
	begin

	addr(i) <= write_addr_ptr   when (write_ram_fsm = RUN or write_ram_fsm = START) and to_integer(write_ram_ptr) = i else
	           read_addr_ptr(0) when (to_integer(read_ram_ptr(0)) = i and (read_ram_fsm(0) = TRACEBACK or read_ram_fsm(0) = WAIT_FOR_RAM or read_ram_fsm(0) = FINISH)) or
	                                 (next_traceback(0) = '1' and write_window_complete = '1' and to_integer(read_ram_ptr(0)) = i) else
	           read_addr_ptr(1);

	inst_generic_sp_ram : generic_sp_ram
	generic map(
		DISTR_RAM => DISTRIBUTED_RAM,
		WORDS     => MAX_WINDOW_LENGTH,
		BITWIDTH  => NUMBER_TRELLIS_STATES
	)
	port map(
		clk => clk,
		rst => rst,
		wen => wen_ram(i),
		en  => '1',
		a   => std_logic_vector(addr(i)),
		d   => s_axis_input_tdata,
		q   => q_reg(i)
	);
	end generate gen_generic_sp_ram;

end architecture rtl;
