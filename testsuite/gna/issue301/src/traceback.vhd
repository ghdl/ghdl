--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Traceback unit for a viterbi decoder
--! @author Markus Fehrenz
--! @date   2011/07/11
--!
--! @details The traceback unit only processes a data stream.
--! There is no knowledge about the decoder configuration.
--! The information about acquisition and window lengths is received from ram control.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;


entity trellis_traceback is
	port(
		-- general signals
		clk : in std_logic;
		rst : in std_logic;

		s_axis_input_tvalid       : in  std_logic;
		s_axis_input_tdata        : in  std_logic_vector(NUMBER_TRELLIS_STATES - 1 downto 0);
		s_axis_input_tlast        : in  std_logic;
		s_axis_input_tready       : out std_logic;
		s_axis_input_window_tuser : in  std_logic;
		s_axis_input_last_tuser   : in  std_logic;

		m_axis_output_tvalid     : out std_logic;
		m_axis_output_tdata      : out std_logic;
		m_axis_output_tlast      : out std_logic;
		m_axis_output_last_tuser : out std_logic;
		m_axis_output_tready     : in  std_logic
	);
end entity trellis_traceback;


architecture rtl of trellis_traceback is

	signal current_node               : unsigned(BW_TRELLIS_STATES - 1 downto 0);
	signal m_axis_output_tvalid_int   : std_logic;
	signal s_axis_input_tready_int    : std_logic;

begin
	s_axis_input_tready_int <= '1' when m_axis_output_tready = '1' or m_axis_output_tvalid_int = '0' else
	                           '0';
	s_axis_input_tready <= s_axis_input_tready_int;

	m_axis_output_tvalid <= m_axis_output_tvalid_int;


	-- Traceback the ACS local path decisions and output the resulting global path.
	pr_traceback : process(clk) is
	begin
	if rising_edge(clk) then
		if rst = '1' then
			m_axis_output_tvalid_int   <= '0';
			m_axis_output_tdata        <= '0';
			m_axis_output_tlast        <= '0';
			m_axis_output_last_tuser   <= '0';
			current_node               <= (others => '0');
		else

			if m_axis_output_tready = '1' then	
				m_axis_output_tvalid_int <= '0';
			end if;

			-- calculate the decoded bit with an shift register
			if s_axis_input_tvalid = '1' and s_axis_input_tready_int = '1' then

				m_axis_output_tlast      <= s_axis_input_tlast;
				m_axis_output_last_tuser <= s_axis_input_last_tuser;

				-- handle tvalid output signal
				if s_axis_input_window_tuser = '1' then
					m_axis_output_tvalid_int <= '1';
					m_axis_output_tdata <= current_node(BW_TRELLIS_STATES - 1);
				end if;

				-- last value of current window?
				if s_axis_input_last_tuser = '1' then
					current_node <= to_unsigned(0, BW_TRELLIS_STATES);
				else
					current_node <= current_node(BW_TRELLIS_STATES - 2 downto 0)
					                & s_axis_input_tdata(to_integer(current_node(BW_TRELLIS_STATES - 1 downto 0)));
				end if;
			end if;
		end if;
	end if;
	end process pr_traceback;
end architecture rtl;
