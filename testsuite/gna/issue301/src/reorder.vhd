--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Reorder twisted output due to windowing
--! @author Markus Fehrenz
--! @date   2011/05/12
--!
--! @details The windowing output is twisted.
--!          The correct order is simply rebuilt by reversing
--!          the output of each traceback unit.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;
use dec_viterbi.pkg_types.all;


entity reorder is
	port(
		clk :   in std_logic;
		rst :   in std_logic;

		--
		-- Traceback unit output, twisted order
		--
		s_axis_input_tvalid     : in  std_logic;
		s_axis_input_tdata      : in  std_logic;
		s_axis_input_tlast      : in  std_logic;
		s_axis_input_last_tuser : in  std_logic;
		s_axis_input_tready     : out std_logic;

		--
		-- Viterbi decoder output, original (input) order.
		--
		m_axis_output_tvalid     : out std_logic;
		m_axis_output_tdata      : out std_logic;
		m_axis_output_tlast      : out std_logic;
		m_axis_output_last_tuser : out std_logic; -- Last bit of one traceback window
		m_axis_output_tready     : in  std_logic
	);
end entity reorder;


architecture rtl of reorder is

	-- used to store one reversed output of a traceback unit
	signal buffer_sreg              : unsigned(MAX_WINDOW_LENGTH - 1 downto 0);
	signal buffer_cnt               : unsigned(BW_MAX_WINDOW_LENGTH - 1 downto 0);
	signal buffer_end               : integer range ENCODER_MEMORY_DEPTH downto 0;
	signal send_output, last_window : boolean;

	signal s_axis_input_tready_int  : std_logic;

begin

	s_axis_input_tready     <= s_axis_input_tready_int;
	s_axis_input_tready_int <= '1' when not(send_output) else
	                           '0';

-- 	m_axis_output_tvalid     <= '1' when send_output and m_axis_output_tready= '1' else
	m_axis_output_tvalid     <= '1' when send_output else
	                            '0';
	m_axis_output_tdata      <= buffer_sreg(0);

	m_axis_output_tlast      <= '1' when buffer_cnt = ENCODER_MEMORY_DEPTH  and last_window else
	                            '0';

	-- Reorder the global path given from an traceback unit with the help of a shift register.
	pr_reorder : process(clk) is
	begin
	if rising_edge(clk) then
		if rst = '1' then
			buffer_sreg              <= (others => '0');
			buffer_cnt               <= (others => '0');
			send_output              <= false;
			last_window              <= false;
			buffer_end               <= 0;
			m_axis_output_last_tuser <= '0';
		else

			-- store output of traceback unit
			if s_axis_input_tvalid = '1' and s_axis_input_tready_int = '1' then
				if s_axis_input_tlast = '1' then
					last_window <= true;
					buffer_end <= ENCODER_MEMORY_DEPTH;
				end if;
				if s_axis_input_last_tuser = '1' then
					send_output <= true;
					buffer_sreg <= buffer_sreg(MAX_WINDOW_LENGTH - 2 downto 0) & s_axis_input_tdata;
				else
					buffer_sreg <= buffer_sreg(MAX_WINDOW_LENGTH - 2 downto 0) & s_axis_input_tdata;
					buffer_cnt  <= buffer_cnt + 1;
				end if;
			end if;

			-- send reordered data to the output
			if m_axis_output_tready = '1' and send_output then
				buffer_sreg <= '0' & buffer_sreg(MAX_WINDOW_LENGTH - 1 downto 1);

				-- Next transfer will be the last one of this window.
				if buffer_cnt = 1 then
					m_axis_output_last_tuser <= '1';
				end if;

				-- This was the last data transfer. Tailbits are cut off
				if buffer_cnt = buffer_end then
					send_output              <= false;
					last_window              <= false;
					buffer_end               <= 0;
					buffer_cnt               <= (others => '0');
					m_axis_output_last_tuser <= '0';
				else
					buffer_cnt <= buffer_cnt - 1;
				end if;
			end if;
		end if;
	end if;
	end process pr_reorder;

end architecture rtl;
