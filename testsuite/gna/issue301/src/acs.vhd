--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Add-compare-select unit for trellis processing.
--! @author Markus Fehrenz
--! @date   2011/07/04
--!
--! @details The ACS decides which path is the the surviving trellis path.
--! In the design there are 2^{K-1} ACS instances.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;
use dec_viterbi.pkg_types.all;
use dec_viterbi.pkg_helper.all;


entity acs is
	generic(

		-- Reset value
		INITIALIZE_VALUE : in signed(BW_MAX_PROBABILITY - 1 downto 0)
	);
	port(
		clk : in std_logic;
		rst : in std_logic;

		--
		-- Values from branch distance, signed values in std_logic_vector
		-- high is located in the upper half.
		--
		s_axis_inbranch_tvalid     : in  std_logic;
		s_axis_inbranch_tdata_low  : in  std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);
		s_axis_inbranch_tdata_high : in  std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);
		s_axis_inbranch_tlast      : in  std_logic;
		s_axis_inbranch_tready     : out std_logic;

		--
		-- Probabilities from previous nodes, signed values in std_logic_vector
		-- high is located in the upper half.
		--
		s_axis_inprev_tvalid     : in  std_logic;
		s_axis_inprev_tdata_low  : in  std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
		s_axis_inprev_tdata_high : in  std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
		s_axis_inprev_tready     : out std_logic;

		-- probability result of the add compare and select
		m_axis_outprob_tvalid  : out std_logic;
		m_axis_outprob_tdata   : out std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
		m_axis_outprob_tready  : in  std_logic;

		-- decision result of the add compare and select
		m_axis_outdec_tvalid   : out std_logic;
		m_axis_outdec_tdata    : out std_logic;
		m_axis_outdec_tlast    : out std_logic;
		m_axis_outdec_tready   : in  std_logic
	);
end entity acs;


architecture rtl of acs is

	signal s_axis_inbranch_tlast_d : std_logic;
	signal m_axis_outdec_tvalid_int : std_logic;
	signal s_axis_inbranch_tready_int : std_logic;

begin

	s_axis_inbranch_tready_int <= '1' when m_axis_outdec_tready = '1' or m_axis_outdec_tvalid_int = '0' else
	                              '0';
	s_axis_inbranch_tready <= s_axis_inbranch_tready_int;
	m_axis_outdec_tvalid   <= m_axis_outdec_tvalid_int;

	-- Add branch to previous, compare both paths and select survivor.
	pr_add_compare : process(clk) is
		variable v_diff, v_high, v_low : signed(BW_MAX_PROBABILITY - 1 downto 0);
	begin
	if rising_edge(clk) then
		if rst = '1' then
			m_axis_outdec_tvalid_int <= '0';
			m_axis_outdec_tdata      <= '0';
			m_axis_outdec_tlast      <= '0';
			m_axis_outprob_tvalid    <= '0';
			s_axis_inprev_tready     <= '0';
			s_axis_inbranch_tlast_d  <= '0';
			m_axis_outprob_tdata     <= std_logic_vector(INITIALIZE_VALUE);
		else
			-- If this is the last value, prepare for processing of next incoming value.
			if s_axis_inbranch_tlast_d = '1' then
				m_axis_outprob_tdata     <= std_logic_vector(INITIALIZE_VALUE);
				s_axis_inbranch_tlast_d  <= '0';
				m_axis_outdec_tvalid_int <= '0';
			end if;
			if m_axis_outdec_tvalid_int = '1' and m_axis_outdec_tready = '1' then
				m_axis_outdec_tvalid_int <= '0';
			end if;

			-- Process only if we receive valid data.
			if s_axis_inbranch_tvalid = '1' and s_axis_inbranch_tready_int = '1' then
				s_axis_inbranch_tlast_d <= s_axis_inbranch_tlast;

				-- Add.
				v_low  := signed(s_axis_inbranch_tdata_low)  + signed(s_axis_inprev_tdata_low);
				v_high := signed(s_axis_inbranch_tdata_high) + signed(s_axis_inprev_tdata_high);

				-- Use modulo normalization, do not extend the sign here!
				v_diff := v_low - v_high;

				-- Compare, select the correct path.
				if v_diff < 0 then
					m_axis_outdec_tdata  <= '1';
					m_axis_outprob_tdata <= std_logic_vector(v_high);
				else
					m_axis_outdec_tdata  <= '0';
					m_axis_outprob_tdata <= std_logic_vector(v_low);
				end if;
				m_axis_outdec_tvalid_int <= '1';
			end if;

			m_axis_outdec_tlast    <= s_axis_inbranch_tlast;
		end if;
	end if;
	end process pr_add_compare;

end architecture rtl;
