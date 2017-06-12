--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Component declarations for Viterbi decoder
--! @author Markus Fehrenz
--! @date   2011/04/07
--!
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;
use dec_viterbi.pkg_types.all;

package pkg_components is

	component axi4s_buffer is
		generic (
			DATA_WIDTH : natural := 1
		);
		port (
		clk            : in  std_logic;
		rst            : in  std_logic;

		input          : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
		input_valid    : in  std_logic;
		input_last     : in  std_logic;
		input_accept   : out std_logic;

		output         : out std_logic_vector(DATA_WIDTH - 1 downto 0);
		output_valid   : out std_logic;
		output_last    : out std_logic;
		output_accept  : in  std_logic
		);
	end component axi4s_buffer;

	component branch_distance is
		generic(
			EDGE_WEIGHT : in std_logic_vector(NUMBER_PARITY_BITS - 1 downto 0)
		);
		port(
			clk : in std_logic;
			rst : in std_logic;

			s_axis_input_tvalid : in  std_logic;
			s_axis_input_tdata  : in  t_input_block;
			s_axis_input_tlast  : in  std_logic;
			s_axis_input_tready : out std_logic;
	
			m_axis_output_tvalid : out std_logic;
			m_axis_output_tdata  : out std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);
			m_axis_output_tlast  : out std_logic;
			m_axis_output_tready : in  std_logic

		);
	end component branch_distance;

	component acs is
		generic(
			initialize_value : in signed(BW_MAX_PROBABILITY - 1 downto 0)
		);
		port(
			clk                    : in std_logic;
			rst                    : in std_logic;

			s_axis_inbranch_tvalid     : in  std_logic;
			s_axis_inbranch_tdata_low  : in  std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);
			s_axis_inbranch_tdata_high : in  std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);
			s_axis_inbranch_tlast      : in  std_logic;
			s_axis_inbranch_tready     : out std_logic;
	
			s_axis_inprev_tvalid     : in  std_logic;
			s_axis_inprev_tdata_low  : in  std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
			s_axis_inprev_tdata_high : in  std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
			s_axis_inprev_tready     : out std_logic;
	
			m_axis_outprob_tvalid  : out std_logic;
			m_axis_outprob_tdata   : out std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
			m_axis_outprob_tready  : in  std_logic;
	
			m_axis_outdec_tvalid   : out std_logic;
			m_axis_outdec_tdata    : out std_logic;
			m_axis_outdec_tlast    : out std_logic;
			m_axis_outdec_tready   : in  std_logic
		);
	end component acs;

	component ram_ctrl is
		port(
			clk       : in std_logic;
			rst       : in std_logic;
		
			s_axis_input_tvalid : in  std_logic;
			s_axis_input_tdata  : in  std_logic_vector(NUMBER_TRELLIS_STATES - 1 downto 0);
			s_axis_input_tlast  : in  std_logic;
			s_axis_input_tready : out std_logic;

			m_axis_output_tvalid       : out std_logic_vector(1 downto 0);
			m_axis_output_tdata        : out t_ram_rd_data;
			m_axis_output_tlast        : out std_logic_vector(1 downto 0);
			m_axis_output_tready       : in  std_logic_vector(1 downto 0);
			m_axis_output_window_tuser : out std_logic_vector(1 downto 0);
			m_axis_output_last_tuser   : out std_logic_vector(1 downto 0);

			s_axis_ctrl_tvalid : in  std_logic;
			s_axis_ctrl_tdata  : in  std_logic_vector(31 downto 0);
			s_axis_ctrl_tready : out std_logic
		);
	end component ram_ctrl;

	component generic_sp_ram is
		generic(
			DISTR_RAM : boolean;
			WORDS     : integer;
			BITWIDTH  : integer
		);
		port(
			clk : in std_logic;
			rst : in std_logic;
	
			wen : in std_logic;
			en  : in std_logic;
	
			a   : in std_logic_vector(BW_MAX_WINDOW_LENGTH - 1 downto 0);
			d   : in  std_logic_vector(BITWIDTH - 1 downto 0 );
			q   : out std_logic_vector(BITWIDTH - 1 downto 0)
		);
	end component generic_sp_ram;

	component trellis_traceback is
		port(
			clk : in std_logic;
			rst : in std_logic;

			s_axis_input_tvalid       : in std_logic;
			s_axis_input_tdata        : in std_logic_vector(NUMBER_TRELLIS_STATES - 1 downto 0);
			s_axis_input_tlast        : in std_logic;
			s_axis_input_tready       : out std_logic;
			s_axis_input_window_tuser : in std_logic;
			s_axis_input_last_tuser   : in std_logic;

			m_axis_output_tvalid     : out std_logic;
			m_axis_output_tdata      : out std_logic;
			m_axis_output_tlast      : out std_logic;
			m_axis_output_last_tuser : out std_logic;
			m_axis_output_tready     : in  std_logic
		);
	end component trellis_traceback;

	component reorder is
		port(
			clk : in std_logic;
			rst : in std_logic;
	
			s_axis_input_tvalid     : in  std_logic;
			s_axis_input_tdata      : in  std_logic;
			s_axis_input_tlast      : in  std_logic;
			s_axis_input_last_tuser : in  std_logic;
			s_axis_input_tready     : out std_logic;
		
			m_axis_output_tvalid     : out std_logic;
			m_axis_output_tdata      : out std_logic;
			m_axis_output_tlast      : out std_logic;
			m_axis_output_last_tuser : out std_logic;
			m_axis_output_tready     : in  std_logic
		);
	end component reorder;

	component recursionx is
		port(
			clk : in std_logic;
			rst : in std_logic;
	
			s_axis_input_tvalid     : in  std_logic;
			s_axis_input_tdata      : in  std_logic;
			s_axis_input_tlast      : in  std_logic;
			s_axis_input_tready     : out std_logic;
		
			m_axis_output_tvalid     : out std_logic;
			m_axis_output_tdata      : out std_logic;
			m_axis_output_tlast      : out std_logic;
			m_axis_output_tready     : in  std_logic
		);
	end component recursionx;

end package pkg_components;
