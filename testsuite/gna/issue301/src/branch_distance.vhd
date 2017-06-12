--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Branch distance calculation unit.
--! @author Markus Fehrenz
--! @date   2011/08/04
--!
--! @details Each branch has to be calculated only once.
--!          The branch calculations are configured with a generic.
--!          There is no limitation in branch calculations.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;
use dec_viterbi.pkg_types.all;


entity branch_distance is
	generic(
		EDGE_WEIGHT : in std_logic_vector(0 to NUMBER_PARITY_BITS - 1)
	);
	port(
		clk                 : in std_logic;
		rst                 : in std_logic;

		--
		-- Input LLR values
		--
		s_axis_input_tvalid : in  std_logic;
		s_axis_input_tdata  : in  t_input_block;
		s_axis_input_tlast  : in  std_logic;
		s_axis_input_tready : out std_logic;


		--
		-- Output branch metrics, going to ACS unit.
		--
		m_axis_output_tvalid : out std_logic;
		m_axis_output_tdata  : out std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);
		m_axis_output_tlast  : out std_logic;
		m_axis_output_tready : in  std_logic
	);
end entity branch_distance;


architecture rtl of branch_distance is

	signal m_axis_output_tvalid_int : std_logic;
	signal s_axis_input_tready_int  : std_logic;

begin

	-- We are ready, when we are allowed to write to the output, or the output is idle.
	s_axis_input_tready_int <= '1' when m_axis_output_tready = '1' else
	                           '0';

	-- Connect internal versions of signal to output port.
	s_axis_input_tready   <= s_axis_input_tready_int;
	m_axis_output_tvalid  <= m_axis_output_tvalid_int;


	-- Calculation of specific branch distance with a geometric distance function.
	pr_branch : process(clk) is
		variable v_branch_result : integer;
	begin
	if rising_edge(clk) then
		if rst = '1' then
			m_axis_output_tvalid_int <= '0';
			m_axis_output_tdata      <= (others => '0');
			m_axis_output_tlast      <= '0';
		else

			if m_axis_output_tvalid_int = '1' and m_axis_output_tready = '1' then
				m_axis_output_tvalid_int <= '0';
				m_axis_output_tlast      <= '0';
			end if;

			if s_axis_input_tready_int = '1' and s_axis_input_tvalid = '1' then
				v_branch_result := 0;

				for i in NUMBER_PARITY_BITS - 1 downto 0 loop

					--
					-- Either the value is added or subtracted, depending on
					-- the current branch metric we are computing.
					--
					if EDGE_WEIGHT(i) = '0' then
						v_branch_result := v_branch_result + to_integer(s_axis_input_tdata(i));
					else
						v_branch_result := v_branch_result - to_integer(s_axis_input_tdata(i));
					end if;
				end loop;
				m_axis_output_tdata <= std_logic_vector(to_signed(v_branch_result, BW_BRANCH_RESULT));
				m_axis_output_tvalid_int <= '1';
				m_axis_output_tlast      <= s_axis_input_tlast;
			end if;

		end if;
	end if;
	end process pr_branch;

end architecture rtl;
