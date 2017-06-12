--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Recursion unit for recursive code.
--! @author Markus Fehrenz
--! @date   2011/01/12
--!
--! @details The recusion handling buffers the reorder ouput and
--! calculates the correct output depending on the feedback polynomial.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;

entity recursionx is
	port(
	clk : in std_logic;
	rst : in std_logic;

	--
	-- Decoded bits input from the reordering units in std_logic
	--
	s_axis_input_tvalid : in  std_logic;
	s_axis_input_tdata  : in  std_logic;
	s_axis_input_tlast  : in  std_logic;
	s_axis_input_tready : out std_logic;

	--
	-- Output decoded bits convolved with the feedback polynomial
	--
	m_axis_output_tvalid : out std_logic;
	m_axis_output_tdata  : out std_logic;
	m_axis_output_tlast  : out std_logic;
	m_axis_output_tready : in  std_logic
	);
end entity recursionx;

architecture rtl of recursionx is
	signal recursion_sreg : unsigned(ENCODER_MEMORY_DEPTH downto 0);
	signal s_axis_input_tready_int  : std_logic;
	signal m_axis_output_tvalid_int : std_logic;

begin
	s_axis_input_tready_int <= '1' when m_axis_output_tready = '1' or m_axis_output_tvalid_int = '0' else
	                           '0';

	s_axis_input_tready  <= s_axis_input_tready_int;
	m_axis_output_tvalid <= m_axis_output_tvalid_int;

	-- Use the feedback polynomial to convolve the global path.
	pr_recursion : process(clk) is
		variable v_bit : std_logic := '0';
		variable v_recursion_state : unsigned(ENCODER_MEMORY_DEPTH downto 0);
	begin
	if rising_edge(clk) then
		if rst = '1' then
			recursion_sreg <= (others => '0');
			m_axis_output_tdata  <= '0';
			m_axis_output_tlast  <= '0';
		else
			m_axis_output_tvalid_int <= s_axis_input_tvalid;

			if s_axis_input_tvalid = '1' and s_axis_input_tready_int = '1' then

				-- move current decoded output bits into shift register and reset if last flag is valid
				if s_axis_input_tlast = '1' then
					recursion_sreg <= (others => '0');
				else
					recursion_sreg <= s_axis_input_tdata & recursion_sreg(ENCODER_MEMORY_DEPTH downto 1);
				end if;

				-- convolve with feedback polynomial with the output register.
				v_bit := '0';
				v_recursion_state := (s_axis_input_tdata & recursion_sreg(ENCODER_MEMORY_DEPTH downto 1)) and 
				                     ('1' & to_unsigned(FEEDBACK_POLYNOMIAL, ENCODER_MEMORY_DEPTH));
				for i in ENCODER_MEMORY_DEPTH downto 0 loop
					v_bit := v_bit xor v_recursion_state(i);
				end loop;
				m_axis_output_tdata <= v_bit;

				m_axis_output_tlast <= s_axis_input_tlast;
			end if;
		end if;
	end if;
	end process pr_recursion;

end architecture rtl;
