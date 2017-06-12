--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Derived parameters
--! @author Markus Fehrenz
--! @date   2011/07/04
--!
--! @details This constants are derived from constants defined in pkg_param.
--!          In order to prevent errors, there is no user choice for these parameters.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_helper.all;


package pkg_param_derived is

	-- Calculation of constraint length.
	function calc_constraint_length return natural;

	-- Memory depth of the encoder shift register.
	constant ENCODER_MEMORY_DEPTH : natural;

	-- Number of trellis states corresponds to the nubmer of ACS units.
	constant NUMBER_TRELLIS_STATES : natural;

	-- Number of branch units for a single polynomial set
	constant NUMBER_BRANCH_UNITS : natural;

	-- Bitwidth constants are needed for type conversions
	constant BW_TRELLIS_STATES    : natural;
	constant BW_MAX_WINDOW_LENGTH : natural;
	constant BW_BRANCH_RESULT     : natural;
	constant BW_MAX_PROBABILITY   : natural;

end package pkg_param_derived;


package body pkg_param_derived is

	function calc_constraint_length return natural is
		variable v_maximum : natural := 0;
	begin

		-- Find the position of the leftmost bit in the polynomials.
		for i in NUMBER_PARITY_BITS - 1 downto 0 loop
			v_maximum := max(v_maximum, no_bits_natural(PARITY_POLYNOMIALS(i)));
		end loop;
		v_maximum := max(v_maximum, no_bits_natural(FEEDBACK_POLYNOMIAL));
		return v_maximum;
	end function calc_constraint_length;


	constant ENCODER_MEMORY_DEPTH : natural := calc_constraint_length - 1;

	constant NUMBER_TRELLIS_STATES : natural := 2 ** ENCODER_MEMORY_DEPTH;

	constant NUMBER_BRANCH_UNITS : natural := 2 ** NUMBER_PARITY_BITS;

	constant BW_TRELLIS_STATES    : natural := no_bits_natural(NUMBER_TRELLIS_STATES - 1);
	constant BW_MAX_WINDOW_LENGTH : natural := no_bits_natural(MAX_WINDOW_LENGTH - 1);
	constant BW_BRANCH_RESULT     : natural := no_bits_natural((2 ** (BW_LLR_INPUT - 1)) * NUMBER_PARITY_BITS) + 1;
	constant BW_MAX_PROBABILITY   : natural := no_bits_natural(((2 ** (BW_LLR_INPUT - 1)) * NUMBER_PARITY_BITS) * 4 * ENCODER_MEMORY_DEPTH);
end package body pkg_param_derived;
