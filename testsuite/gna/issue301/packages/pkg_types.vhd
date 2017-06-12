--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Global types for the Viterbi decoder
--! @author Markus Fehrenz
--! @date   2011/07/04
--!
--! @details Most types are shared and used in different context.
--!          Changing single types should be done with adding an additional type.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;

package pkg_types is

	-- Parity structure: p1_bit, p2_bit, ..., pN_bit
	type t_input_block is array (NUMBER_PARITY_BITS - 1 downto 0) of signed(BW_LLR_INPUT - 1 downto 0);

	-- Types are used for bulk information to ACS and branch unit.
	type t_node_s is array (NUMBER_TRELLIS_STATES - 1 downto 0) of signed(BW_MAX_PROBABILITY - 1 downto 0);
	type t_node   is array (NUMBER_TRELLIS_STATES - 1 downto 0) of std_logic_vector(BW_MAX_PROBABILITY - 1 downto 0);
	type t_branch is array (NUMBER_BRANCH_UNITS - 1 downto 0)   of std_logic_vector(BW_BRANCH_RESULT - 1 downto 0);

	-- RAM Data
	type t_ram_rd_data is array (1 downto 0) of std_logic_vector(NUMBER_TRELLIS_STATES - 1 downto 0);

end package pkg_types;
