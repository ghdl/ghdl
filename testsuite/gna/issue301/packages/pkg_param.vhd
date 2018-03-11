--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief Parameters
--! @author Markus Fehrenz
--! @date 2011/07/01
--!
--! @details This is the configuration file of the Viterbi decoder.
--!          Any changes for parameters should be done here.
--!          Changing parameters somewhere else may result in a malicious
--!          behavior.
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package pkg_param is
	-----------------------------------
	-- Convolutional Code Parameters --
	-----------------------------------


	--
	-- Set the number of parity values
	-- This has to correspond to PARITY_POLYNOMIALS
	--
	constant NUMBER_PARITY_BITS : natural := 2;
	type t_parity is array (NUMBER_PARITY_BITS - 1 downto 0) of natural;

	--
	-- Set parity polynoms in decimal notation
	-- NUMBER_PARITY_BITS has to correspond to the number of elements
	-- Examples: WiFi : [121,91] or [121,91,101]
	--           CDMA : [491,369] or [367,435,369] or [501,441,331,315]
	--           GSM  : [27,19] or [27,21,31]
	--           DAB  : [91,121,101,91]
	--           WiMAX: [91,121,117]
	--
	constant PARITY_POLYNOMIALS   : t_parity := (121,91);


	--
	-- Set a recursive polynomial
	-- Set to 0 if no recursion is used
	-- Setting this arbitrary may result in a worse error correction ability
	--
	constant FEEDBACK_POLYNOMIAL : natural := 0;


	-----------------------------
	-- Architecture Parameters --
	-----------------------------

	--
	-- Set bit width of LLR input
	-- Recommended values: 3 or 4
	--
	constant BW_LLR_INPUT : natural := 4;

	--
	-- Set the maximum window length which shall be allowed at runtime.
	-- Recommended: at least 6 * constraint length
	--
	constant MAX_WINDOW_LENGTH : natural := 96;

	--
	-- Set to 'true' if distributed RAM shall be used
	-- Set to 'false' if block RAM shall be used
	--
	constant DISTRIBUTED_RAM : boolean := true;

end package pkg_param;
