--!
--! Copyright (C) 2012 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  AXI4-Stream buffer that allows to buffer the accept-signal.
--! @author Matthias Alles
--! @date   2012/04/18
--!
--! @details
--! One problem when concatenating multiple AXI4-Stream builind blocks is that
--! the accept signal has to pass from the very last component to the input
--! of the very first component. Only then it is possible to have an interruption
--! free data processing within the whole chain. The drawback of this approach is
--! that the accept signal has a long path and high fanouts.
--! This entity allows to use registers on the accept signals by introducing buffers
--! for storing the input values. It should improve timing of bigger building blocks.
--!

library ieee;
use ieee.std_logic_1164.all;


entity axi4s_buffer is
	generic (
		DATA_WIDTH : natural := 1
	);
	port (

	clk            : in  std_logic;
	rst            : in  std_logic;

	-- Input data handling
	----------------------

	input          : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
	input_valid    : in  std_logic;
	input_last     : in  std_logic;
	input_accept   : out std_logic;


	-- Output data handling
	-----------------------
	output         : out std_logic_vector(DATA_WIDTH - 1 downto 0);
	output_valid   : out std_logic;
	output_last    : out std_logic;
	output_accept  : in  std_logic
);
end entity axi4s_buffer;


architecture rtl of axi4s_buffer is


	signal input_accept_int : std_logic;

	signal output_reg        : std_logic_vector(DATA_WIDTH - 1 downto 0);
	signal output_last_reg   : std_logic;
	signal output_valid_reg  : std_logic;

	signal buffer_full : std_logic;
	signal buffer_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
	signal buffer_last : std_logic;

begin

	input_accept <= input_accept_int;

	output       <= output_reg;
	output_last  <= output_last_reg;
	output_valid <= output_valid_reg;

	--
	-- This process registers all signals.
	-- No combinatorial logic is bypassed from input to output and vice versa.
	--
	pr_reg: process(clk) is
	begin
	if rising_edge(clk) then
		if rst = '1' then
			output_reg        <= (others => '0');
			output_last_reg   <= '0';
			output_valid_reg  <= '0';

			input_accept_int <= '1';

			buffer_full <= '0';
			buffer_data <= (others => '0');
			buffer_last <= '0';
		else

			--
			-- Data is coming, buf output data can't be sent => Store input data in buffer
			-- and remove input_accept signal!
			--
			if input_valid = '1' and input_accept_int = '1' and output_valid_reg = '1' and output_accept = '0' then
				buffer_data      <= input;
				buffer_last      <= input_last;
				buffer_full      <= '1';
				input_accept_int <= '0';
			end if;

			--
			-- Output data is being read but there is data in the buffer waiting for being sent
			-- => Use the buffer data!
			--
			if output_accept = '1' and output_valid_reg = '1' and buffer_full = '1' then
				output_reg       <= buffer_data;
				output_last_reg  <= buffer_last;
				output_valid_reg <= '1';
				buffer_full      <= '0';
				input_accept_int <= '1';

			--
			-- Data is being read and buffer is empty => Use input data directly!
			-- Output register is empty => Use input data directly!
			--
			elsif (output_accept = '1' and output_valid_reg = '1') or output_valid_reg = '0' then
				output_reg       <= input;
				output_last_reg  <= input_last;
				output_valid_reg <= input_valid;
			end if;

		end if;
	end if;
	end process pr_reg;

end architecture rtl;
