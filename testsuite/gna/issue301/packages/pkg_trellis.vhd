--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Trellis parameter calculations (e.g., transitions, init values).
--! @author Markus Fehrenz
--! @date   2011/07/27
--!
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_param.all;
use dec_viterbi.pkg_param_derived.all;
use dec_viterbi.pkg_types.all;


package pkg_trellis is

	type t_prev_base       is array (1 downto 0) of std_logic_vector(BW_TRELLIS_STATES - 1 downto 0);
	type t_previous_states is array (NUMBER_TRELLIS_STATES - 1 downto 0) of t_prev_base;

	type t_trans_base  is array (1 downto 0) of std_logic_vector(NUMBER_PARITY_BITS - 1 downto 0);
	type t_transitions is array (NUMBER_TRELLIS_STATES - 1 downto 0) of t_trans_base;

	type t_trans_base_signed  is array (1 downto 0) of std_logic_vector(NUMBER_PARITY_BITS downto 0);
	type t_transitions_signed is array (NUMBER_TRELLIS_STATES - 1 downto 0) of t_trans_base_signed;


	--
	-- This function calculates the previous states of each state.
	-- The values are used to connect the ACS units.
	--
	function calc_previous_states return t_previous_states;


	--
	-- This function calculates corresponding transitions to a trellis sate.
	-- The values are used to connect branch units to ACS units.
	--
	function calc_transitions     return t_transitions;


	--
	-- This function calculates the initialization values for trellis metrics.
	-- The values are used as a constant and written to the ACS unit, every time a new block arrives.
	--
	function calc_initialize      return t_node_s;

	constant PREVIOUS_STATES    : t_previous_states;
	constant TRANSITIONS        : t_transitions;
	constant INITIALIZE_TRELLIS : t_node_s;

end package pkg_trellis;


package body pkg_trellis is


	function calc_previous_states return t_previous_states is
		variable v_prev_states       : t_previous_states := (others=>(others=>(others => '0')));
		variable v_state0, v_state1  : std_logic_vector(BW_TRELLIS_STATES - 1 downto 0);
	begin
		for i in NUMBER_TRELLIS_STATES - 1 downto 0 loop
			v_state0 := std_logic_vector(to_unsigned(i,BW_TRELLIS_STATES));
			v_state1 := v_state0(BW_TRELLIS_STATES - 2 downto 0) & '0';
			v_prev_states(i)(0) := v_state1;
			v_state1 := v_state0(BW_TRELLIS_STATES - 2 downto 0) & '1';
			v_prev_states(i)(1) := v_state1;
		end loop;
	return v_prev_states;
	end function calc_previous_states;


	function calc_transitions return t_transitions is
		variable v_transitions     : t_transitions_signed := (others => (others => (others => '0')));
		variable v_transitions_out : t_transitions := (others => (others => (others => '0')));
		variable v_one_transition  : std_logic_vector(NUMBER_PARITY_BITS - 1 downto 0);
		variable v_next_state      : unsigned(ENCODER_MEMORY_DEPTH - 1 downto 0) := (others => '0');
		variable v_state, v_states : unsigned(ENCODER_MEMORY_DEPTH downto 0);
		variable v_bit             : std_logic := '0';
	begin

		--
		-- It is possible to reduce code size at this stage, if feedback is handled differently,
		-- but the complexity will increase.
		--

		for i in NUMBER_TRELLIS_STATES - 1 downto 0 loop

			--
			-- for input : 0
			-- determine correct input with feedback
			--
			v_next_state := to_unsigned(i,ENCODER_MEMORY_DEPTH) and to_unsigned(FEEDBACK_POLYNOMIAL, ENCODER_MEMORY_DEPTH);
			for k in ENCODER_MEMORY_DEPTH - 1 downto 0 loop
				v_bit := v_bit xor v_next_state(k);
			end loop;
			v_state(ENCODER_MEMORY_DEPTH)              := v_bit;
			v_state(ENCODER_MEMORY_DEPTH - 1 downto 0) := to_unsigned(i,ENCODER_MEMORY_DEPTH);
			v_next_state := v_state(ENCODER_MEMORY_DEPTH downto 1);
			v_bit := '0';

			-- determine paritybits
			for j in NUMBER_PARITY_BITS - 1 downto 0 loop
				v_states := v_state and to_unsigned(PARITY_POLYNOMIALS(j), ENCODER_MEMORY_DEPTH + 1);
				for k in ENCODER_MEMORY_DEPTH downto 0 loop
					v_bit := v_bit xor v_states(k);
				end loop;
				v_one_transition(j) := v_bit;
				v_bit := '0';
			end loop;

			-- decide where to save the parity result
			if v_transitions(to_integer(v_next_state))(1)(NUMBER_PARITY_BITS) = '0' then
				 v_transitions(to_integer(v_next_state))(1)(NUMBER_PARITY_BITS) := '1';
				 v_transitions(to_integer(v_next_state))(1)(NUMBER_PARITY_BITS - 1 downto 0) := v_one_transition;
			else
				 v_transitions(to_integer(v_next_state))(0)(NUMBER_PARITY_BITS - 1 downto 0) := v_one_transition;
			end if;

			--
			-- for input: 1
			-- determine correct input with feedback
			--
			v_next_state := to_unsigned(i,ENCODER_MEMORY_DEPTH) and to_unsigned(FEEDBACK_POLYNOMIAL, ENCODER_MEMORY_DEPTH);
			for k in ENCODER_MEMORY_DEPTH - 1 downto 0 loop
				v_bit := v_bit xor v_next_state(k);
			end loop;
			v_state(ENCODER_MEMORY_DEPTH)              := '1' xor v_bit;
			v_state(ENCODER_MEMORY_DEPTH - 1 downto 0) := to_unsigned(i,ENCODER_MEMORY_DEPTH);
			v_next_state := v_state(ENCODER_MEMORY_DEPTH downto 1);
			v_bit := '0';

			-- determine paritybits
			for j in NUMBER_PARITY_BITS - 1 downto 0 loop
				v_states := v_state and to_unsigned(PARITY_POLYNOMIALS(j), ENCODER_MEMORY_DEPTH + 1);
				for k in ENCODER_MEMORY_DEPTH downto 0 loop
					v_bit := v_bit xor v_states(k);
				end loop;
				v_one_transition(j) := v_bit;
				v_bit := '0';
			end loop;

			-- decide where to save parity result
			if v_transitions(to_integer(v_next_state))(1)(NUMBER_PARITY_BITS) = '0' then
				 v_transitions(to_integer(v_next_state))(1)(NUMBER_PARITY_BITS) := '1';
				 v_transitions(to_integer(v_next_state))(1)(NUMBER_PARITY_BITS - 1 downto 0) := v_one_transition;
			else
				 v_transitions(to_integer(v_next_state))(0)(NUMBER_PARITY_BITS - 1 downto 0) := v_one_transition;
			end if;
		end loop;

		-- truncate, the bit, used to decide where to save parity result
		for i in NUMBER_TRELLIS_STATES - 1 downto 0 loop
			v_transitions_out(i)(1) := v_transitions(i)(1)(NUMBER_PARITY_BITS - 1 downto 0);
			v_transitions_out(i)(0) := v_transitions(i)(0)(NUMBER_PARITY_BITS - 1 downto 0);
		end loop;

		return v_transitions_out;
	end function calc_transitions;


	function calc_initialize return t_node_s is
		variable v_initialize : t_node_s;
	begin
		v_initialize(0) := to_signed(0, BW_MAX_PROBABILITY);
		for i in NUMBER_TRELLIS_STATES - 1 downto 1 loop
			v_initialize(i) := to_signed(- 2 ** (BW_MAX_PROBABILITY - 2), BW_MAX_PROBABILITY);
		end loop;
	return v_initialize;
	end function calc_initialize;


	constant PREVIOUS_STATES    : t_previous_states := calc_previous_states;
	constant TRANSITIONS        : t_transitions     := calc_transitions;
	constant INITIALIZE_TRELLIS : t_node_s          := calc_initialize;

end package body pkg_trellis;
