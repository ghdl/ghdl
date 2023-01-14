------------------------------------------------------------------
----                                                          ----
----  Content: Parameterized quire accumulator unit:          ----
----           quire accumulator for deferred rounding ctrl   ----
----                                                          ----
----  Author:  E. Theodore L. Omtzigt                         ----
----           theo@stillwater-sc.com                         ----
----                                                          ----
------------------------------------------------------------------
----                                                          ----
---- Copyright (C) 2017-2018                                  ----
----               E. Theodore L. Omtzigt                     ----
----               theo@stillwater-sc.com                     ----
----                                                          ----
------------------------------------------------------------------

---- A quire is a super accumulator that receives unrounded
---- results from the adder and/or multiplier and accumulates
---- the unrounded results enabling to user controlled rounding.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;

entity quire_accumulator is
	port(
		clk   : in  std_logic;
		rst_n : in  std_logic;
		q_in  : in  quire_in_type;
		reg   : in  quire_in_reg;       -- output from register file
		add   : in  quire_in_add;       -- output from adder/subtractor
		mul   : in  quire_in_mul;       -- output from multiplier
		q_out : out quire_out_type
	);
end entity;

architecture behavior of quire_accumulator is

	signal r, r_in : quire_word;

begin
	COMBINATORIAL : process(rst_n, q_in)
		variable v     : quire_word       := QUIRE_ZERO;
		variable state : quire_state_type := QS_ZERO;
	begin
		v := r;                         -- default assignment

		if rst_n = '0' then
			v := QUIRE_ZERO;
		else
			if q_in.cmd = LOAD then
				v := q_in.d_in;
			elsif q_in.cmd = CLEAR then
				v := QUIRE_ZERO;
			elsif q_in.cmd = NOP then
				v := r;
			end if;
			
			if v = QUIRE_ZERO then
				state := QS_ZERO;
			else
				state := QS_POS;
			end if;
		end if;

		r_in          <= v;
		q_out.q_out   <= r;
		q_out.state   <= state;

	end process;

	SEQUENTIAL : process(clk)
	begin
		if rising_edge(clk) then
			r <= r_in;
		end if;
	end process;

end architecture;
