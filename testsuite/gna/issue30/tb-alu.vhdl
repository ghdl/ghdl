library	ieee;
use		ieee.std_logic_1164.all;

package testbench is
constant	zero:				std_logic_vector(3 downto 0) := x"0";
constant	one:	   			std_logic_vector(3 downto 0) := x"1";
constant	two:				std_logic_vector(3 downto 0) := x"2";
constant	three:	   			std_logic_vector(3 downto 0) := x"3";
constant	four:				std_logic_vector(3 downto 0) := x"4";
constant	five:				std_logic_vector(3 downto 0) := x"5";
constant	six:				std_logic_vector(3 downto 0) := x"6";
constant	seven:				std_logic_vector(3 downto 0) := x"7";
constant	eight:				std_logic_vector(3 downto 0) := x"8";
constant	nine:				std_logic_vector(3 downto 0) := x"9";
constant	ten:				std_logic_vector(3 downto 0) := x"a";
constant	eleven:				std_logic_vector(3 downto 0) := x"b";
constant	twelve:				std_logic_vector(3 downto 0) := x"c";
constant	thirteen:			std_logic_vector(3 downto 0) := x"d";
constant	fourteen:			std_logic_vector(3 downto 0) := x"e";
constant	fifteen:			std_logic_vector(3 downto 0) := x"f";

constant	counter_width:		positive	:= 24;

constant	Disable:			std_logic	:= '0';
constant	enable:				std_logic	:= '1';
end;


library	ieee;
use		ieee.std_logic_1164.all;
use		ieee.std_logic_unsigned.all;

library	work;
use		work.testbench.all;
use		work.definitions.all;

entity tb_alu is
end;

architecture struct_tb_alu of tb_alu is
component clkgen is
port(
	clk_out:	out	std_logic;
	resetn:		out	std_logic
);
end component;

component synchronous_latchN is
generic(
	N:	positive
);
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic_vector((N-1) downto 0);
	q:				out	std_logic_vector((N-1) downto 0)
);
end component;

component synchronous_latch_autoclear is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end component;

component counterN is
generic(
	N:	positive
);
port(
	clock:			in	std_logic;
	carry_in:		in	std_logic;
	clock_enable:	in	std_logic;
	resetn:			in	std_logic;
	output:			out	std_logic_vector((N-1) downto 0);
	carry_out:		out	std_logic
);
end component;

component alu is
port(
	-- control
	operation:			in	std_logic_vector(4 downto 0);

	-- operands
	primary_operand:	in	std_logic_vector(7 downto 0);
	secondary_operand:	in	std_logic_vector(7 downto 0);
	flags_in:			in	std_logic_vector(7 downto 0);

	--	results
	output, flags_out:	out	std_logic_vector(7 downto 0);
	secondary_out:		out	std_logic_vector(7 downto 0)
);
end component;

component magnitudeN is
generic(
	N:	positive
);
port(
	a, b:	in	std_logic_vector((N-1) downto 0);
	equal:	out	std_logic;
	lt:		out	std_logic; 	-- '1' if a < b
	gt:		out	std_logic	-- '1' if a > b
);
end component;


signal	clock: 						std_logic;
signal	resetn:						std_logic;

signal	notclock:					std_logic;

signal	next_state:					std_logic_vector(3 downto 0);
signal	nxt_state:					std_logic_vector(3 downto 0);
signal	current_state:				std_logic_vector(3 downto 0);

signal	counter_state:				std_logic;
signal	counter_clock:				std_logic;
signal	counter_clock_enable:		std_logic;
signal	counter_out:				std_logic_vector((counter_width - 1) downto 0);
signal	counter_zero:				std_logic;

signal	zero_secondary_alu_result:	std_logic;

signal	test_bits:					std_logic;
signal	res_bits:					std_logic;
signal	res_result:					std_logic_vector(7 downto 0);

signal	alu_result:					std_logic_vector(7 downto 0);
signal	secondary_alu_result:		std_logic_vector(7 downto 0);
signal	flags_in:					std_logic_vector(7 downto 0);
signal	flags:						std_logic_vector(7 downto 0);

signal	sum_check:					std_logic_vector(8 downto 0);
signal	sum_overflow_check:			std_logic;
signal	sum_zero_check:				std_logic;
signal	half_sum_check:				std_logic_vector(4 downto 0);
signal	sum_checker:				std_logic;

signal	subtract_check:				std_logic_vector(8 downto 0);
signal	subtract_overflow_check:	std_logic;
signal	subtract_zero_check:		std_logic;
signal	half_difference_check:		std_logic_vector(4 downto 0);
signal	subtract_checker:			std_logic;

signal	and_check:					std_logic_vector(7 downto 0);
signal	and_zero_check:				std_logic;
signal	and_parity_check:			std_logic;
signal	and_checker:				std_logic;

signal	xor_check:					std_logic_vector(7 downto 0);
signal	xor_zero_check:				std_logic;
signal	xor_parity_check:			std_logic;
signal	xor_checker:				std_logic;

signal	or_check:					std_logic_vector(7 downto 0);
signal	or_zero_check:				std_logic;
signal	or_parity_check:			std_logic;
signal	or_checker:					std_logic;

signal	rlc_check:					std_logic_vector(7 downto 0);
signal	rlc_zero_check:				std_logic;
signal	rlc_parity_check:			std_logic;
signal	rlc_checker:				std_logic;

signal	rrc_check:					std_logic_vector(7 downto 0);
signal	rrc_zero_check:				std_logic;
signal	rrc_parity_check:			std_logic;
signal	rrc_checker:				std_logic;

signal	rl_check:					std_logic_vector(7 downto 0);
signal	rl_zero_check:				std_logic;
signal	rl_parity_check:			std_logic;
signal	rl_checker:					std_logic;

signal	rr_check:					std_logic_vector(7 downto 0);
signal	rr_zero_check:				std_logic;
signal	rr_parity_check:			std_logic;
signal	rr_checker:					std_logic;

signal	daa_unimp:					std_logic;

signal	cpl_check:					std_logic_vector(7 downto 0);
signal	cpl_checker:				std_logic;

signal	scf_checker:				std_logic;

signal	ccf_flags:					std_logic_vector(7 downto 0);

signal	sla_check:					std_logic_vector(7 downto 0);
signal	sla_zero_check:				std_logic;
signal	sla_parity_check:			std_logic;
signal	sla_checker:				std_logic;

signal	sra_check:					std_logic_vector(7 downto 0);
signal	sra_zero_check:				std_logic;
signal	sra_parity_check:			std_logic;
signal	sra_checker:				std_logic;

signal	sll_check:					std_logic_vector(7 downto 0);
signal	sll_zero_check:				std_logic;
signal	sll_parity_check:			std_logic;
signal	sll_checker:				std_logic;

signal	srl_check:					std_logic_vector(7 downto 0);
signal	srl_zero_check:				std_logic;
signal	srl_parity_check:			std_logic;
signal	srl_checker:				std_logic;

signal	bit_checker:				std_logic;
signal	bit_check:					std_logic_vector(7 downto 0);
signal	bit_zero_checker:			std_logic;
signal	res_checker:				std_logic;
signal	set_checker:				std_logic;

signal	inrc_zero:					std_logic;
signal	inrc_parity:				std_logic;

signal	primary_rld_check:			std_logic_vector(7 downto 0);
signal	secondary_rld_check:		std_logic_vector(7 downto 0);
signal	rld_zero_check:				std_logic;
signal	rld_parity_check:			std_logic;
signal	primary_rld_checker:		std_logic;
signal	secondary_rld_checker:		std_logic;

signal	primary_rrd_check:			std_logic_vector(7 downto 0);
signal	secondary_rrd_check:		std_logic_vector(7 downto 0);
signal	rrd_zero_check:				std_logic;
signal	rrd_parity_check:			std_logic;
signal	primary_rrd_checker:		std_logic;
signal	secondary_rrd_checker:		std_logic;

signal	bmtc_check:					std_logic_vector(7 downto 0);
signal	bmtc_parity_check:			std_logic;
signal	bmtc_checker:				std_logic;

signal	done:						std_logic;

begin
	u1: clkgen port map(
			clk_out => clock,
			resetn => resetn
		);

	notclock <= not clock;

	u2: synchronous_latchN
			generic map(
				N => 4
			)
			port map(
				rstn => resetn,
				clock => notclock,
				clock_enable => '1',
				d => next_state,
				q => nxt_state
			);

	u3: synchronous_latchN
			generic map(
				N => 4
			)
			port map(
				rstn => resetn,
				clock => clock,
				clock_enable => '1',
				d => nxt_state,
				q => current_state
			);

	u4: synchronous_latch_autoclear port map(
			rstn => resetn,
			clock => notclock,
			clock_enable => counter_clock,
			d => counter_state,
			q => counter_clock_enable
		);

	u5: counterN
			generic map(
				N => counter_width
			)
			port map(
				clock => clock,
				carry_in => '1',
				clock_enable => counter_clock_enable,
				resetn => resetn,
				output => counter_out((counter_width - 1) downto 0),
				carry_out => open
			);

	u6: alu port map(
			operation => counter_out(21 downto 17),
			primary_operand => counter_out(7 downto 0),
			secondary_operand => counter_out(15 downto 8),
			flags_in => flags_in,
			output => alu_result,
			flags_out => flags,
			secondary_out => secondary_alu_result
		);

	flags_in <= (	carry_bit => counter_out(16),
					others => '0');

	u7: magnitudeN
			generic map(
				N => counter_width
			)
			port map(
				a => counter_out,
				b => x"000000",
				equal => counter_zero,
				lt => open,
				gt => open
			);

	u8: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => sum_check(7 downto 0),
				equal => sum_checker,
				lt => open,
				gt => open
			);

	u9: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => secondary_alu_result,
				b => x"00",
				equal => zero_secondary_alu_result,
				lt => open,
				gt => open
			);
	u10: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => subtract_check(7 downto 0),
				equal => subtract_checker,
				lt => open,
				gt => open
			);

	u11: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => and_check(7 downto 0),
				equal => and_checker,
				lt => open,
				gt => open
			);

	u12: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => xor_check(7 downto 0),
				equal => xor_checker,
				lt => open,
				gt => open
			);

	u13: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => or_check(7 downto 0),
				equal => or_checker,
				lt => open,
				gt => open
			);

	u14: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => rlc_check(7 downto 0),
				equal => rlc_checker,
				lt => open,
				gt => open
			);

	u15: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => rrc_check(7 downto 0),
				equal => rrc_checker,
				lt => open,
				gt => open
			);

	u16: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => rl_check(7 downto 0),
				equal => rl_checker,
				lt => open,
				gt => open
			);

	u17: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => rr_check(7 downto 0),
				equal => rr_checker,
				lt => open,
				gt => open
			);

	u18: magnitudeN
			generic map(
				N => 17
			)
			port map(
				a => counter_out(16 downto 0),
				b => "00000000000000000",
				equal => daa_unimp,
				lt => open,
				gt => open
			);

	u19: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => cpl_check(7 downto 0),
				equal => cpl_checker,
				lt => open,
				gt => open
			);

	u20: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => counter_out(7 downto 0),
				equal => scf_checker,
				lt => open,
				gt => open
			);

	u21: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => sla_check(7 downto 0),
				equal => sla_checker,
				lt => open,
				gt => open
			);


	u22: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => sra_check(7 downto 0),
				equal => sra_checker,
				lt => open,
				gt => open
			);

	u23: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => sll_check(7 downto 0),
				equal => sll_checker,
				lt => open,
				gt => open
			);

	u24: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => srl_check(7 downto 0),
				equal => srl_checker,
				lt => open,
				gt => open
			);

	u25: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => counter_out(15 downto 8),
				equal => bit_checker,
				lt => open,
				gt => open
			);

	bit_check <= (counter_out(7 downto 0) and counter_out(15 downto 8));

	u26: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => x"00",
				b => bit_check,
				equal => bit_zero_checker,
				lt => open,
				gt => open
			);

	u27: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => res_result,
				equal => res_checker,
				lt => open,
				gt => open
			);

	u28: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => primary_rld_check,
				equal => primary_rld_checker,
				lt => open,
				gt => open
			);

	u29: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => secondary_alu_result,
				b => secondary_rld_check,
				equal => secondary_rld_checker,
				lt => open,
				gt => open
			);

	u30: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => primary_rrd_check,
				equal => primary_rrd_checker,
				lt => open,
				gt => open
			);

	u31: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => secondary_alu_result,
				b => secondary_rrd_check,
				equal => secondary_rrd_checker,
				lt => open,
				gt => open
			);

	u32: magnitudeN
			generic map(
				N => 8
			)
			port map(
				a => alu_result,
				b => bmtc_check,
				equal => bmtc_checker,
				lt => open,
				gt => open
			);

	u33: magnitudeN
			generic map(
				N => 22
			)
			port map(
				a => counter_out(21 downto 0),
				b => (others => '1'), -- x"3fffff",
				equal => done,
				lt => open,
				gt => open
			);

	process(current_state, resetn) begin
		if resetn = '0' then
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

			next_state <= zero;
		else
			case current_state is
				when zero =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= one;

				when one =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					assert counter_zero = '1'
					report "counter initialisation failure"
					severity failure;

					next_state <= two;

				when two =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= three;

				when three =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					case counter_out(21 downto 17) is
						when add_operation | adc_operation =>
							assert sum_checker = '1'
							report "incorrect sum"
							severity failure;

							assert sum_check(8) = flags(carry_bit)
							report "incorrect addition carry flag"
							severity failure;

							assert sum_overflow_check = flags(parity_overflow_bit)
							report "incorrect addition overflow flag"
							severity failure;

							assert sum_zero_check = flags(zero_bit)
							report "incorrect addition zero flag"
							severity failure;

							assert half_sum_check(4) = flags(half_carry_bit)
							report "incorrect interdigit carry flag"
							severity failure;

		                    assert alu_result(7) = flags(sign_bit)
        		            report "incorrect addition sign flag"
                    		severity failure;

							assert flags(add_sub_bit) = '0'
							report "incorrect addition add/subtract flag"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for add/adc"
							severity failure;

                        when sub_operation | sbc_operation =>
                        	assert subtract_checker = '1'
							report "incorrect difference"
							severity failure;

							assert subtract_check(8) = flags(carry_bit)
							report "incorrect subtraction borrow flag"
							severity failure;

							assert subtract_overflow_check = flags(parity_overflow_bit)
							report "incorrect subtraction overflow flag"
							severity failure;

							assert subtract_zero_check = flags(zero_bit)
							report "incorrect subtraction zero flag"
							severity failure;

							assert half_difference_check(4) = flags(half_carry_bit)
							report "incorrect interdigit borrow flag"
							severity failure;

		                    assert alu_result(7) = flags(sign_bit)
		                    report "incorrect subtraction sign flag"
		                    severity failure;

							assert flags(add_sub_bit) = '1'
							report "incorrect subtraction add/subtract flag"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for sub/sbc"
							severity failure;

                        when and_operation =>
							assert and_checker = '1'
							report "incorrect logical AND result"
							severity failure;

							assert and_zero_check = flags(zero_bit)
							report "incorrect logical AND zero flag"
							severity failure;

							assert and_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for logical AND"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for logical AND"
            		        severity failure;

                    		assert flags(half_carry_bit) = '1'
		                    report "incorrect half-carry flag for logical AND"
        		            severity failure;

                		    assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract bit for logical AND"
        		            severity failure;

                		    assert flags(carry_bit) = '0'
		                    report "incorrect carry bit for logical AND"
        		            severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for and"
							severity failure;

						when xor_operation =>
							assert xor_checker = '1'
							report "incorrect logical XOR result"
							severity failure;

							assert xor_zero_check = flags(zero_bit)
							report "incorrect logical XOR zero flag"
							severity failure;

							assert xor_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for logical XOR"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for logical XOR"
        		            severity failure;

                		    assert flags(half_carry_bit) = '1'
		                    report "incorrect half-carry flag for logical XOR"
        		            severity failure;

                		    assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract flag for logical XOR"
		                    severity failure;

        		            assert flags(carry_bit) = '0'
                		    report "incorrect carry bit for logical XOR"
		                    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for xor"
							severity failure;

						when or_operation =>
							assert or_checker = '1'
							report "incorrect logical OR result"
							severity failure;

							assert or_zero_check = flags(zero_bit)
							report "incorrect logical OR zero flag"
							severity failure;

							assert or_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for logical OR"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for logical OR"
        		            severity failure;

		                    assert flags(half_carry_bit) = '1'
		                    report "incorrect half-carry flag for logical OR"
        		            severity failure;

                		    assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract flag for logical OR"
		                    severity failure;

        		            assert flags(carry_bit) = '0'
		                    report "incorrect carry flag for logical OR"
		                    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for OR operation"
							severity failure;

						when cmp_operation =>
							assert subtract_checker = '1'
							report "incorrect compare result"
							severity failure;

							assert subtract_check(8) = flags(carry_bit)
							report "incorrect compare borrow flag"
							severity failure;

							assert subtract_overflow_check = flags(parity_overflow_bit)
							report "incorrect compare overflow flag"
							severity failure;

							assert subtract_zero_check = flags(zero_bit)
							report "incorrect compare zero flag"
							severity failure;

							assert half_difference_check(4) = flags(half_carry_bit)
							report "incorrect compare interdigit borrow flag"
							severity failure;

		                    assert alu_result(7) = flags(sign_bit)
		                    report "incorrect compare sign flag"
		                    severity failure;

							assert flags(add_sub_bit) = '1'
							report "incorrect compare add/subtract flag"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for compare"
							severity failure;

						when rlc_operation =>
							assert rlc_checker = '1'
							report "incorrect rlc result"
							severity failure;

							assert rlc_zero_check = flags(zero_bit)
							report "incorrect rlc zero flag"
							severity failure;

							assert rlc_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for rlc"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for rlc"
                		    severity failure;

		                    assert flags(half_carry_bit) = '0'
        		            report "incorrect half-carry flag for rlc"
                    		severity failure;

		                    assert flags(add_sub_bit) = '0'
        		            report "incorrect add/subtract flag for rlc"
                		    severity failure;

		                    assert flags(carry_bit) = counter_out(7)
        		            report "incorrect carry flag for rlc"
                		    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for rls"
							severity failure;

						when rrc_operation =>
							assert rrc_checker = '1'
							report "incorrect rrc result"
							severity failure;

							assert rrc_zero_check = flags(zero_bit)
							report "incorrect rrc zero bit"
							severity failure;

							assert rrc_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for rrc"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for rrc"
		                    severity failure;

		                    assert flags(half_carry_bit) = '0'
        		            report "incorrect half-carry flag for rrc"
                		    severity failure;

		                    assert flags(add_sub_bit) = '0'
        		            report "incorrect add/subtract flag for rrc"
                		    severity failure;

		                    assert flags(carry_bit) = counter_out(0)
        		            report "incorrect carry flag for rrc"
                		    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for rrc"
							severity failure;

						when rl_operation =>
							assert rl_checker = '1'
							report "incorrect rl result"
							severity failure;

							assert rl_zero_check = flags(zero_bit)
							report "incorrect rl zero bit"
							severity failure;

							assert rl_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for rl"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for rl"
                		    severity failure;

		                    assert flags(half_carry_bit) = '0'
        		            report "incorrect half-carry flag for rl"
                		    severity failure;

		                    assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract flag for rl"
        		            severity failure;

		                    assert flags(carry_bit) = counter_out(7)
		                    report "incorrect carry flag for rl"
        		            severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for rl"
							severity failure;

                		when rr_operation =>
                			assert rr_checker = '1'
							report "incorrect rr result"
							severity failure;

							assert rr_zero_check = flags(zero_bit)
							report "incorrect rr zero bit"
							severity failure;

							assert rr_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for rr"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for rr"
                		    severity failure;

		                    assert flags(half_carry_bit) = '0'
        		            report "incorrect half-carry flag for rr"
                		    severity failure;

		                    assert flags(add_sub_bit) = '0'
        		            report "incorrect add/subtract flag for rr"
                		    severity failure;

		                    assert flags(carry_bit) = counter_out(0)
        		            report "incorrect carry flag for rr"
                		    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for rr"
							severity failure;

						when daa_operation =>
							assert daa_unimp = '0'
							report "DAA is not implemented"
							severity note;

						when cpl_operation =>
							assert cpl_checker = '1'
							report "incorrect cpl result"
							severity failure;

							assert flags(add_sub_bit) = '1'
							report "incorrect cpl add/sub flag"
							severity failure;

							assert flags(half_carry_bit) = '1'
							report "incorrect cpl half-carry flag"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for cpl"
							severity failure;

						when scf_operation =>
							assert scf_checker = '1'
							report "incorrect scf result"
							severity failure;

							assert flags(carry_bit) = '1'
							report "incorrect carry flag for scf"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for scf"
							severity failure;

						when ccf_operation =>
							assert scf_checker = '1'
							report "incorrect ccf result"
							severity failure;

							assert flags(carry_bit) = not (counter_out(16))
							report "incorrect ccf carry flag"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for ccf"
							severity failure;

                		when sla_operation =>
                			assert sla_checker = '1'
							report "incorrect sla result"
							severity failure;

							assert sla_zero_check = flags(zero_bit)
							report "incorrect sla zero flag"
							severity failure;

							assert sla_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for sla"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for sla"
        		            severity failure;

                		    assert flags(half_carry_bit) = '0'
		                    report "incorrect half-carry flag for sla"
        		            severity failure;

                		    assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract flag for sla"
        		            severity failure;

                		    assert flags(carry_bit) = counter_out(7)
		                    report "incorrect carry bit for flag"
        		            severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for sla"
							severity failure;

                        when sra_operation =>
                        	assert sra_checker = '1'
                        	report "incorrect sra result"
							severity failure;

							assert sra_zero_check = flags(zero_bit)
							report "incorrect sra zero flag"
							severity failure;

							assert sra_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for sra"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for sra"
        		            severity failure;

                		    assert flags(half_carry_bit) = '0'
		                    report "incorrect half-carry flag for sra"
        		            severity failure;

                		    assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract flag for sra"
		                    severity failure;

        		            assert flags(carry_bit) = counter_out(0)
		                    report "incorrect carry flag for sra"
        		            severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for sra"
							severity failure;

						when sll_operation =>
							assert sll_checker = '1'
							report "incorrect sll result"
							severity failure;

							assert sll_zero_check = flags(zero_bit)
							report "incorrect sll zero flag"
							severity failure;

							assert sll_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for sll"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for sll"
		                    severity failure;

		                    assert flags(half_carry_bit) = '0'
        		            report "incorrect half-carry flag for sll"
		                    severity failure;

        		            assert flags(add_sub_bit) = '0'
		                    report "incorrect add/subtract flag for sll"
        		            severity failure;

                		    assert flags(carry_bit) = counter_out(7)
		                    report "incorrect carry flag for sll"
		                    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for sll"
							severity failure;

        				when srl_operation =>
        					assert srl_checker = '1'
							report "incorrect srl result"
							severity failure;

							assert srl_zero_check = flags(zero_bit)
							report "incorrect srl zero flag"
							severity failure;

							assert srl_parity_check = flags(parity_overflow_bit)
							report "incorrect parity flag for srl"
							severity failure;

							assert alu_result(7) = flags(sign_bit)
							report "incorrect sign flag for srl"
                		    severity failure;

		                    assert flags(half_carry_bit) = '0'
        		            report "incorrect half-carry flag for srl"
                		    severity failure;

		                    assert flags(add_sub_bit) = '0'
        		            report "incorrect add/subtract flag for srl"
                		    severity failure;

		                    assert flags(carry_bit) = counter_out(0)
        		            report "incorrect carry flag for srl"
                		    severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for srl"
							severity failure;

						when bit_operation =>
							assert bit_checker = '1'
							report "incorrect result for bit operation"
							severity failure;

							if test_bits = '1' then
								if bit_zero_checker = '1' then
									assert flags(zero_bit) = '1'
									report "BIT: zero flag != '1'"
									severity failure;
								elsif bit_zero_checker = '0' then
									assert flags(zero_bit) = '0'
									report "BIT: zero flag != '0'"
									severity failure;
								end if;
							end if;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for bit"
							severity failure;

						when res_operation =>
							if test_bits = '1' then
								assert res_checker = '1'
								report "incorrect result for RES"
								severity failure;
							end if;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for res"
							severity failure;

						when set_operation =>
							if test_bits = '1' then
								assert or_checker = '1'
								report "incorrect result for SET"
								severity failure;
							end if;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for set"
							severity failure;

						when in16_operation =>
							assert scf_checker = '1'
							report "incorrect result for in r,(C)"
							severity failure;

							assert flags(zero_bit) = inrc_zero
							report "incorrect zero flag for in r,(c)"
							severity failure;

							assert flags(parity_overflow_bit) = inrc_parity
							report "incorrect parity flag for in r,(c)"
							severity failure;

							assert flags(half_carry_bit) = '0'
							report "incorrect half-carry flag for in r,(c)"
							severity failure;

							assert flags(add_sub_bit) = '0'
							report "incorrect add/subtract flag for in r,(c)"
							severity failure;

							assert flags(sign_bit) = alu_result(sign_bit)
							report "incorrect sign flag for in r,(c)"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for in r, (c)"
							severity failure;

						when rld_operation =>
							assert primary_rld_checker = '1'
							report "incorrect primary result for rld"
							severity failure;

							assert secondary_rld_checker = '1'
							report "incorrect secondary rld result"
							severity failure;

							assert alu_result(sign_bit) = flags(sign_bit)
							report "incorrect sign flag for rld"
							severity failure;

                            assert rld_zero_check = flags(zero_bit)
                            report "incorrect zero flag for rld"
                            severity failure;

                            assert rld_parity_check = flags(parity_overflow_bit)
                            report "incorrect parity flag for rld"
                            severity failure;

                            assert flags(half_carry_bit) = '0'
                            report "incorrect half-carry flag for rld"
                            severity failure;

                            assert flags(add_sub_bit) = '0'
                            report "incorrect add/subtract flag for rld"
                            severity failure;

						when rrd_operation =>
							assert primary_rrd_checker = '1'
							report "incorrect primary result for rrd"
							severity failure;

							assert secondary_rrd_checker = '1'
							report "incorrect secondary rrd result"
							severity failure;

							assert alu_result(sign_bit) = flags(sign_bit)
							report "incorrect sign flag for rrd"
							severity failure;

                            assert rrd_zero_check = flags(zero_bit)
                            report "incorrect zero flag for rrd"
                            severity failure;

                            assert rrd_parity_check = flags(parity_overflow_bit)
                            report "incorrect parity flag for rrd"
                            severity failure;

                            assert flags(half_carry_bit) = '0'
                            report "incorrect half-carry flag for rrd"
                            severity failure;

                            assert flags(add_sub_bit) = '0'
                            report "incorrect add/subtract flag for rrd"
                            severity failure;

						when blockterm16_operation =>
							assert bmtc_checker = '1'
							report "incorrect bmtc result"
							severity failure;

							assert bmtc_parity_check = flags(parity_overflow_bit)
							report "incorrect bmtc parity bit"
							severity failure;

							assert zero_secondary_alu_result = '1'
							report "secondary_alu_result != 0 for block termination"
							severity failure;

						when others =>
--							assert counter_out(16 downto 0) /= ('0' & x"0000")
--							report "unimplemented alu operation"
--							severity warning;

					end case;

					if done = '1' then
						counter_state <= Disable;
						counter_clock <= Disable;
						next_state <= fifteen;
					else
						counter_clock <= enable;
						counter_state <= enable;
						next_state <= two;
					end if;

				when four =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= five;

				when five =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= six;

				when six =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= seven;

				when seven =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here


					next_state <= eight;

				when eight =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= nine;

				when nine =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= ten;

				when ten =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= eleven;

				when eleven =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= twelve;

				when twelve =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= thirteen;

				when thirteen =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here


					next_state <= fourteen;

				when fourteen =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= fifteen;

				when fifteen =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					assert false
					report "test success"
					severity note;

				when others =>
-- default states begin here
counter_state <= Disable;
counter_clock <= Disable;

test_bits <=	(
				((		counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(		counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(		counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0))) or
				((not	counter_out(7))	and	(not	counter_out(6))	and	(not	counter_out(5))	and	(not	counter_out(4))	and	(not	counter_out(3))	and	(not	counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0)))
				);

res_bits <=		(
				((not	counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(not	counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (not	counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(not	counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(not	counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(not	counter_out(2))	and	(		counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(not	counter_out(1))	and	(		counter_out(0))) or
				((		counter_out(7))	and	(		counter_out(6))	and (		counter_out(5))	and	(		counter_out(4))	and	(		counter_out(3))	and	(		counter_out(2))	and	(		counter_out(1))	and	(not	counter_out(0)))
				);

sum_check <= ('0' & counter_out(15 downto 8)) + ('0' & counter_out(7 downto 0)) + (x"00" & (counter_out(16) and counter_out(17)));
sum_overflow_check <= (not (counter_out(15) xor counter_out(7))) and (counter_out(15) xor sum_check(7));
sum_zero_check <= not (sum_check(7) or sum_check(6) or sum_check(5) or sum_check(4) or sum_check(3) or sum_check(2) or sum_check(1) or sum_check(0));
half_sum_check <= ('0' & counter_out(11 downto 8)) + ('0' & counter_out(3 downto 0)) + (x"0" & (counter_out(16) and counter_out(17)));

subtract_check <= ('0' & counter_out(7 downto 0)) - ('0' & counter_out(15 downto 8)) - (x"00" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));
subtract_overflow_check <= (counter_out(7) xor counter_out(15)) and (counter_out(7) xor subtract_check(7));
subtract_zero_check <= not (subtract_check(7) or subtract_check(6) or subtract_check(5) or subtract_check(4) or subtract_check(3) or subtract_check(2) or subtract_check(1) or subtract_check(0));
half_difference_check <= ('0' & counter_out(3 downto 0)) - ('0' & counter_out(11 downto 8)) - (x"0" & (counter_out(16) and counter_out(17) and counter_out(18) and (not counter_out(19))));

and_check <= counter_out(15 downto 8) and counter_out(7 downto 0);
and_zero_check <= not (and_check(7) or and_check(6) or and_check(5) or and_check(4) or and_check(3) or and_check(2) or and_check(1) or and_check(0));
and_parity_check <= not (and_check(7) xor and_check(6) xor and_check(5) xor and_check(4) xor and_check(3) xor and_check(2) xor and_check(1) xor and_check(0));

xor_check <= counter_out(15 downto 8) xor counter_out(7 downto 0);
xor_zero_check <= not (xor_check(7) or xor_check(6) or xor_check(5) or xor_check(4) or xor_check(3) or xor_check(2) or xor_check(1) or xor_check(0));
xor_parity_check <= not (xor_check(7) xor xor_check(6) xor xor_check(5) xor xor_check(4) xor xor_check(3) xor xor_check(2) xor xor_check(1) xor xor_check(0));

or_check <= counter_out(15 downto 8) or counter_out(7 downto 0);
or_zero_check <= not (or_check(7) or or_check(6) or or_check(5) or or_check(4) or or_check(3) or or_check(2) or or_check(1) or or_check(0));
or_parity_check <= not (or_check(7) xor or_check(6) xor or_check(5) xor or_check(4) xor or_check(3) xor or_check(2) xor or_check(1) xor or_check(0));

rlc_check <= counter_out(6 downto 0) & counter_out(7);
rlc_zero_check <= not (rlc_check(7) or rlc_check(6) or rlc_check(5) or rlc_check(4) or rlc_check(3) or rlc_check(2) or rlc_check(1) or rlc_check(0));
rlc_parity_check <= not (rlc_check(7) xor rlc_check(6) xor rlc_check(5) xor rlc_check(4) xor rlc_check(3) xor rlc_check(2) xor rlc_check(1) xor rlc_check(0));

rrc_check <= counter_out(0) & counter_out(7 downto 1);
rrc_zero_check <= not (rrc_check(7) or rrc_check(6) or rrc_check(5) or rrc_check(4) or rrc_check(3) or rrc_check(2) or rrc_check(1) or rrc_check(0));
rrc_parity_check <= not (rrc_check(7) xor rrc_check(6) xor rrc_check(5) xor rrc_check(4) xor rrc_check(3) xor rrc_check(2) xor rrc_check(1) xor rrc_check(0));

rl_check <= counter_out(6 downto 0) & counter_out(16);
rl_zero_check <= not (rl_check(7) or rl_check(6) or rl_check(5) or rl_check(4) or rl_check(3) or rl_check(2) or rl_check(1) or rl_check(0));
rl_parity_check <= not (rl_check(7) xor rl_check(6) xor rl_check(5) xor rl_check(4) xor rl_check(3) xor rl_check(2) xor rl_check(1) xor rl_check(0));

rr_check <= counter_out(16) & counter_out(7 downto 1);
rr_zero_check <= not (rr_check(7) or rr_check(6) or rr_check(5) or rr_check(4) or rr_check(3) or rr_check(2) or rr_check(1) or rr_check(0));
rr_parity_check <= not (rr_check(7) xor rr_check(6) xor rr_check(5) xor rr_check(4) xor rr_check(3) xor rr_check(2) xor rr_check(1) xor rr_check(0));

cpl_check <= not counter_out(7 downto 0);

sla_check <= counter_out(6 downto 0) & '0';
sla_zero_check <= not (sla_check(7) or sla_check(6) or sla_check(5) or sla_check(4) or sla_check(3) or sla_check(2) or sla_check(1) or sla_check(0));
sla_parity_check <= not (sla_check(7) xor sla_check(6) xor sla_check(5) xor sla_check(4) xor sla_check(3) xor sla_check(2) xor sla_check(1) xor sla_check(0));

sra_check <= counter_out(7) & counter_out(7 downto 1);
sra_zero_check <= not (sra_check(7) or sra_check(6) or sra_check(5) or sra_check(4) or sra_check(3) or sra_check(2) or sra_check(1) or sra_check(0));
sra_parity_check <= not (sra_check(7) xor sra_check(6) xor sra_check(5) xor sra_check(4) xor sra_check(3) xor sra_check(2) xor sra_check(1) xor sra_check(0));

sll_check <= counter_out(6 downto 0) & '0';
sll_zero_check <= not (sll_check(7) or sll_check(6) or sll_check(5) or sll_check(4) or sll_check(3) or sll_check(2) or sll_check(1) or sll_check(0));
sll_parity_check <= not (sll_check(7) xor sll_check(6) xor sll_check(5) xor sll_check(4) xor sll_check(3) xor sll_check(2) xor sll_check(1) xor sll_check(0));

srl_check <= '0' & counter_out(7 downto 1);
srl_zero_check <= not (srl_check(7) or srl_check(6) or srl_check(5) or srl_check(4) or srl_check(3) or srl_check(2) or srl_check(1) or srl_check(0));
srl_parity_check <= not (srl_check(7) xor srl_check(6) xor srl_check(5) xor srl_check(4) xor srl_check(3) xor srl_check(2) xor srl_check(1) xor srl_check(0));

inrc_zero <= not (counter_out(7) or counter_out(6) or counter_out(5) or counter_out(4) or counter_out(3) or counter_out(2) or counter_out(1) or counter_out(0));
inrc_parity <= not (counter_out(7) xor counter_out(6) xor counter_out(5) xor counter_out(4) xor counter_out(3) xor counter_out(2) xor counter_out(1) xor counter_out(0));

primary_rld_check <= counter_out(7 downto 4) & counter_out(15 downto 12);
secondary_rld_check <= counter_out(11 downto 8) & counter_out(3 downto 0);
rld_zero_check <= not (primary_rld_check(7) or primary_rld_check(6) or primary_rld_check(5) or primary_rld_check(4) or primary_rld_check(3) or primary_rld_check(2) or primary_rld_check(1) or primary_rld_check(0));
rld_parity_check <= not (primary_rld_check(7) xor primary_rld_check(6) xor primary_rld_check(5) xor primary_rld_check(4) xor primary_rld_check(3) xor primary_rld_check(2) xor primary_rld_check(1) xor primary_rld_check(0));

primary_rrd_check <= counter_out(7 downto 4) & counter_out(11 downto 8);
secondary_rrd_check <= counter_out(3 downto 0) & counter_out(15 downto 12);
rrd_zero_check <= not (primary_rrd_check(7) or primary_rrd_check(6) or primary_rrd_check(5) or primary_rrd_check(4) or primary_rrd_check(3) or primary_rrd_check(2) or primary_rrd_check(1) or primary_rrd_check(0));
rrd_parity_check <= not (primary_rrd_check(7) xor primary_rrd_check(6) xor primary_rrd_check(5) xor primary_rrd_check(4) xor primary_rrd_check(3) xor primary_rrd_check(2) xor primary_rrd_check(1) xor primary_rrd_check(0));

bmtc_check <= counter_out(7 downto 0) or counter_out(15 downto 8);
bmtc_parity_check <= not (bmtc_check(7) or bmtc_check(6) or bmtc_check(5) or bmtc_check(4) or bmtc_check(3) or bmtc_check(2) or bmtc_check(1) or bmtc_check(0));
-- default states end here

					next_state <= zero;
					assert false
					report "state machine failure"
					severity failure;

			end case;
		end if;
	end process;
end;
