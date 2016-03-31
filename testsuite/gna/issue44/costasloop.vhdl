-- costasloop.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity costasloop is
	port(carrier: in signed(11 downto 0);
		clk, reset: in std_logic;
		op: out std_logic);
end costasloop;

architecture costasloop_arch of costasloop is
	component nco is
		port(clk, reset: in std_logic;
		fword: in unsigned(5 downto 0);
		op_sin: out signed(4 downto 0);
		op_cos: out signed(4 downto 0));
	end component;

	component q_one_dot_fp_multiplier is
		generic (a_word_size, b_word_size:integer);
		port(a: in signed(a_word_size-1 downto 0);
			b: in signed(b_word_size-1 downto 0);
			mult_out: out signed(a_word_size + b_word_size -2 downto 0));
		end component;

		component lpf is
			port(clk, reset: in std_logic;
			x_in: in signed(15 downto 0);
				y_out: out signed(19 downto 0));
			end component;

    component loopfilter is
  port(clk, reset: in std_logic;
      mult_error_op:in signed(38 downto 0);
      f_desired: in unsigned(5 downto 0);
      f_word_output: out unsigned(5 downto 0));
    end component;

	signal nco_input: unsigned(5 downto 0);
	signal nco_sin, nco_cos: signed(4 downto 0);
	signal mult_sin, mult_cos: signed(15 downto 0);
	signal raw_op_sin, raw_op_cos: signed(19 downto 0);
	signal mult_error_op: signed(38 downto 0);
begin
	--NCO phase multiplier
	N: nco port map(clk, reset, nco_input, nco_sin, nco_cos);

	--Multiplier
	M0: q_one_dot_fp_multiplier generic map(a_word_size=>nco_sin'length, b_word_size => carrier'length) port map(nco_sin, carrier, mult_sin);
	M1: q_one_dot_fp_multiplier generic map(a_word_size=>nco_sin'length, b_word_size => carrier'length) port map(nco_cos, carrier, mult_cos);

	--FIR Filter
	L0: lpf port map(clk, reset, mult_sin, raw_op_sin);
	L1: lpf port map(clk, reset, mult_cos, raw_op_cos);
	--Extract output (Comparator)
	COMPARATOR: op <= raw_op_sin(raw_op_sin'length -1); --Sign bit
	--Error Multiplier
	EM: q_one_dot_fp_multiplier generic map(a_word_size=>raw_op_sin'length, b_word_size => raw_op_cos'length) port map(raw_op_sin, raw_op_cos, mult_error_op);
	--Loop Filter
	
	--NCO mapping to error
    LF: loopfilter port map(clk, reset, mult_error_op, to_unsigned(16, 6), nco_input);
end costasloop_arch;
