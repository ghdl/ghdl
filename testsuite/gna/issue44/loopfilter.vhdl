-- loopfilter.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity loopfilter is
  port(clk, reset: in std_logic;
      mult_error_op:in signed(38 downto 0);
      f_desired: in unsigned(5 downto 0);
      f_word_output: out unsigned(5 downto 0));
end loopfilter;

architecture loopfilter_arch of loopfilter is

  signal beta_e, alpha_e: signed(45 downto 0);
  constant alpha: signed(7 downto 0) := "01110011";
  constant beta: signed(7 downto 0) := "00011010";
  constant plus_one: signed(alpha_e'range) := ('0', others => '1');

  component  q_one_dot_fp_multiplier is
  	generic (a_word_size, b_word_size:integer);
  	port(a: in signed(a_word_size-1 downto 0);
  		b: in signed(b_word_size-1 downto 0);
  		mult_out: out signed(a_word_size + b_word_size -2 downto 0));
  end component;

begin

  M0: q_one_dot_fp_multiplier generic map(a_word_size => beta'length, b_word_size => mult_error_op'length)
                              port map(beta, mult_error_op,beta_e );
  M1: q_one_dot_fp_multiplier generic map(a_word_size => alpha'length, b_word_size => mult_error_op'length)
                              port map(alpha, mult_error_op, alpha_e);

  ADDERS : process(clk, reset)
  variable freq_add_var : signed(beta_e'range);
  variable n_total_var: signed(alpha_e'range);
  variable temp: signed(n_total_var'range);
  begin
    if reset = '1' then
      freq_add_var := (others => '0');
      freq_add_var(42 downto 37) := signed(std_logic_vector(f_desired));
      n_total_var := (others => '0');
    elsif rising_edge(clk) then
        freq_add_var := freq_add_var + beta_e;
        n_total_var := freq_add_var + alpha_e;
    end if;

    if n_total_var(n_total_var'length -1) = '1' then
      temp := plus_one + n_total_var;
    else
      temp := n_total_var;
    end if;

    f_word_output <= unsigned(temp(42 downto 37));

  end process;



end loopfilter_arch;
