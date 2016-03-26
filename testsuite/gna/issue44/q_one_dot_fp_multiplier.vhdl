-- q_one_dot_fp_multiplier.vhd
--TODO: Better way of handling -1 * -1 case?
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity q_one_dot_fp_multiplier is
	generic (a_word_size, b_word_size:integer);
	port(a: in signed(a_word_size-1 downto 0);
		b: in signed(b_word_size-1 downto 0);
		mult_out: out signed(a_word_size + b_word_size -2 downto 0));
end q_one_dot_fp_multiplier;

architecture mult_arch of q_one_dot_fp_multiplier is
	constant a_minus_1: signed(a'range) := ('1', others=>'0');
	constant b_minus_1: signed(b'range) := ('1', others => '0');
begin
	process(a, b)
	variable output_temp:signed(mult_out'length downto 0);
	begin
		output_temp := a * b;
		if (a = a_minus_1) and (b = b_minus_1) then
			mult_out <= ('0', others =>'1');
		else
				mult_out <= output_temp(mult_out'length-1 downto 0);
		end if;
end process;
end mult_arch;
