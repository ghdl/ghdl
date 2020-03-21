library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug2 is
	generic(
		LEN : positive := 32
	);
	port(
		input_a :  in unsigned(LEN-1 downto 0);
		input_b :  in unsigned(LEN-1 downto 0);
		output  : out unsigned(LEN-1 downto 0)
	);
end bug2;

architecture behav of bug2 is
begin
	output <= maximum(input_a, input_b);
end architecture;
