library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	generic(
		LEN : positive := 32
	);
	port(
		input  :  in unsigned(LEN-1 downto 0);
		output : out unsigned(LEN-1 downto 0)
	);
end bug;

architecture behav of bug is
begin
	output <= input and not to_unsigned(4096-1, input'length);
end architecture;
