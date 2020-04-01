library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		dummy : out positive
	);
end bug;

architecture behav of bug is
	constant A : positive := 4;
	constant B : positive := 1100;
	constant C : positive := to_integer(A * to_unsigned(B, 11));
begin
   dummy <= c;

end architecture;
