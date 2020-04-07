library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		m0 : out positive;
		m1 : out positive
	);
end bug;

architecture behav of bug is
	constant A : positive := 4;
	constant B : positive := 1100;
	constant C : positive := to_integer(A * to_unsigned(B, 11));
	constant D : positive := to_integer(to_unsigned(B, 11) * A);
begin
   m0 <= c;
   m1 <= d;

end architecture;
