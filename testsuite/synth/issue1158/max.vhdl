library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity max is
	generic(
		A : positive :=  5;
		B : positive :=  7
	);
	port(
		dummy : std_logic
	);
end max;

architecture test of max is
	constant cst : positive := maximum(A,B);
begin
  assert cst = 7 severity error;
end architecture;
