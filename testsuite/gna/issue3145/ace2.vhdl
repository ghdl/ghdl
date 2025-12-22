library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.log2;

entity ace is
	generic (
		a: integer -- using positive here will cause value out of range
	);
end entity;

architecture behavior of ace is
	constant x: real := log2(real(a));
begin
end;

