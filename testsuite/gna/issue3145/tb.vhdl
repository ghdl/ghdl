library ieee;

entity testbench is
end entity;

architecture behavior of testbench is
begin
	ace: entity work.ace
		generic map (
			a => 16
		);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.log2;

entity ace is
	generic (
		a: integer -- using positive here will cause value out of range
	);
end entity;

architecture behavior of ace is
	constant x: positive := positive(log2(real(a)));
begin
end;

