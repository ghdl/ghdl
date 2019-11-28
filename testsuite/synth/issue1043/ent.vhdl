library ieee;
use ieee.std_logic_1164.all;

entity ent is
	generic (
		g : natural := 8
	);
	port (
		o1 : out std_logic;
		o2 : out std_logic
	);
end;

architecture a of ent is
	constant x : real := real(g);

	constant a : natural := g;
	constant y : real := real(a);
begin
	o1 <= '1' when integer(x) = 8 else '0';
	o2 <= '1' when integer(y) = 8 else '0';
end;
