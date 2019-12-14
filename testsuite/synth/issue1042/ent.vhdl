library ieee;
use ieee.std_logic_1164.all;

entity ent is
	generic (
		VAL : real := 1.5
	);
	port (
		lt  : out std_logic;
		lte : out std_logic;
		eq  : out std_logic;
		gte : out std_logic;
		gt  : out std_logic
	);
end;

architecture a of ent is
begin
	lt  <= '1' when VAL  < 1.5 else '0';
	lte <= '1' when VAL <= 1.5 else '0';
	eq  <= '1' when VAL  = 1.5 else '0';
	gte <= '1' when VAL >= 1.5 else '0';
	gt  <= '1' when VAL  > 1.5 else '0';
end;

