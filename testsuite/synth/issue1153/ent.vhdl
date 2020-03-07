library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		a, b : in std_logic;
		q : out std_logic
	);
end;

architecture a of ent is
begin
	q <= a xnor b;
end;
