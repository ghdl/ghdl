library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		o : out std_logic
	);
end;

architecture a of ent is
begin
	gen: if false generate
		o <= '1';
	else generate
		o <= '0';
	end generate;
end;

