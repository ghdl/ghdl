library ieee;
use ieee.std_logic_1164.all;

entity ent is
end;

architecture a of ent is
begin
	assert false
		report "Just a note"
		severity note;

	assert false
		report "Test assertion failed"
		severity failure;
end;
