library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		dummy : in std_ulogic
	);
end bug;

architecture behav of bug is
	constant VALUES_END   : positive := 1;
	constant ARRAY_LENGTH : positive := 16;
	
	subtype value_t is integer range 0 to VALUES_END-1;
	type array_t is array (0 to ARRAY_LENGTH-1) of value_t;

	signal idx   : integer range 0 to ARRAY_LENGTH-1;
	signal value : value_t;
	
	signal array_v : array_t;
begin
	array_v(idx) <= value;
end architecture;
