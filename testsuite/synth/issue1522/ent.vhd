library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	port (int_out : out integer);
end;

architecture RTL of ent is
	type int2_array is array (0 to 1) of integer;

	type my_record is record
		a : int2_array;
		b : int2_array;
	end record my_record;

	signal c : unsigned(1 downto 0) := (others => '0');

	constant sym : my_record := (a => (0=>1, 1=>2), b => (0=>2, 1=>2));
begin
	int_out <= sym.a(to_integer(c));
	c <= (others => '0');
end;
