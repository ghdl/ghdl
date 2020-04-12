library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
port (p, q : out std_logic);
end entity;

architecture arch of top is
	type subrecord_r is record
		c : std_logic;
		d : std_logic;
	end record;
	type record_r is record
		s : subrecord_r;
		a : std_logic;
		b : std_logic;
	end record;
	signal s : subrecord_r;
	signal r : record_r;
begin
	s <= ('0', '0');
	r <= (s, '0', '1');
	p <= r.a;
	q <= r.b;
end architecture;
