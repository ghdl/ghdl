entity ent3 is
end entity;

architecture impl of ent3 is
	type bitvv is array(natural range <>) of bit_vector;
	signal foo: bitvv(2 downto 0)(0 downto 0);
begin

foo <= bitvv'(
	2 downto 1 => foo(1 downto 0),
	0 => foo(2)
);

end architecture;
